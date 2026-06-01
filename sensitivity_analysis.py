#!/usr/bin/env python3
"""
Systematische Sensitivitätsanalyse für ALM-Simulation
======================================================

Dieses Skript variiert systematisch:
  A) Rentnerbestand-Parameter (Demographie)
     - Durchschnittsalter, Bestandesgrösse, Rentenhöhe
     - Anteil Verheiratete, Ehegattenrente, Altersdifferenz
  B) Asset-Allokation & Duration-Strategie
     - Gewichtung GovBonds/CorpBonds/Equities/RealEstate/Alternatives
     - Duration-Modi (fixed, fixed_reset, liability_matching, cashflow_matching)
     - Sammelstiftung-Modus

Die Ergebnisse werden als CSV exportiert (eine Datei pro Szenario, plus
eine Übersichts-CSV mit Risikometriken aller Szenarien) und dienen als
Datengrundlage für eine systematische Sensitivitätsanalyse in RStudio.

Verwendung:
    python sensitivity_analysis.py --n_paths 200 --t_horizon 40 --output_dir data/sensitivity
    python sensitivity_analysis.py --n_paths 500 --dry_run          # Nur Szenarien anzeigen
  python sensitivity_analysis.py --n_paths 200 --lhs_samples 200  # Kompakter Lauf (200 Pop-Varianten)
  python sensitivity_analysis.py --n_paths 200 --lhs_samples 1000 # Grosse Abdeckung (1000 Varianten)
    python sensitivity_analysis.py --n_paths 300 --resume 42        # Ab Szenario 42 fortsetzen
    python sensitivity_analysis.py --n_paths 200 --max_disk_gb 50   # Max. 50 GB Speicher
    python sensitivity_analysis.py --config scenarios.json          # Eigene Szenario-Datei
"""

import numpy as np
import pandas as pd
import multiprocessing as mp
import time
import os
import sys
import shutil
import argparse
import json
import signal
import itertools
from datetime import datetime
from pathlib import Path

# ==============================================================================
# KONFIGURATION: PARAMETER-GRIDS
# ==============================================================================

# --- A) RENTNERBESTAND (Demographie) ---
POPULATION_GRID = {
    'n_population':       [50, 100, 200, 500, 1000],          # Bestandesgrösse
    'age_mean':           [67, 70, 72, 74, 76, 78, 80],             # Durchschnittsalter
    'pension_mean':       [25000, 35000, 55000],    # Durchschnittliche Jahresrente (CHF)
    'share_married':      [0.30, 0.50, 0.6, 0.70],      # Anteil Verheiratete
    'spouse_pension_rate': [0.40, 0.5, 0.60],            # Ehegattenrente in % der Rente
    'spouse_age_diff':    [-5, -3, -1, 0],                     # Altersdifferenz (neg = Partner jünger)
    'share_female':       [0.6, 0.55, 0.5, 0.45, 0.4],                   # Anteil weiblich
}

# --- B) ASSET ALLOCATION & STRATEGIE ---
# Jedes Tuple: (GovBonds, CorpBonds, Equities, RealEstate, Alternatives)
# Summe muss 1.0 ergeben, InterestRate-Gewicht ist immer 0
ALLOCATION_GRID = [

    # Mit Alternatives / Infrastructure Debt

    (0.35, 0.20, 0.05, 0.15, 0.25),


]

# --- C) DURATION-STRATEGIEN ---
DURATION_GRID = [
    # (gov_mode, corp_mode, alt_mode, gov_init_dur, corp_init_dur, alt_init_dur)
    ('fixed',              'fixed',             'fixed',             20.0, 10.0, 30.0),
]

# --- D) SAMMELSTIFTUNG ---
SAMMELSTIFTUNG_GRID = [
    (False, 5),
]


# ==============================================================================
# HILFSFUNKTIONEN
# ==============================================================================

def generate_population_df(params, seed=None):
    """
    Erzeugt einen synthetischen Rentnerbestand als DataFrame.

    Args:
        params: Dict mit Schlüsseln:
            n_population, age_mean, pension_mean, share_married,
            spouse_pension_rate, spouse_age_diff, share_female
        seed: Random Seed für Reproduzierbarkeit

    Returns:
        pd.DataFrame im Format von rentnerbestand_initial.csv
    """
    rng = np.random.RandomState(seed)

    n = params['n_population']
    age_mean = params['age_mean']
    pension_mean = params['pension_mean']
    share_married = params['share_married']
    spouse_pension_rate = params['spouse_pension_rate']
    spouse_age_diff_mean = params['spouse_age_diff']
    share_female = params['share_female']

    # 1. Alter
    age_std = 8
    ages = np.round(np.clip(
        rng.normal(age_mean, age_std, n), 65, 100
    )).astype(int)

    # 2. Geschlecht
    genders = rng.choice(['F', 'M'], size=n, p=[share_female, 1 - share_female])

    # 3. Zivilstand
    # Verteilung: share_married verheiratet, 10% verwitwet, Rest ledig
    share_widowed = 0.10
    share_single = max(0.0, 1.0 - share_married - share_widowed)
    marital_statuses = rng.choice(
        ['Married', 'Single', 'Widowed'],
        size=n,
        p=[share_married, share_single, share_widowed]
    )

    # 4. Rentenhöhe
    pension_std = pension_mean * 0.35  # ca. 35% CV
    pensions = np.round(np.clip(
        rng.normal(pension_mean, pension_std, n),
        pension_mean * 0.25,  # Min: 25% des Mittelwerts
        pension_mean * 3.0    # Max: 300% des Mittelwerts
    ), -2)

    # 5. Ehegatten-Altersdifferenz
    spouse_age_diff_std = 2
    spouse_age_diffs = np.round(np.where(
        genders == 'M',
        rng.normal(spouse_age_diff_mean, spouse_age_diff_std, n),
        rng.normal(-spouse_age_diff_mean, spouse_age_diff_std, n)
    )).astype(int)

    # 6. Geburtsjahr
    current_year = 2024
    birth_years = current_year - ages

    return pd.DataFrame({
        'ID': np.arange(1, n + 1),
        'Age': ages,
        'Gender': genders,
        'MaritalStatus': marital_statuses,
        'InitialPension': pensions,
        'BirthYear': birth_years,
        'SpouseAgeDiff': spouse_age_diffs,
        'SpousePensionRate': spouse_pension_rate,
    })


def check_disk_space(output_dir, min_free_gb=2.0, max_used_gb=None):
    """
    Prüft den verfügbaren Speicherplatz und die bisherige Datenmenge.

    Returns:
        (ok, free_gb, used_gb) - ok=False wenn Abbruch nötig
    """
    try:
        stat = shutil.disk_usage(output_dir)
        free_gb = stat.free / (1024 ** 3)
    except OSError:
        # Falls Verzeichnis noch nicht existiert, prüfe Parent
        stat = shutil.disk_usage(os.path.dirname(output_dir) or '/')
        free_gb = stat.free / (1024 ** 3)

    # Bisherige Datenmenge im Output-Verzeichnis
    used_bytes = 0
    if os.path.isdir(output_dir):
        for f in os.scandir(output_dir):
            if f.is_file():
                used_bytes += f.stat().st_size
    used_gb = used_bytes / (1024 ** 3)

    ok = True
    if free_gb < min_free_gb:
        print(f"\n  WARNUNG: Nur noch {free_gb:.1f} GB frei auf dem Datenträger!")
        ok = False
    if max_used_gb is not None and used_gb > max_used_gb:
        print(f"\n  WARNUNG: Output-Verzeichnis bereits {used_gb:.2f} GB gross (Limit: {max_used_gb} GB)")
        ok = False

    return ok, free_gb, used_gb


def _lhs_sample(param_grid, n_samples, seed=0):
    """
    Latin Hypercube Sampling über diskrete Parameter-Grids.

    Jede Dimension wird in n_samples gleichgrosse Schichten (Strata) eingeteilt.
    Pro Stratum wird genau ein Wert gezogen, danach werden die Dimensionen
    unabhängig voneinander permutiert – das garantiert, dass jede Schicht
    pro Dimension genau einmal vertreten ist (maximale Spread-Eigenschaft).

    Args:
        param_grid:  Dict {param_name: [wert1, wert2, ...]}
        n_samples:   Anzahl LHS-Stichproben
        seed:        Random Seed für Reproduzierbarkeit

    Returns:
        Liste von Dicts mit einer Parameterkombination pro Eintrag
    """
    rng = np.random.RandomState(seed)
    param_names = list(param_grid.keys())
    param_values = [param_grid[k] for k in param_names]
    n_dims = len(param_names)

    # Pro Dimension: n_samples Strata, je ein gleichmässig verteilter Wert
    # aus [i/n_samples, (i+1)/n_samples], dann auf diskrete Grid-Werte mappen
    unit_samples = np.zeros((n_samples, n_dims))
    for d in range(n_dims):
        # Stratum-Mittelpunkte + kleines Rauschen innerhalb des Stratums
        strata = (np.arange(n_samples) + rng.uniform(0, 1, n_samples)) / n_samples
        rng.shuffle(strata)                    # unabhaengige Permutation pro Dimension
        unit_samples[:, d] = strata

    # Einheitswerte [0,1] auf diskrete Grid-Werte mappen
    samples = []
    for i in range(n_samples):
        combo = {}
        for d, name in enumerate(param_names):
            vals = param_values[d]
            # Index: u in [0,1] -> Index in [0, len(vals)-1]
            idx = int(unit_samples[i, d] * len(vals))
            idx = min(idx, len(vals) - 1)      # Randfall absichern
            combo[name] = vals[idx]
        samples.append(combo)

    return samples


def build_scenario_list(pop_grid, alloc_grid, duration_grid, sammelstiftung_grid,
                        lhs_samples=500, lhs_seed=0):
    """
    Erzeugt die Szenario-Liste via Latin Hypercube Sampling (LHS).

    Statt OAT (One-At-a-Time, 25 Varianten) oder dem vollen kartesischen
    Produkt (5x7x3x4x3x4x5 = 25'200 Varianten) deckt LHS den gesamten
    Parameterraum mit konfigurierbarer Stichprobenzahl gleichmaessig ab:

      - Der 7-dimensionale Populationsraum wird in n=lhs_samples Punkte
        aufgeteilt, sodass jede Dimension (Groesse, Alter, Rente, ...)
        gleichmaessig repraesentiert ist.
      - Korrelationen zwischen Parametern sind nicht erzwungen – jede
        Dimension wird unabhaengig permutiert (maximale Diversitaet).
      - Zusaetzlich wird immer das Basis-Szenario (Referenz) eingefuegt,
        damit ein bekannter Ankerpunkt vorhanden ist.
      - Jede Pop-Variante wird mit allen Strategie-Kombinationen
        (Allokation x Duration x Sammelstiftung) gekreuzt.

    Parameterraum-Abdeckung gegenueber OAT:
      OAT:  25 Varianten, testet Parameter nur isoliert
      LHS:  lhs_samples Varianten (Default 500), erfasst Interaktionen

    Args:
        pop_grid:            Dict mit diskreten Wertemengen pro Parameter
        alloc_grid:          Liste von Allokations-Tuples
        duration_grid:       Liste von Duration-Tuples
        sammelstiftung_grid: Liste von Sammelstiftung-Tuples
        lhs_samples:         Anzahl LHS-Stichproben (Default: 500)
        lhs_seed:            Random Seed fuer Reproduzierbarkeit (Default: 0)

    Returns:
        Liste von Szenario-Dicts
    """
    scenarios = []

    # ---- Basis-Populationsparameter (Referenz-Szenario, immer enthalten) ----
    base_pop = {
        'n_population':        100,
        'age_mean':            72,
        'pension_mean':        35000,
        'share_married':       0.50,
        'spouse_pension_rate':  0.40,
        'spouse_age_diff':     -3,
        'share_female':        0.55,
    }

    # ---- LHS-Sampling des Populations-Parameterraums ----------------------
    lhs_variants = _lhs_sample(pop_grid, n_samples=lhs_samples, seed=lhs_seed)

    # Basis-Szenario vorne einfuegen (als Referenz-Ankerpunkt)
    all_variants = [base_pop.copy()] + lhs_variants

    # Deduplizierung (LHS kann gelegentlich denselben Gitterpunkt zweimal treffen)
    unique_pops = []
    seen = set()
    for p in all_variants:
        key = tuple(sorted(p.items()))
        if key not in seen:
            seen.add(key)
            unique_pops.append(p)

    # ---- Kombination: Population x Allokation x Duration x Sammelstiftung ----
    strategy_combos = list(itertools.product(alloc_grid, duration_grid, sammelstiftung_grid))

    scenario_id = 0
    for pop_params in unique_pops:
        for alloc, duration, sammelstiftung in strategy_combos:
            scenario_id += 1
            w_gov, w_corp, w_eq, w_re, w_alt = alloc
            gov_mode, corp_mode, alt_mode, gov_dur, corp_dur, alt_dur = duration
            ss_enabled, ss_interval = sammelstiftung

            scenarios.append({
                'scenario_id': scenario_id,
                # Population
                **{f'pop_{k}': v for k, v in pop_params.items()},
                # Asset Allocation
                'w_gov_bonds': w_gov,
                'w_corp_bonds': w_corp,
                'w_equities': w_eq,
                'w_realestate': w_re,
                'w_alternatives': w_alt,
                # Duration
                'gov_bond_duration_mode': gov_mode,
                'corp_bond_duration_mode': corp_mode,
                'alt_bond_duration_mode': alt_mode,
                'initial_gov_bond_duration': gov_dur,
                'initial_corp_bond_duration': corp_dur,
                'initial_alt_bond_duration': alt_dur,
                # Sammelstiftung
                'sammelstiftung_enabled': ss_enabled,
                'sammelstiftung_interval': ss_interval,
            })

    return scenarios


def apply_scenario_to_config(cfg, scenario):
    """
    Überschreibt die relevanten cfg-Attribute für ein Szenario.
    """
    # Asset Allocation
    cfg.WEIGHTS = [
        0.0,
        scenario['w_gov_bonds'],
        scenario['w_corp_bonds'],
        scenario['w_equities'],
        scenario['w_realestate'],
        scenario['w_alternatives'],
    ]

    # Duration-Strategie
    cfg.GOV_BOND_DURATION_MODE = scenario['gov_bond_duration_mode']
    cfg.CORP_BOND_DURATION_MODE = scenario['corp_bond_duration_mode']
    cfg.ALT_BOND_DURATION_MODE = scenario['alt_bond_duration_mode']
    cfg.INITIAL_GOV_BOND_DURATION = scenario['initial_gov_bond_duration']
    cfg.INITIAL_CORP_BOND_DURATION = scenario['initial_corp_bond_duration']
    cfg.INITIAL_ALT_BOND_DURATION = scenario['initial_alt_bond_duration']
    cfg.INITIAL_BOND_DURATION = scenario['initial_gov_bond_duration']

    # Sammelstiftung
    cfg.SAMMELSTIFTUNG_ENABLED = scenario['sammelstiftung_enabled']
    cfg.SAMMELSTIFTUNG_INTERVAL = scenario['sammelstiftung_interval']


def run_single_scenario(scenario, cfg, sim_module, n_paths, t_horizon, output_dir,
                        survival_table_df, population_df, qx_arrays, num_cores,
                        export_all_paths=True):
    """
    Führt ein einzelnes Szenario durch: Config setzen, simulieren, exportieren.

    Args:
        scenario:           Szenario-Dict
        cfg:                config_alm Modul-Referenz
        sim_module:         main_alm_simulation Modul-Referenz
        n_paths:            Anzahl MC-Pfade
        t_horizon:          Simulationshorizont
        output_dir:         Ausgabeverzeichnis
        survival_table_df:  Sterbetafel
        population_df:      Rentnerbestand-DataFrame
        qx_arrays:          Vorberechnete Mortality-Arrays
        num_cores:          Anzahl paralleler Kerne
        export_all_paths:   Ob alle Pfade als CSV exportiert werden

    Returns:
        Dict mit Risikometriken und Metadaten
    """
    sid = scenario['scenario_id']

    # 1. Config anpassen
    cfg.N_PATHS = n_paths
    cfg.T_HORIZON = t_horizon
    apply_scenario_to_config(cfg, scenario)

    # 2. Population vorbereiten (load_data erwartet CSV, wir übergeben direkt den DF)
    pop = population_df.copy()
    # Sicherstellen, dass die nötigen Spalten vorhanden sind
    if 'SpouseAgeDiff' in pop.columns:
        default_diff = np.where(pop['Gender'] == 'M', -3, 3)
        pop['SpouseAgeDiff'] = pop['SpouseAgeDiff'].fillna(
            pd.Series(default_diff, index=pop.index)
        )
    else:
        pop['SpouseAgeDiff'] = np.where(pop['Gender'] == 'M', -3, 3)

    if 'SpousePensionRate' in pop.columns:
        pop['SpousePensionRate'] = pop['SpousePensionRate'].fillna(0.40)
    else:
        pop['SpousePensionRate'] = 0.40

    pop['SpouseInitialAge'] = pop['Age'] + pop['SpouseAgeDiff']
    pop['Status'] = 'Active'
    pop['YearOfDeath'] = 0
    pop['CurrentPension'] = pop['InitialPension']
    pop['Age'] = pop['Age'].astype(int)

    # 3. W0 und V0 berechnen
    W0 = sim_module.calculate_liability_barwert_initial(pop, survival_table_df)
    V0 = W0 * (1 + cfg.GENERAL_RESERVE_RATE)
    pop_stats = sim_module.compute_initial_population_stats(pop, W0)

    # 4. Parallele MC-Simulation
    map_arguments = [
        (pop, survival_table_df, t_horizon, V0, qx_arrays, path_idx)
        for path_idx in range(n_paths)
    ]

    pool = mp.Pool(processes=num_cores)
    try:
        full_results = pool.map(sim_module.run_monte_carlo_path_full, map_arguments)
    finally:
        pool.close()
        pool.join()

    # 5. Risikometriken berechnen
    risk_metrics = sim_module.calculate_risk_metrics(full_results, dg_threshold=0.80)

    # 6. CSV-Export aller Pfade (inkl. Szenario-ID und Population-Stats)
    csv_path = None
    if export_all_paths:
        csv_path = export_scenario_csv(
            full_results, t_horizon, sid, scenario, pop_stats, output_dir, cfg
        )

    # 7. Zusammenfassung
    summary = sim_module.analyze_results(full_results, t_horizon)
    end_dg_mean = float(summary['Mean_DG'].iloc[-1])
    end_dg_p5 = float(summary['P5_DG'].iloc[-1])

    result = {
        'scenario_id': sid,
        **{k: v for k, v in scenario.items() if k != 'scenario_id'},
        # Risiko
        'prob_underfunding': risk_metrics['prob_underfunding'],
        'prob_default': risk_metrics['prob_default'],
        'n_paths_underfunding': risk_metrics['n_paths_underfunding'],
        'n_paths_default': risk_metrics['n_paths_default'],
        # End-Deckungsgrad
        'end_dg_mean': end_dg_mean,
        'end_dg_p5': end_dg_p5,
        # Finanzkennzahlen
        'W0': W0,
        'V0': V0,
        # Population-Stats
        **{f'bestand_{k}': v for k, v in pop_stats.items()},
        # Metadaten
        'n_paths': n_paths,
        't_horizon': t_horizon,
        'csv_file': os.path.basename(csv_path) if csv_path else '',
    }

    return result


def export_scenario_csv(full_results, T_horizon, scenario_id, scenario,
                        population_stats, output_dir, cfg):
    """
    Exportiert alle MC-Pfade eines Szenarios als CSV.
    Format analog zu export_all_paths_csv aus main_alm_simulation.py,
    erweitert um scenario_id und Szenario-Parameter.
    """
    os.makedirs(output_dir, exist_ok=True)

    # Szenario-Metadaten als konstante Spalten
    config_params = {
        'scenario_id': scenario_id,
        # Asset Allocation
        'weight_gov_bonds': cfg.WEIGHTS[1],
        'weight_corp_bonds': cfg.WEIGHTS[2],
        'weight_equities': cfg.WEIGHTS[3],
        'weight_real_estate': cfg.WEIGHTS[4],
        'weight_alternatives': cfg.WEIGHTS[5],
        # Duration
        'gov_bond_duration_mode': cfg.GOV_BOND_DURATION_MODE,
        'corp_bond_duration_mode': cfg.CORP_BOND_DURATION_MODE,
        'alt_bond_duration_mode': cfg.ALT_BOND_DURATION_MODE,
        'initial_gov_bond_duration': cfg.INITIAL_GOV_BOND_DURATION,
        'initial_corp_bond_duration': cfg.INITIAL_CORP_BOND_DURATION,
        'initial_alt_bond_duration': cfg.INITIAL_ALT_BOND_DURATION,
        # Sammelstiftung
        'sammelstiftung_enabled': cfg.SAMMELSTIFTUNG_ENABLED,
        'sammelstiftung_interval': cfg.SAMMELSTIFTUNG_INTERVAL,
        # Kapitalmarkt (konstant über Szenarien, aber für RStudio nützlich)
        'mu_interest_rate': cfg.MU[0],
        'mu_equities': cfg.MU[3],
        'sigma_equities': cfg.SIGMA[3],
        'yield_curve_slope': cfg.YIELD_CURVE_SLOPE,
        'technical_rate_duration': cfg.TECHNICAL_RATE_DURATION,
        'liability_discount_spread': cfg.LIABILITY_DISCOUNT_SPREAD,
        'technical_rate_floor': cfg.TECHNICAL_RATE_FLOOR,
        'general_reserve_rate': cfg.GENERAL_RESERVE_RATE,
        # Simulation
        'n_paths': cfg.N_PATHS,
        't_horizon': cfg.T_HORIZON,
    }

    # Population-Stats hinzufügen
    if population_stats:
        for key, val in population_stats.items():
            config_params[f'bestand_{key}'] = val

    # Szenario-spezifische Population-Parameter
    for k, v in scenario.items():
        if k.startswith('pop_'):
            config_params[k] = v

    rows = []
    for path_idx, res in enumerate(full_results):
        for t in range(T_horizon):
            row = {
                'path_nr': path_idx + 1,
                'year': t + 1,
                'deckungsgrad': res['deckungsgrad'][t],
                'V_t': res['V_t'][t],
                'W_t': res['W_t'][t],
                'portfolio_return': res['portfolio_return'][t],
                'gov_bonds_return': res['gov_bonds_return'][t],
                'corp_bonds_return': res['corp_bonds_return'][t],
                'equities_return': res['equities_return'][t],
                'realestate_return': res['realestate_return'][t],
                'alternatives_return': res['alternatives_return'][t],
                'gov_bonds_coupon': res['gov_bonds_coupon'][t],
                'gov_bonds_duration_effect': res['gov_bonds_duration_effect'][t],
                'corp_bonds_coupon': res['corp_bonds_coupon'][t],
                'corp_bonds_duration_effect': res['corp_bonds_duration_effect'][t],
                'corp_bonds_default_loss': res['corp_bonds_default_loss'][t],
                'alt_bonds_coupon': res['alt_bonds_coupon'][t],
                'alt_bonds_duration_effect': res['alt_bonds_duration_effect'][t],
                'alt_bonds_default_loss': res['alt_bonds_default_loss'][t],
                'r1_t': res['r1_t'][t],
                'i_gov_bonds_t': res['i_gov_bonds_t'][t],
                'i_corp_bonds_t': res['i_corp_bonds_t'][t],
                'i_alt_bonds_t': res['i_alt_bonds_t'][t],
                'i_tech_t': res['i_tech_t'][t],
                'cashflow_rent': res['cashflow_rent'][t],
                'cashflow_admin_fee': res['cashflow_admin_fee'][t],
                'cashflow_total': res['cashflow_total'][t],
                'special_pension_payout': res['special_pension_payout'][t],
                'liability_duration': res['liability_duration'][t],
                'gov_bond_duration': res['gov_bond_duration'][t],
                'corp_bond_duration': res['corp_bond_duration'][t],
                'alt_bond_duration': res['alt_bond_duration'][t],
                'num_pensioners': res['num_pensioners'][t],
                'num_widows': res['num_widows'][t],
                'interest_rate_cap_payout': res['interest_rate_cap_payout'][t],
                'interest_rate_cap_active': res['interest_rate_cap_active'][t],
            }
            row.update(config_params)
            rows.append(row)

    df = pd.DataFrame(rows)
    filename = f"sensitivity_scenario_{scenario_id:04d}.csv"
    filepath = os.path.join(output_dir, filename)
    df.to_csv(filepath, index=False, sep=';', decimal='.')

    return filepath


def format_duration(seconds):
    """Formatiert Sekunden in lesbares Format."""
    if seconds < 60:
        return f"{seconds:.0f}s"
    elif seconds < 3600:
        return f"{seconds/60:.1f}min"
    else:
        h = int(seconds // 3600)
        m = int((seconds % 3600) // 60)
        return f"{h}h {m}min"


def load_scenarios_from_file(filepath):
    """Lädt eine benutzerdefinierte Szenario-Datei (JSON)."""
    with open(filepath, 'r') as f:
        data = json.load(f)

    if 'scenarios' in data:
        return data['scenarios']
    elif isinstance(data, list):
        return data
    else:
        raise ValueError(f"Ungültiges Szenario-Format in {filepath}")


# ==============================================================================
# GRACEFUL SHUTDOWN
# ==============================================================================

_shutdown_requested = False

def _signal_handler(signum, frame):
    global _shutdown_requested
    if _shutdown_requested:
        print("\n\nZweites Signal empfangen - sofortiger Abbruch.")
        sys.exit(1)
    _shutdown_requested = True
    print("\n\nAbbruch angefordert (Ctrl+C). Aktuelles Szenario wird noch abgeschlossen...")
    print("(Erneut Ctrl+C für sofortigen Abbruch)")


# ==============================================================================
# HAUPTPROGRAMM
# ==============================================================================

def main():
    global _shutdown_requested

    parser = argparse.ArgumentParser(
        description='Systematische ALM-Sensitivitätsanalyse',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Beispiele:
  python sensitivity_analysis.py --n_paths 200
  python sensitivity_analysis.py --n_paths 500 --t_horizon 30 --output_dir data/sens_v2
  python sensitivity_analysis.py --dry_run                      # Szenarien anzeigen
  python sensitivity_analysis.py --resume 42                    # Ab Szenario 42 fortsetzen
  python sensitivity_analysis.py --max_disk_gb 50               # Speicherlimit 50 GB
  python sensitivity_analysis.py --no_path_export               # Nur Summary, keine Pfad-CSVs
  python sensitivity_analysis.py --config my_scenarios.json     # Eigene Szenarien
        """
    )
    parser.add_argument('--n_paths', type=int, default=200,
                        help='Anzahl Monte-Carlo-Pfade pro Szenario (Default: 200)')
    parser.add_argument('--t_horizon', type=int, default=40,
                        help='Simulationshorizont in Jahren (Default: 40)')
    parser.add_argument('--output_dir', type=str, default='data/sensitivity',
                        help='Ausgabeverzeichnis (Default: data/sensitivity)')
    parser.add_argument('--num_cores', type=int, default=None,
                        help='Anzahl CPU-Kerne (Default: alle - 1)')
    parser.add_argument('--dry_run', action='store_true',
                        help='Nur Szenarien anzeigen, nicht simulieren')
    parser.add_argument('--resume', type=int, default=None,
                        help='Ab diesem Szenario-Index fortsetzen (1-basiert)')
    parser.add_argument('--max_disk_gb', type=float, default=None,
                        help='Maximale Datenmenge im Output-Verzeichnis (GB)')
    parser.add_argument('--min_free_gb', type=float, default=2.0,
                        help='Minimaler freier Speicherplatz (GB, Default: 2.0)')
    parser.add_argument('--disk_check_interval', type=int, default=5,
                        help='Speicherplatz-Prüfung alle N Szenarien (Default: 5)')
    parser.add_argument('--no_path_export', action='store_true',
                        help='Keine Pfad-CSVs exportieren (nur Summary)')
    parser.add_argument('--config', type=str, default=None,
                        help='Pfad zu einer JSON-Datei mit benutzerdefinierten Szenarien')
    parser.add_argument('--pop_seed', type=int, default=42,
                        help='Random Seed für Populationsgenerierung (Default: 42)')
    parser.add_argument('--lhs_samples', type=int, default=500,
                        help='Anzahl Latin-Hypercube-Stichproben für Populations-Varianten '
                             '(Default: 500). Gesamtszenarien = lhs_samples x Strategie-Kombinationen.')
    parser.add_argument('--lhs_seed', type=int, default=0,
                        help='Random Seed für LHS-Sampling (Default: 0, für Reproduzierbarkeit)')

    args = parser.parse_args()

    # Signal-Handler für Ctrl+C
    signal.signal(signal.SIGINT, _signal_handler)
    signal.signal(signal.SIGTERM, _signal_handler)

    # ---- Import der Simulation (erst hier, damit --help schnell geht) ----
    print("Lade Simulationsmodule...")
    import config_alm as cfg
    import main_alm_simulation as sim

    # Anzahl Kerne
    num_cores = args.num_cores or max(1, mp.cpu_count() - 1)

    # ---- Szenario-Liste aufbauen ----
    if args.config:
        print(f"Lade Szenarien aus {args.config}...")
        scenarios = load_scenarios_from_file(args.config)
        # scenario_id sicherstellen
        for i, s in enumerate(scenarios):
            if 'scenario_id' not in s:
                s['scenario_id'] = i + 1
    else:
        scenarios = build_scenario_list(
            POPULATION_GRID, ALLOCATION_GRID, DURATION_GRID, SAMMELSTIFTUNG_GRID,
            lhs_samples=args.lhs_samples,
            lhs_seed=args.lhs_seed,
        )

    n_total = len(scenarios)

    # ---- Dry Run ----
    if args.dry_run:
        print(f"\n{'='*70}")
        print(f"DRY RUN: {n_total} Szenarien generiert")
        print(f"{'='*70}")
        print(f"  MC-Pfade pro Szenario:  {args.n_paths}")
        print(f"  Horizont:               {args.t_horizon} Jahre")
        print(f"  Kerne:                  {num_cores}")
        print(f"  Output:                 {args.output_dir}")
        print(f"  Pfad-CSVs:              {'Nein' if args.no_path_export else 'Ja'}")

        # Populations-Varianten zählen
        pop_keys = set()
        for s in scenarios:
            pop_key = tuple((k, s[k]) for k in sorted(s.keys()) if k.startswith('pop_'))
            pop_keys.add(pop_key)
        n_lhs = getattr(args, 'lhs_samples', 500)
        print(f"\n  Populations-Varianten:  {len(pop_keys)}  (LHS n={n_lhs}, seed={getattr(args, 'lhs_seed', 0)})")

        # Allokationen zählen
        alloc_keys = set()
        for s in scenarios:
            alloc_keys.add((s['w_gov_bonds'], s['w_corp_bonds'], s['w_equities'],
                           s['w_realestate'], s['w_alternatives']))
        print(f"  Allokationen:           {len(alloc_keys)}")

        # Duration-Strategien
        dur_keys = set()
        for s in scenarios:
            dur_keys.add((s['gov_bond_duration_mode'], s['corp_bond_duration_mode'],
                         s['alt_bond_duration_mode'],
                         s['initial_gov_bond_duration'], s['initial_corp_bond_duration'],
                         s['initial_alt_bond_duration']))
        print(f"  Duration-Strategien:    {len(dur_keys)}")

        # Geschätzte Datenmenge (grob: ~0.5 MB pro Szenario bei 200 Pfaden x 40 Jahren)
        if not args.no_path_export:
            rows_per_scenario = args.n_paths * args.t_horizon
            bytes_per_row = 600  # geschätzt
            est_gb = (n_total * rows_per_scenario * bytes_per_row) / (1024**3)
            print(f"\n  Geschätzte Datenmenge:  ~{est_gb:.1f} GB")
        else:
            print(f"\n  Geschätzte Datenmenge:  ~{n_total * 0.001:.2f} GB (nur Summary)")

        print(f"\n--- Erste 10 Szenarien ---")
        for s in scenarios[:10]:
            alloc_str = (f"Gov={s['w_gov_bonds']:.0%} Corp={s['w_corp_bonds']:.0%} "
                        f"Eq={s['w_equities']:.0%} RE={s['w_realestate']:.0%} "
                        f"Alt={s['w_alternatives']:.0%}")
            dur_str = f"{s['gov_bond_duration_mode']}/{s['corp_bond_duration_mode']}/{s['alt_bond_duration_mode']}"
            pop_str = (f"N={s['pop_n_population']} Age={s['pop_age_mean']} "
                      f"Pen={s['pop_pension_mean']}")
            ss_str = "SS" if s['sammelstiftung_enabled'] else "--"
            print(f"  [{s['scenario_id']:4d}] {pop_str}  |  {alloc_str}  |  {dur_str}  |  {ss_str}")

        if n_total > 10:
            print(f"  ... und {n_total - 10} weitere Szenarien")

        return

    # ---- Daten laden (einmalig) ----
    print(f"\n{'='*70}")
    print(f"SENSITIVITÄTSANALYSE: {n_total} Szenarien")
    print(f"{'='*70}")
    print(f"  MC-Pfade pro Szenario:  {args.n_paths}")
    print(f"  Horizont:               {args.t_horizon} Jahre")
    print(f"  Kerne:                  {num_cores}")
    print(f"  Output:                 {args.output_dir}")
    print(f"  Pfad-CSVs:              {'Nein' if args.no_path_export else 'Ja'}")

    print("\nLade Sterbetafel...")
    survival_table_df = pd.read_csv(cfg.SURVIVAL_TABLE_PATH)
    survival_table_df.set_index(['Age', 'Gender'], inplace=True)

    print("Vorberechnung der Mortality-Arrays...")
    qx_arrays = sim.precompute_mortality_arrays(survival_table_df)

    # Output-Verzeichnis erstellen
    os.makedirs(args.output_dir, exist_ok=True)

    # ---- Resume-Logik ----
    start_idx = 0
    summary_results = []
    summary_csv_path = os.path.join(args.output_dir, 'sensitivity_summary.csv')

    if args.resume is not None:
        start_idx = args.resume - 1  # 1-basiert -> 0-basiert
        print(f"\nFortsetzen ab Szenario {args.resume} (von {n_total})")
        # Versuche bisherige Summary zu laden
        if os.path.exists(summary_csv_path):
            existing_summary = pd.read_csv(summary_csv_path, sep=';', decimal='.')
            summary_results = existing_summary.to_dict('records')
            print(f"  {len(summary_results)} bisherige Ergebnisse geladen")

    # ---- Populations-Cache ----
    # Wir generieren die Population pro Szenario basierend auf den pop_*-Parametern
    # Cache um bei gleichen Parametern die gleiche Population wiederzuverwenden
    pop_cache = {}

    # ---- Simulations-Schleife ----
    overall_start = time.time()
    completed = 0
    skipped = start_idx

    for idx in range(start_idx, n_total):
        if _shutdown_requested:
            print(f"\nAbbruch nach Szenario {idx} (von {n_total})")
            break

        scenario = scenarios[idx]
        sid = scenario['scenario_id']

        # ---- Speicherplatz prüfen ----
        if completed > 0 and completed % args.disk_check_interval == 0:
            ok, free_gb, used_gb = check_disk_space(
                args.output_dir, args.min_free_gb, args.max_disk_gb
            )
            if not ok:
                print(f"\n  ABBRUCH wegen Speicherplatzmangel nach Szenario {sid}")
                print(f"  Frei: {free_gb:.1f} GB, Belegt: {used_gb:.2f} GB")
                break

        # ---- Population generieren oder aus Cache holen ----
        pop_key = tuple(
            (k, scenario[k]) for k in sorted(scenario.keys()) if k.startswith('pop_')
        )
        if pop_key not in pop_cache:
            pop_params = {k.replace('pop_', ''): v for k, v in pop_key}
            pop_df = generate_population_df(pop_params, seed=args.pop_seed)
            pop_cache[pop_key] = pop_df

        population_df = pop_cache[pop_key]

        # ---- Fortschritts-Anzeige ----
        elapsed = time.time() - overall_start
        if completed > 0:
            avg_time = elapsed / completed
            remaining = avg_time * (n_total - idx)
            eta_str = format_duration(remaining)
        else:
            eta_str = "?"

        alloc_str = (f"Gov={scenario['w_gov_bonds']:.0%} Corp={scenario['w_corp_bonds']:.0%} "
                    f"Eq={scenario['w_equities']:.0%} RE={scenario['w_realestate']:.0%} "
                    f"Alt={scenario['w_alternatives']:.0%}")
        dur_str = f"{scenario['gov_bond_duration_mode'][:3]}/{scenario['corp_bond_duration_mode'][:3]}/{scenario['alt_bond_duration_mode'][:3]}"
        pop_str = f"N={scenario['pop_n_population']} Age={scenario['pop_age_mean']}"
        ss_str = "SS" if scenario['sammelstiftung_enabled'] else "--"

        print(f"\n[{idx+1}/{n_total}] Szenario {sid}  |  {pop_str}  |  {alloc_str}  "
              f"|  {dur_str}  |  {ss_str}  |  ETA: {eta_str}")

        # ---- Simulation ausführen ----
        t_start = time.time()
        try:
            result = run_single_scenario(
                scenario=scenario,
                cfg=cfg,
                sim_module=sim,
                n_paths=args.n_paths,
                t_horizon=args.t_horizon,
                output_dir=args.output_dir,
                survival_table_df=survival_table_df,
                population_df=population_df,
                qx_arrays=qx_arrays,
                num_cores=num_cores,
                export_all_paths=not args.no_path_export,
            )
            t_elapsed = time.time() - t_start

            # Ergebnis speichern
            result['runtime_seconds'] = t_elapsed
            summary_results.append(result)
            completed += 1

            # Ergebnis anzeigen
            print(f"  -> P(UF)={result['prob_underfunding']*100:.1f}%  "
                  f"P(Default)={result['prob_default']*100:.1f}%  "
                  f"DG_mean={result['end_dg_mean']*100:.1f}%  "
                  f"DG_P5={result['end_dg_p5']*100:.1f}%  "
                  f"[{format_duration(t_elapsed)}]")

            # ---- Summary inkrementell speichern (alle 10 Szenarien) ----
            if completed % 10 == 0 or idx == n_total - 1:
                summary_df = pd.DataFrame(summary_results)
                summary_df.to_csv(summary_csv_path, index=False, sep=';', decimal='.')

                # Speicherplatz-Info
                ok, free_gb, used_gb = check_disk_space(args.output_dir)
                print(f"  [Checkpoint] {completed} Szenarien gespeichert  "
                      f"|  Frei: {free_gb:.1f} GB  |  Belegt: {used_gb:.2f} GB")

        except Exception as e:
            t_elapsed = time.time() - t_start
            print(f"  FEHLER in Szenario {sid}: {e}")
            # Fehler als Ergebnis speichern
            summary_results.append({
                'scenario_id': sid,
                **{k: v for k, v in scenario.items() if k != 'scenario_id'},
                'prob_underfunding': None,
                'prob_default': None,
                'error': str(e),
                'runtime_seconds': t_elapsed,
            })
            completed += 1
            continue

    # ---- Finale Summary speichern ----
    total_elapsed = time.time() - overall_start

    if summary_results:
        summary_df = pd.DataFrame(summary_results)
        summary_df.to_csv(summary_csv_path, index=False, sep=';', decimal='.')
        print(f"\n{'='*70}")
        print(f"FERTIG: {completed} von {n_total} Szenarien abgeschlossen")
        print(f"{'='*70}")
        print(f"  Gesamtdauer:            {format_duration(total_elapsed)}")
        print(f"  Summary-Datei:          {summary_csv_path}")

        # Finale Speicher-Info
        ok, free_gb, used_gb = check_disk_space(args.output_dir)
        print(f"  Datenvolumen:           {used_gb:.2f} GB")
        print(f"  Freier Speicherplatz:   {free_gb:.1f} GB")

        # Fehler-Zusammenfassung
        n_errors = sum(1 for r in summary_results if r.get('error'))
        if n_errors > 0:
            print(f"  Fehler:                 {n_errors} Szenarien")

        print(f"\nDateien für RStudio:")
        print(f"  - {summary_csv_path}")
        if not args.no_path_export:
            print(f"  - {args.output_dir}/sensitivity_scenario_*.csv")
    else:
        print("\nKeine Szenarien simuliert.")


if __name__ == '__main__':
    main()
