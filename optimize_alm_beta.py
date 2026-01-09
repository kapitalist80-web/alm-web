"""
ALM Parameter-Optimierung
==========================

Dieses Skript optimiert die beeinflussbaren Parameter der ALM-Simulation:
- Asset-Allokation (Gewichtungen)
- Bond-Durationen (GovBonds, CorpBonds)

Ziel: Minimierung von Unterdeckungen und Defaults bei gegebenen Reserven.

Methode: Bayesian Optimization mit scipy.optimize.minimize
         (alternativ: Grid Search oder Random Search)

Verwendung:
    python optimize_alm.py
    python optimize_alm.py --method grid      # Grid Search
    python optimize_alm.py --method random    # Random Search
    python optimize_alm.py --method bayesian  # Bayesian (default)
"""

import numpy as np
import pandas as pd
import multiprocessing as mp
import time
import sys
import argparse
from datetime import datetime
from scipy.optimize import minimize, differential_evolution
from functools import partial
import warnings
warnings.filterwarnings('ignore')

# Import der Simulation
import config_alm as cfg
from main_alm_simulation import (
    load_data, 
    calculate_liability_barwert_initial,
    run_monte_carlo_path_full,
    calculate_risk_metrics,
    precompute_mortality_arrays,
    LOWER_TRIANGLE,
    EXPECTED_BASIS_RATE
)

# ==============================================================================
# OPTIMIERUNGS-KONFIGURATION
# ==============================================================================

# Basis-Anzahl Simulationspfade (für konservative Portfolios)
N_PATHS_BASE = 150

# Maximale Anzahl Pfade (für sehr volatile Portfolios)
N_PATHS_MAX = 400

# Anzahl paralleler Kerne
NUM_CORES = max(1, mp.cpu_count() - 1)

# Parameter-Grenzen für Optimierung
PARAM_BOUNDS = {
    # Asset-Gewichte (müssen zusammen 1.0 ergeben, daher nur 4 frei wählbar)
    'w_gov_bonds': (0.30, 0.80),      # Staatsanleihen: 30-80%
    'w_corp_bonds': (0.05, 0.40),     # Corporate Bonds: 5-40%
    'w_equities': (0.00, 0.20),       # Aktien: 0-20%
    'w_realestate': (0.00, 0.25),     # Immobilien: 0-25%
    # w_alternatives wird berechnet als 1 - Summe der anderen
    
    # Bond-Durationen
    'gov_bond_duration': (8.0, 25.0),   # Staatsanleihen Duration: 8-25 Jahre
    'corp_bond_duration': (3.0, 12.0),  # Corporate Bonds Duration: 3-12 Jahre
}

# Gewichtung der Zielfunktion
# Ziel = α × P(Unterdeckung) + β × P(Default) + γ × E[Ungedeckte Liabilities]
ALPHA_UNDERFUNDING = 1.0    # Gewicht für Unterdeckungswahrscheinlichkeit
BETA_DEFAULT = 5.0          # Gewicht für Default-Wahrscheinlichkeit (höher, da schwerwiegender)
GAMMA_SHORTFALL = 0.00001   # Gewicht für erwarteten Shortfall (skaliert)




# Logging aller gültigen Auswertungen (für finale "Zero-Objective"-Validierung)
EVAL_LOG = []  # wird in objective_function befüllt

# Toleranz, um "Zielfunktion = 0" robust zu erkennen (Floating-Point)
ZERO_OBJECTIVE_TOL = 1e-12

# ==============================================================================
# ADAPTIVE PFADANZAHL
# ==============================================================================

def calculate_adaptive_n_paths(weights, base_paths=N_PATHS_BASE, max_paths=N_PATHS_MAX):
    """
    Berechnet die Anzahl Simulationspfade basierend auf der Portfolio-Volatilität.
    
    Portfolios mit höherer Gewichtung in volatilen Asset-Klassen benötigen mehr
    Pfade für eine zuverlässige Schätzung der Risikometriken.
    
    Args:
        weights: Liste der Asset-Gewichte [0, w_gov, w_corp, w_eq, w_re, w_alt]
        base_paths: Mindestanzahl Pfade (für konservative Portfolios)
        max_paths: Maximalanzahl Pfade (für sehr volatile Portfolios)
    
    Returns:
        n_paths: Anzahl der zu simulierenden Pfade
    """
    # Volatilitäten aus Config (Index: 0=Basis, 1=Gov, 2=Corp, 3=Eq, 4=RE, 5=Alt)
    sigmas = np.array(cfg.SIGMA)
    weights_arr = np.array(weights)
    
    # Berechne Portfolio-Volatilität (vereinfacht, ohne Korrelationen)
    # Dies ist eine obere Schranke der tatsächlichen Volatilität
    portfolio_vol_approx = np.sqrt(np.sum((weights_arr * sigmas) ** 2))
    
    # Referenz-Volatilität: Reines Bond-Portfolio (65% Gov, 35% Corp)
    ref_weights = np.array([0.0, 0.65, 0.35, 0.0, 0.0, 0.0])
    ref_vol = np.sqrt(np.sum((ref_weights * sigmas) ** 2))
    
    # Volatilitäts-Ratio: Wie viel volatiler ist das aktuelle Portfolio?
    vol_ratio = portfolio_vol_approx / ref_vol if ref_vol > 0 else 1.0
    
    # Pfadanzahl skaliert quadratisch mit Volatilitäts-Ratio
    # (Varianz der Schätzung sinkt mit 1/n, also brauchen wir n ~ σ²)
    scaling_factor = vol_ratio ** 2
    
    # Berechne Pfadanzahl (zwischen base und max)
    n_paths = int(base_paths * scaling_factor)
    n_paths = max(base_paths, min(n_paths, max_paths))
    
    return n_paths


def get_volatility_category(weights):
    """
    Gibt eine Kategorie für die Portfolio-Volatilität zurück (für Logging).
    """
    n_paths = calculate_adaptive_n_paths(weights)
    if n_paths <= N_PATHS_BASE * 1.2:
        return "niedrig", n_paths
    elif n_paths <= N_PATHS_BASE * 2.0:
        return "mittel", n_paths
    else:
        return "hoch", n_paths


# ==============================================================================
# HILFSFUNKTIONEN
# ==============================================================================

def validate_weights(w_gov, w_corp, w_eq, w_re):
    """Prüft ob Gewichte gültig sind und berechnet Alternatives-Gewicht."""
    total = w_gov + w_corp + w_eq + w_re
    if total > 1.0:
        return None, False
    w_alt = 1.0 - total
    if w_alt < 0 or w_alt > 0.30:  # Alternatives max 30%
        return None, False
    return w_alt, True


def _round_params_for_key(params, nd_w=6, nd_d=3):
    """Hilfsfunktion zum Deduplizieren nahezu identischer Parameter."""
    return (
        round(float(params['w_gov_bonds']), nd_w),
        round(float(params['w_corp_bonds']), nd_w),
        round(float(params['w_equities']), nd_w),
        round(float(params['w_realestate']), nd_w),
        round(float(params['w_alternatives']), nd_w),
        round(float(params['gov_bond_duration']), nd_d),
        round(float(params['corp_bond_duration']), nd_d),
    )


def log_evaluation(params, objective, risk_metrics, n_paths_used, source='objective'):
    """Speichert jede gültige Auswertung in EVAL_LOG für spätere finale Tests."""
    try:
        EVAL_LOG.append({
            'source': source,
            'n_paths_used': int(n_paths_used),
            'reserve_rate': float(cfg.GENERAL_RESERVE_RATE),
            'objective': float(objective),
            'prob_underfunding': float(risk_metrics.get('prob_underfunding', np.nan)),
            'prob_default': float(risk_metrics.get('prob_default', np.nan)),
            'n_underfunding': int(risk_metrics.get('n_paths_underfunding', -1)),
            'n_default': int(risk_metrics.get('n_paths_default', -1)),
            **params
        })
    except Exception:
        # Logging darf die Optimierung nicht stören
        pass


def _find_first_col(df, candidates):
    """Gibt die erste passende Spalte (case-insensitive) zurück, sonst None."""
    cols_l = {c.lower(): c for c in df.columns}
    for cand in candidates:
        if cand.lower() in cols_l:
            return cols_l[cand.lower()]
    return None


def summarize_initial_population(initial_population_df, survival_table_df=None, W0=None):
    """Erstellt robuste Kennzahlen des ursprünglichen Rentnerbestandes.

    Liefert ein Dict, das direkt in CSV-Outputs gemerged werden kann.
    Spaltennamen werden heuristisch erkannt (Alter/Geschlecht/Zivilstand/Rente).
    """
    df = initial_population_df.copy()

    # Heuristische Spaltenzuordnung (erweitert um Spaltennamen aus main_alm_simulation.py)
    col_gender = _find_first_col(df, ['gender', 'geschlecht', 'sex', 'g'])
    col_age = _find_first_col(df, ['age', 'alter', 'current_age', 'age_years'])
    col_married = _find_first_col(df, ['maritalstatus', 'marital_status', 'verheiratet', 'married', 'is_married', 'civil_status', 'zivilstand'])
    col_pension = _find_first_col(df, ['initialpension', 'initial_pension', 'rente', 'pension', 'annual_pension', 'pension_amount', 'rente_jahr', 'jahresrente'])

    out = {}

    # Gesamtgrössen
    out['bestand_n_personen'] = int(len(df))
    if col_pension is not None:
        pension = pd.to_numeric(df[col_pension], errors='coerce')
        out['bestand_rente_summe'] = float(pension.sum(skipna=True))
        out['bestand_rente_mean'] = float(pension.mean(skipna=True))
        out['bestand_rente_std'] = float(pension.std(skipna=True)) if pension.notna().any() else np.nan
        # Verteilung der Rente (robuste Quantile)
        for q, name in [(0.10, 'p10'), (0.25, 'p25'), (0.50, 'p50'), (0.75, 'p75'), (0.90, 'p90')]:
            out[f'bestand_rente_{name}'] = float(pension.quantile(q)) if pension.notna().any() else np.nan
        out['bestand_rente_min'] = float(pension.min(skipna=True)) if pension.notna().any() else np.nan
        out['bestand_rente_max'] = float(pension.max(skipna=True)) if pension.notna().any() else np.nan
    else:
        out['bestand_rente_summe'] = np.nan
        out['bestand_rente_mean'] = np.nan
        out['bestand_rente_std'] = np.nan
        for name in ['p10', 'p25', 'p50', 'p75', 'p90', 'min', 'max']:
            out[f'bestand_rente_{name}'] = np.nan

    # Durchschnittsalter
    if col_age is not None:
        age = pd.to_numeric(df[col_age], errors='coerce')
        out['bestand_alter_mean'] = float(age.mean(skipna=True))
        out['bestand_alter_p50'] = float(age.quantile(0.50)) if age.notna().any() else np.nan
    else:
        out['bestand_alter_mean'] = np.nan
        out['bestand_alter_p50'] = np.nan

    # Anteile Geschlecht
    if col_gender is not None:
        g = df[col_gender].astype(str).str.strip().str.upper()
        # Normalisierung typischer Codes (alles auf M/F)
        g = g.replace({
            'M': 'M', 'MALE': 'M', 'MANN': 'M', 'MAENNLICH': 'M', 'MÄNNLICH': 'M',
            'F': 'F', 'W': 'F', 'FEMALE': 'F', 'FRAU': 'F', 'WEIBLICH': 'F'
        })
        total = max(1, len(g))
        out['bestand_anteil_m'] = float((g == 'M').sum() / total)
        out['bestand_anteil_w'] = float((g == 'F').sum() / total)
        out['bestand_anteil_other_unknown'] = float((~g.isin(['M', 'F'])).sum() / total)
    else:
        out['bestand_anteil_m'] = np.nan
        out['bestand_anteil_w'] = np.nan
        out['bestand_anteil_other_unknown'] = np.nan

    # Anteil Verheiratete / Ledige (Zivilstand)
    if col_married is not None:
        m = df[col_married]
        # robuste Interpretation (Bool/0-1/Strings)
        if pd.api.types.is_bool_dtype(m):
            married = m
        else:
            ms = m.astype(str).str.strip().str.lower()
            married = ms.isin(['1', 'true', 't', 'yes', 'y', 'ja', 'verheiratet', 'married', 'ehe', 'e'])
            # falls numerisch (0/1)
            mn = pd.to_numeric(m, errors='coerce')
            married = married | (mn == 1)
        out['bestand_anteil_verheiratet'] = float(married.mean())
        out['bestand_anteil_ledig'] = float((~married).mean())
    else:
        out['bestand_anteil_verheiratet'] = np.nan
        out['bestand_anteil_ledig'] = np.nan

    # Initialer Barwert
    out['bestand_W0_initialer_barwert'] = float(W0) if W0 is not None else np.nan

    return out


def final_validate_zero_objective_candidates(
    survival_table_df, initial_population_df, qx_arrays_precomputed, W0,
    n_paths_final=500, out_dir='data', population_summary=None
):
    """
    Nimmt alle während der Optimierung getesteten Kombinationen, filtert jene mit
    Zielfunktion ~= 0 (=> P(Unterdeckung)=0 und P(Default)=0) und testet sie
    nochmals final mit fix n_paths_final. Speichert nur jene, die auch final
    Zielfunktion ~= 0 erreichen.
    """
    import os
    os.makedirs(out_dir, exist_ok=True)

    if len(EVAL_LOG) == 0:
        return None, None

    eval_df = pd.DataFrame(EVAL_LOG)

    # Bestand-Parameter als konstante Spalten hinzufügen (falls gewünscht)
    if population_summary:
        for k, v in population_summary.items():
            eval_df[k] = v

    # Kandidaten: objective ~ 0 (robust) UND keine Defaults/Unterdeckungen gemeldet
    cand = eval_df[
        (eval_df['objective'] <= ZERO_OBJECTIVE_TOL) &
        (eval_df['prob_default'] <= ZERO_OBJECTIVE_TOL) &
        (eval_df['prob_underfunding'] <= ZERO_OBJECTIVE_TOL)
    ].copy()

    if cand.empty:
        return eval_df, None

    # Deduplizieren über gerundeten Key
    seen = set()
    unique_params = []
    for _, row in cand.iterrows():
        params = {
            'w_gov_bonds': row['w_gov_bonds'],
            'w_corp_bonds': row['w_corp_bonds'],
            'w_equities': row['w_equities'],
            'w_realestate': row['w_realestate'],
            'w_alternatives': row['w_alternatives'],
            'gov_bond_duration': row['gov_bond_duration'],
            'corp_bond_duration': row['corp_bond_duration'],
        }
        k = _round_params_for_key(params)
        if k in seen:
            continue
        seen.add(k)
        unique_params.append(params)

    final_rows = []
    for i, params in enumerate(unique_params, 1):
        try:
            final_metrics, _ = run_simulation_with_params(
                params, survival_table_df, initial_population_df,
                qx_arrays_precomputed, W0, n_paths=n_paths_final
            )
            obj_final = (ALPHA_UNDERFUNDING * final_metrics['prob_underfunding'] +
                         BETA_DEFAULT * final_metrics['prob_default'] +
                         (GAMMA_SHORTFALL * (final_metrics.get('W_at_default_mean', 0) *
                                            final_metrics['prob_default']
                                            if final_metrics.get('n_paths_default', 0) > 0 else 0)))
            if obj_final <= ZERO_OBJECTIVE_TOL:
                final_rows.append({
                    'rank': len(final_rows) + 1,
                    'n_paths_final': n_paths_final,
                    'reserve_rate': float(cfg.GENERAL_RESERVE_RATE),
                    'objective_final': float(obj_final),
                    'prob_underfunding_final': float(final_metrics['prob_underfunding']),
                    'prob_default_final': float(final_metrics['prob_default']),
                    'n_underfunding_final': int(final_metrics['n_paths_underfunding']),
                    'n_default_final': int(final_metrics['n_paths_default']),
                    **params
                })
        except Exception as e:
            print(f"  Fehler bei finaler Validierung Kandidat {i}: {e}")

    if len(final_rows) == 0:
        return eval_df, None

    final_df = pd.DataFrame(final_rows)
    if population_summary:
        for k, v in population_summary.items():
            final_df[k] = v
    timestamp_str = datetime.now().strftime('%Y%m%d_%H%M%S')
    out_path = os.path.join(out_dir, f"final_zero_objective_candidates_{timestamp_str}.csv")
    final_df.to_csv(out_path, index=False, sep=';')

    return eval_df, out_path



def run_simulation_with_params(params, survival_table_df, initial_population_df, 
                                qx_arrays_precomputed, W0, n_paths=None):
    """
    Führt eine Simulation mit den gegebenen Parametern durch.
    
    Args:
        params: Dict mit den zu optimierenden Parametern
        survival_table_df, initial_population_df: Daten
        qx_arrays_precomputed: Vorberechnete Mortality-Arrays
        W0: Initialer Barwert der Verpflichtungen
        n_paths: Anzahl Simulationspfade (None = adaptiv basierend auf Volatilität)
    
    Returns:
        risk_metrics: Dictionary mit Risikometriken
        n_paths_used: Tatsächlich verwendete Pfadanzahl
    """
    # Parameter in Config überschreiben (temporär)
    original_weights = cfg.WEIGHTS.copy()
    original_gov_duration = cfg.INITIAL_GOV_BOND_DURATION
    original_corp_duration = cfg.INITIAL_CORP_BOND_DURATION
    
    try:
        # Neue Gewichte setzen
        new_weights = [0.0,  # Basiszins (immer 0)
                       params['w_gov_bonds'],
                       params['w_corp_bonds'],
                       params['w_equities'],
                       params['w_realestate'],
                       params['w_alternatives']]
        cfg.WEIGHTS = new_weights
        
        # Neue Durationen setzen
        cfg.INITIAL_GOV_BOND_DURATION = params['gov_bond_duration']
        cfg.INITIAL_CORP_BOND_DURATION = params['corp_bond_duration']
        cfg.INITIAL_BOND_DURATION = params['gov_bond_duration']  # Legacy
        
        # Adaptive Pfadanzahl berechnen falls nicht explizit angegeben
        if n_paths is None:
            n_paths = calculate_adaptive_n_paths(new_weights)
        
        # Initiales Vermögen berechnen
        INITIAL_ASSETS_V0 = W0 * (1 + cfg.GENERAL_RESERVE_RATE)
        
        # Simulation durchführen
        path_args_base = (initial_population_df, survival_table_df, cfg.T_HORIZON, 
                          INITIAL_ASSETS_V0, qx_arrays_precomputed)
        map_arguments = [path_args_base] * n_paths
        
        pool = mp.Pool(processes=NUM_CORES)
        full_results = pool.map(run_monte_carlo_path_full, map_arguments)
        pool.close()
        pool.join()
        
        # Risikometriken berechnen
        risk_metrics = calculate_risk_metrics(full_results, dg_threshold=0.80)
        
        return risk_metrics, n_paths
        
    finally:
        # Original-Parameter wiederherstellen
        cfg.WEIGHTS = original_weights
        cfg.INITIAL_GOV_BOND_DURATION = original_gov_duration
        cfg.INITIAL_CORP_BOND_DURATION = original_corp_duration
        cfg.INITIAL_BOND_DURATION = original_gov_duration


def objective_function(x, survival_table_df, initial_population_df, 
                       qx_arrays_precomputed, W0, iteration_counter):
    """
    Zielfunktion für die Optimierung.
    
    Args:
        x: Array [w_gov, w_corp, w_eq, w_re, gov_dur, corp_dur]
        
    Returns:
        Skalarer Wert (zu minimieren)
    """
    iteration_counter[0] += 1
    
    w_gov, w_corp, w_eq, w_re, gov_dur, corp_dur = x
    
    # Gewichte validieren
    w_alt, valid = validate_weights(w_gov, w_corp, w_eq, w_re)
    if not valid:
        return 1e10  # Ungültige Kombination -> hoher Penalty
    
    params = {
        'w_gov_bonds': w_gov,
        'w_corp_bonds': w_corp,
        'w_equities': w_eq,
        'w_realestate': w_re,
        'w_alternatives': w_alt,
        'gov_bond_duration': gov_dur,
        'corp_bond_duration': corp_dur
    }
    
    try:
        risk_metrics, n_paths_used = run_simulation_with_params(
            params, survival_table_df, initial_population_df,
            qx_arrays_precomputed, W0
        )
        
        # Zielfunktion berechnen
        prob_underfunding = risk_metrics['prob_underfunding']
        prob_default = risk_metrics['prob_default']
        
        # Erwarteter Shortfall bei Default
        if risk_metrics['n_paths_default'] > 0:
            expected_shortfall = risk_metrics['W_at_default_mean'] * prob_default
        else:
            expected_shortfall = 0
        
        objective = (ALPHA_UNDERFUNDING * prob_underfunding + 
                     BETA_DEFAULT * prob_default + 
                     GAMMA_SHORTFALL * expected_shortfall)

        # Für spätere finale Tests protokollieren
        log_evaluation(params, objective, risk_metrics, n_paths_used, source='objective_function')
        
        # Fortschritt ausgeben
        if iteration_counter[0] % 5 == 0 or iteration_counter[0] == 1:
            print(f"  Iter {iteration_counter[0]:3d} (n={n_paths_used:3d}): "
                  f"Obj={objective:.4f} | "
                  f"P(UD)={prob_underfunding*100:.1f}% | "
                  f"P(Def)={prob_default*100:.1f}% | "
                  f"Gov={w_gov*100:.0f}% Corp={w_corp*100:.0f}% Eq={w_eq*100:.0f}% "
                  f"RE={w_re*100:.0f}% Alt={w_alt*100:.0f}% | "
                  f"D_gov={gov_dur:.1f} D_corp={corp_dur:.1f}")
        
        return objective
        
    except Exception as e:
        print(f"  Fehler in Iteration {iteration_counter[0]}: {e}")
        return 1e10


# ==============================================================================
# OPTIMIERUNGS-METHODEN
# ==============================================================================

def optimize_bayesian(survival_table_df, initial_population_df, 
                      qx_arrays_precomputed, W0, max_iterations=50):
    """
    Bayesian Optimization mit Differential Evolution.
    """
    print("\n" + "="*70)
    print("STARTE BAYESIAN OPTIMIZATION (Differential Evolution)")
    print("="*70)
    
    bounds = [
        PARAM_BOUNDS['w_gov_bonds'],
        PARAM_BOUNDS['w_corp_bonds'],
        PARAM_BOUNDS['w_equities'],
        PARAM_BOUNDS['w_realestate'],
        PARAM_BOUNDS['gov_bond_duration'],
        PARAM_BOUNDS['corp_bond_duration'],
    ]
    
    iteration_counter = [0]
    
    result = differential_evolution(
        objective_function,
        bounds=bounds,
        args=(survival_table_df, initial_population_df, 
              qx_arrays_precomputed, W0, iteration_counter),
        maxiter=max_iterations,
        popsize=10,
        mutation=(0.5, 1.0),
        recombination=0.7,
        seed=42,
        disp=False,
        workers=1  # Bereits parallelisiert in der Simulation
    )
    
    return result


def optimize_grid_search(survival_table_df, initial_population_df,
                         qx_arrays_precomputed, W0):
    """
    Grid Search über vordefinierte Parameter-Kombinationen.
    """
    print("\n" + "="*70)
    print("STARTE GRID SEARCH")
    print("="*70)
    
    # Definiere Grid (reduziert für Rechenzeit)
    gov_weights = [0.40, 0.50, 0.60, 0.70]
    corp_weights = [0.10, 0.20, 0.30]
    eq_weights = [0.00, 0.05, 0.10]
    re_weights = [0.05, 0.10, 0.15]
    gov_durations = [12.0, 15.0, 18.0]
    corp_durations = [5.0, 8.0]
    
    results = []
    iteration = 0
    total_combinations = 0
    
    # Zähle gültige Kombinationen
    for w_gov in gov_weights:
        for w_corp in corp_weights:
            for w_eq in eq_weights:
                for w_re in re_weights:
                    w_alt, valid = validate_weights(w_gov, w_corp, w_eq, w_re)
                    if valid:
                        total_combinations += len(gov_durations) * len(corp_durations)
    
    print(f"  Teste {total_combinations} gültige Kombinationen...")
    print(f"  Adaptive Pfadanzahl: {N_PATHS_BASE}-{N_PATHS_MAX} (basierend auf Volatilität)")
    
    for w_gov in gov_weights:
        for w_corp in corp_weights:
            for w_eq in eq_weights:
                for w_re in re_weights:
                    w_alt, valid = validate_weights(w_gov, w_corp, w_eq, w_re)
                    if not valid:
                        continue
                    
                    for gov_dur in gov_durations:
                        for corp_dur in corp_durations:
                            iteration += 1
                            
                            params = {
                                'w_gov_bonds': w_gov,
                                'w_corp_bonds': w_corp,
                                'w_equities': w_eq,
                                'w_realestate': w_re,
                                'w_alternatives': w_alt,
                                'gov_bond_duration': gov_dur,
                                'corp_bond_duration': corp_dur
                            }
                            
                            try:
                                risk_metrics, n_paths_used = run_simulation_with_params(
                                    params, survival_table_df, initial_population_df,
                                    qx_arrays_precomputed, W0
                                )
                                
                                objective = (ALPHA_UNDERFUNDING * risk_metrics['prob_underfunding'] + 
                                             BETA_DEFAULT * risk_metrics['prob_default'])
                                
                                results.append({
                                    'iteration': iteration,
                                    'n_paths': n_paths_used,
                                    'objective': objective,
                                    'prob_underfunding': risk_metrics['prob_underfunding'],
                                    'prob_default': risk_metrics['prob_default'],
                                    'n_underfunding': risk_metrics['n_paths_underfunding'],
                                    'n_default': risk_metrics['n_paths_default'],
                                    **params
                                })
                                
                                log_evaluation(params, objective, risk_metrics, n_paths_used, source='grid_search')
                                
                                if iteration % 10 == 0:
                                    print(f"  {iteration}/{total_combinations} (n={n_paths_used}): "
                                          f"Obj={objective:.4f} | "
                                          f"P(UD)={risk_metrics['prob_underfunding']*100:.1f}% | "
                                          f"P(Def)={risk_metrics['prob_default']*100:.1f}%")
                                
                            except Exception as e:
                                print(f"  Fehler bei Iteration {iteration}: {e}")
    
    # Beste Ergebnisse finden
    results_df = pd.DataFrame(results)
    results_df = results_df.sort_values('objective')
    
    return results_df


def optimize_random_search(survival_table_df, initial_population_df,
                           qx_arrays_precomputed, W0, n_iterations=100):
    """
    Random Search mit zufälligen Parameter-Kombinationen.
    """
    print("\n" + "="*70)
    print(f"STARTE RANDOM SEARCH ({n_iterations} Iterationen)")
    print("="*70)
    
    np.random.seed(42)
    results = []
    
    for iteration in range(1, n_iterations + 1):
        # Zufällige Parameter generieren
        valid = False
        while not valid:
            w_gov = np.random.uniform(*PARAM_BOUNDS['w_gov_bonds'])
            w_corp = np.random.uniform(*PARAM_BOUNDS['w_corp_bonds'])
            w_eq = np.random.uniform(*PARAM_BOUNDS['w_equities'])
            w_re = np.random.uniform(*PARAM_BOUNDS['w_realestate'])
            w_alt, valid = validate_weights(w_gov, w_corp, w_eq, w_re)
        
        gov_dur = np.random.uniform(*PARAM_BOUNDS['gov_bond_duration'])
        corp_dur = np.random.uniform(*PARAM_BOUNDS['corp_bond_duration'])
        
        params = {
            'w_gov_bonds': w_gov,
            'w_corp_bonds': w_corp,
            'w_equities': w_eq,
            'w_realestate': w_re,
            'w_alternatives': w_alt,
            'gov_bond_duration': gov_dur,
            'corp_bond_duration': corp_dur
        }
        
        try:
            risk_metrics, n_paths_used = run_simulation_with_params(
                params, survival_table_df, initial_population_df,
                qx_arrays_precomputed, W0
            )
            
            objective = (ALPHA_UNDERFUNDING * risk_metrics['prob_underfunding'] + 
                         BETA_DEFAULT * risk_metrics['prob_default'])
            
            results.append({
                'iteration': iteration,
                'n_paths': n_paths_used,
                'objective': objective,
                'prob_underfunding': risk_metrics['prob_underfunding'],
                'prob_default': risk_metrics['prob_default'],
                'n_underfunding': risk_metrics['n_paths_underfunding'],
                'n_default': risk_metrics['n_paths_default'],
                **params
            })
            log_evaluation(params, objective, risk_metrics, n_paths_used, source='random_search')
            
            if iteration % 10 == 0:
                best_so_far = min(results, key=lambda x: x['objective'])
                print(f"  Iter {iteration:3d} (n={n_paths_used:3d}): "
                      f"Obj={objective:.4f} | "
                      f"Bestes bisher: {best_so_far['objective']:.4f}")
            
        except Exception as e:
            print(f"  Fehler bei Iteration {iteration}: {e}")
    
    results_df = pd.DataFrame(results)
    results_df = results_df.sort_values('objective')
    
    return results_df


# ==============================================================================
# HAUPTPROGRAMM
# ==============================================================================

def main():
    parser = argparse.ArgumentParser(description='ALM Parameter-Optimierung')
    parser.add_argument('--method', choices=['bayesian', 'grid', 'random'], 
                        default='bayesian', help='Optimierungsmethode')
    parser.add_argument('--iterations', type=int, default=50,
                        help='Max. Iterationen (für bayesian/random)')
    args = parser.parse_args()
    
    # Erstelle data-Ordner falls nicht vorhanden
    import os
    os.makedirs('data', exist_ok=True)
    
    print("\n" + "="*70)
    print("ALM PARAMETER-OPTIMIERUNG")
    print("="*70)
    print(f"Methode: {args.method}")
    print(f"Adaptive Pfadanzahl: {N_PATHS_BASE}-{N_PATHS_MAX} (basierend auf Volatilität)")
    print(f"Parallele Kerne: {NUM_CORES}")
    print(f"Reservenrate: {cfg.GENERAL_RESERVE_RATE*100:.1f}%")
    
    start_time = time.time()
    
    # Daten laden
    print("\n> Lade Daten...")
    survival_table_df, initial_population_df = load_data(
        cfg.SURVIVAL_TABLE_PATH, cfg.POPULATION_PATH
    )
    
    if survival_table_df is None:
        print("FEHLER: Daten konnten nicht geladen werden.")
        sys.exit(1)
    
    # Initialer Barwert berechnen
    W0 = calculate_liability_barwert_initial(initial_population_df, survival_table_df)
    print(f"  Initialer Barwert W(0): {W0:,.0f} CHF")

    # Kennzahlen des ursprünglichen Rentnerbestandes (für CSV-Outputs)
    population_summary = summarize_initial_population(
        initial_population_df, survival_table_df=survival_table_df, W0=W0
    )
    
    # Mortality-Arrays vorberechnen
    print("> Vorberechnung der Sterbetafeln...")
    qx_arrays_precomputed = precompute_mortality_arrays(survival_table_df)
    
    # Aktuelle Parameter als Baseline testen
    print("\n> Teste aktuelle Parameter (Baseline)...")
    current_params = {
        'w_gov_bonds': cfg.WEIGHTS[1],
        'w_corp_bonds': cfg.WEIGHTS[2],
        'w_equities': cfg.WEIGHTS[3],
        'w_realestate': cfg.WEIGHTS[4],
        'w_alternatives': cfg.WEIGHTS[5],
        'gov_bond_duration': cfg.INITIAL_GOV_BOND_DURATION,
        'corp_bond_duration': cfg.INITIAL_CORP_BOND_DURATION
    }
    
    baseline_metrics, baseline_n_paths = run_simulation_with_params(
        current_params, survival_table_df, initial_population_df,
        qx_arrays_precomputed, W0
    )
    
    baseline_objective = (ALPHA_UNDERFUNDING * baseline_metrics['prob_underfunding'] + 
                          BETA_DEFAULT * baseline_metrics['prob_default'])
    
    print(f"\n  BASELINE ERGEBNIS (n={baseline_n_paths} Pfade):")
    print(f"  Gewichte: Gov={current_params['w_gov_bonds']*100:.0f}% "
          f"Corp={current_params['w_corp_bonds']*100:.0f}% "
          f"Eq={current_params['w_equities']*100:.0f}% "
          f"RE={current_params['w_realestate']*100:.0f}% "
          f"Alt={current_params['w_alternatives']*100:.0f}%")
    print(f"  Durationen: Gov={current_params['gov_bond_duration']:.1f}J "
          f"Corp={current_params['corp_bond_duration']:.1f}J")
    print(f"  P(Unterdeckung): {baseline_metrics['prob_underfunding']*100:.1f}% "
          f"({baseline_metrics['n_paths_underfunding']} Pfade)")
    print(f"  P(Default): {baseline_metrics['prob_default']*100:.1f}% "
          f"({baseline_metrics['n_paths_default']} Pfade)")
    print(f"  Zielfunktion: {baseline_objective:.4f}")
    
    # Optimierung durchführen
    if args.method == 'bayesian':
        result = optimize_bayesian(
            survival_table_df, initial_population_df,
            qx_arrays_precomputed, W0, max_iterations=args.iterations
        )
        
        # Beste Parameter extrahieren
        w_gov, w_corp, w_eq, w_re, gov_dur, corp_dur = result.x
        w_alt, _ = validate_weights(w_gov, w_corp, w_eq, w_re)
        
        best_params = {
            'w_gov_bonds': w_gov,
            'w_corp_bonds': w_corp,
            'w_equities': w_eq,
            'w_realestate': w_re,
            'w_alternatives': w_alt,
            'gov_bond_duration': gov_dur,
            'corp_bond_duration': corp_dur
        }
        best_objective = result.fun
        
    elif args.method == 'grid':
        results_df = optimize_grid_search(
            survival_table_df, initial_population_df,
            qx_arrays_precomputed, W0
        )
        
        best_row = results_df.iloc[0]
        best_params = {
            'w_gov_bonds': best_row['w_gov_bonds'],
            'w_corp_bonds': best_row['w_corp_bonds'],
            'w_equities': best_row['w_equities'],
            'w_realestate': best_row['w_realestate'],
            'w_alternatives': best_row['w_alternatives'],
            'gov_bond_duration': best_row['gov_bond_duration'],
            'corp_bond_duration': best_row['corp_bond_duration']
        }
        best_objective = best_row['objective']
        
    else:  # random
        results_df = optimize_random_search(
            survival_table_df, initial_population_df,
            qx_arrays_precomputed, W0, n_iterations=args.iterations
        )
        
        best_row = results_df.iloc[0]
        best_params = {
            'w_gov_bonds': best_row['w_gov_bonds'],
            'w_corp_bonds': best_row['w_corp_bonds'],
            'w_equities': best_row['w_equities'],
            'w_realestate': best_row['w_realestate'],
            'w_alternatives': best_row['w_alternatives'],
            'gov_bond_duration': best_row['gov_bond_duration'],
            'corp_bond_duration': best_row['corp_bond_duration']
        }
        best_objective = best_row['objective']
    
    # Finale Validierung mit mehr Pfaden (fix 500, unabhängig von Volatilität)
    print("\n" + "="*70)
    print("FINALE VALIDIERUNG (mit 500 Pfaden)")
    print("="*70)
    
    final_metrics, _ = run_simulation_with_params(
        best_params, survival_table_df, initial_population_df,
        qx_arrays_precomputed, W0, n_paths=500
    )
    
    end_time = time.time()
    
    # Ergebnisse ausgeben
    print("\n" + "="*70)
    print("OPTIMIERUNGS-ERGEBNIS")
    print("="*70)
    
    print(f"\n  OPTIMALE PARAMETER:")
    print(f"  {'─'*50}")
    print(f"  Asset-Allokation:")
    print(f"    Staatsanleihen:  {best_params['w_gov_bonds']*100:5.1f}%")
    print(f"    Corporate Bonds: {best_params['w_corp_bonds']*100:5.1f}%")
    print(f"    Aktien:          {best_params['w_equities']*100:5.1f}%")
    print(f"    Immobilien:      {best_params['w_realestate']*100:5.1f}%")
    print(f"    Alternatives:    {best_params['w_alternatives']*100:5.1f}%")
    print(f"  {'─'*50}")
    print(f"  Bond-Durationen:")
    print(f"    Staatsanleihen:  {best_params['gov_bond_duration']:5.1f} Jahre")
    print(f"    Corporate Bonds: {best_params['corp_bond_duration']:5.1f} Jahre")
    
    print(f"\n  RISIKOMETRIKEN (finale Validierung):")
    print(f"  {'─'*50}")
    print(f"  P(Unterdeckung):    {final_metrics['prob_underfunding']*100:5.1f}% "
          f"({final_metrics['n_paths_underfunding']} von {final_metrics['n_paths_total']} Pfaden)")
    print(f"  P(Default):         {final_metrics['prob_default']*100:5.1f}% "
          f"({final_metrics['n_paths_default']} von {final_metrics['n_paths_total']} Pfaden)")
    
    if final_metrics['n_paths_default'] > 0:
        print(f"  Ø Ungedeckte Liab.: {final_metrics['W_at_default_mean']:,.0f} CHF")
    
    print(f"\n  VERGLEICH ZU BASELINE:")
    print(f"  {'─'*50}")
    improvement_ud = (baseline_metrics['prob_underfunding'] - final_metrics['prob_underfunding']) * 100
    improvement_def = (baseline_metrics['prob_default'] - final_metrics['prob_default']) * 100
    print(f"  Δ P(Unterdeckung):  {improvement_ud:+.1f} Prozentpunkte")
    print(f"  Δ P(Default):       {improvement_def:+.1f} Prozentpunkte")
    
    print(f"\n  Rechenzeit: {end_time - start_time:.1f} Sekunden")

    print("\n" + "="*70)
    print("FINALE ZERO-OBJECTIVE KANDIDATEN (aus allen getesteten Kombinationen)")
    print("="*70)

    eval_df, zero_csv_path = final_validate_zero_objective_candidates(
        survival_table_df, initial_population_df, qx_arrays_precomputed, W0,
        n_paths_final=500, out_dir='data', population_summary=population_summary
    )

    # Optional: komplettes Eval-Log speichern (hilfreich für Debug/Analyse)
    if eval_df is not None and not eval_df.empty:
        timestamp_str = datetime.now().strftime('%Y%m%d_%H%M%S')
        all_path = f"data/eval_log_all_{timestamp_str}.csv"
        eval_df.to_csv(all_path, index=False, sep=';')
        print(f"  Alle Auswertungen gespeichert in: {all_path}")

    if zero_csv_path is None:
        print("  Keine Kombinationen gefunden, die Zielfunktion ~= 0 erreichen (auch nicht vor Finaltest).")
    else:
        print(f"  Zero-Objective Kandidaten (final validiert) gespeichert in: {zero_csv_path}")

    
        # Config-Vorschlag ausgeben
        print("\n" + "="*70)
        print("EMPFOHLENE CONFIG-ÄNDERUNGEN")
        print("="*70)
        print(f"""
    # Asset Allocation (optimiert)
    WEIGHTS = [0.00, {best_params['w_gov_bonds']:.2f}, {best_params['w_corp_bonds']:.2f}, {best_params['w_equities']:.2f}, {best_params['w_realestate']:.2f}, {best_params['w_alternatives']:.2f}]

    # Bond-Durationen (optimiert)
    INITIAL_GOV_BOND_DURATION = {best_params['gov_bond_duration']:.1f}
    INITIAL_CORP_BOND_DURATION = {best_params['corp_bond_duration']:.1f}
    """)
    
        # Ergebnisse in Datei speichern
        timestamp_str = datetime.now().strftime('%Y%m%d_%H%M%S')
    
        # Empfohlene Config-Änderungen als CSV mit allen Parametern
        config_recommendation = [
            # Meta-Information
            ['OPTIMIERUNGS-ERGEBNIS', ''],
            ['Zeitstempel', datetime.now().strftime('%Y-%m-%d %H:%M:%S')],
            ['Methode', args.method],
            ['Pfadanzahl Optimierung', f"adaptiv {N_PATHS_BASE}-{N_PATHS_MAX}"],
            ['Pfade Validierung', 500],
            ['', ''],

            # Ursprünglicher Rentnerbestand (Kennzahlen)
            ['RENTNERBESTAND (Input-Kennzahlen)', ''],
        ]

        # Kennzahlen dynamisch anhängen, damit sich das Format bei neuen Spalten nicht ändert
        for k in sorted(population_summary.keys()):
            v = population_summary[k]
            if isinstance(v, float) and np.isfinite(v):
                config_recommendation.append([k, f"{v:.6g}"])
            else:
                config_recommendation.append([k, v])

        # Leerzeile nach Bestandskennzahlen
        config_recommendation += [
            ['', ''],
        
            # Optimierte Parameter
            ['OPTIMIERTE PARAMETER', ''],
            ['w_gov_bonds', f"{best_params['w_gov_bonds']:.4f}"],
            ['w_corp_bonds', f"{best_params['w_corp_bonds']:.4f}"],
            ['w_equities', f"{best_params['w_equities']:.4f}"],
            ['w_realestate', f"{best_params['w_realestate']:.4f}"],
            ['w_alternatives', f"{best_params['w_alternatives']:.4f}"],
            ['gov_bond_duration', f"{best_params['gov_bond_duration']:.1f}"],
            ['corp_bond_duration', f"{best_params['corp_bond_duration']:.1f}"],
            ['', ''],
        
            # Ergebnis der Optimierung
            ['OPTIMIERUNGS-ERGEBNIS', ''],
            ['P(Unterdeckung) optimiert', f"{final_metrics['prob_underfunding']*100:.2f}%"],
            ['P(Default) optimiert', f"{final_metrics['prob_default']*100:.2f}%"],
            ['Anzahl Unterdeckung', f"{final_metrics['n_paths_underfunding']}"],
            ['Anzahl Default', f"{final_metrics['n_paths_default']}"],
            ['', ''],
        
            # Baseline zum Vergleich
            ['BASELINE (vor Optimierung)', ''],
            ['P(Unterdeckung) Baseline', f"{baseline_metrics['prob_underfunding']*100:.2f}%"],
            ['P(Default) Baseline', f"{baseline_metrics['prob_default']*100:.2f}%"],
            ['Verbesserung P(Unterdeckung)', f"{improvement_ud:+.2f} PP"],
            ['Verbesserung P(Default)', f"{improvement_def:+.2f} PP"],
            ['', ''],
        
            # Verwendete Kapitalmarkt-Parameter
            ['KAPITALMARKT-PARAMETER (fixiert)', ''],
            ['Reservenrate', f"{cfg.GENERAL_RESERVE_RATE*100:.1f}%"],
            ['Simulationshorizont', f"{cfg.T_HORIZON} Jahre"],
            ['', ''],
        
            ['Erwartete Renditen (mu)', ''],
            ['mu_Basiszins', f"{cfg.MU[0]*100:.2f}%"],
            ['mu_GovBonds', f"{cfg.MU[1]*100:.2f}%"],
            ['mu_CorpBonds', f"{cfg.MU[2]*100:.2f}%"],
            ['mu_Equities', f"{cfg.MU[3]*100:.2f}%"],
            ['mu_RealEstate', f"{cfg.MU[4]*100:.2f}%"],
            ['mu_Alternatives', f"{cfg.MU[5]*100:.2f}%"],
            ['', ''],
        
            ['Volatilitäten (sigma)', ''],
            ['sigma_Basiszins', f"{cfg.SIGMA[0]*100:.2f}%"],
            ['sigma_GovBonds', f"{cfg.SIGMA[1]*100:.2f}%"],
            ['sigma_CorpBonds', f"{cfg.SIGMA[2]*100:.2f}%"],
            ['sigma_Equities', f"{cfg.SIGMA[3]*100:.2f}%"],
            ['sigma_RealEstate', f"{cfg.SIGMA[4]*100:.2f}%"],
            ['sigma_Alternatives', f"{cfg.SIGMA[5]*100:.2f}%"],
            ['', ''],
        
            ['Zinsstruktur', ''],
            ['Yield Curve Slope', f"{cfg.YIELD_CURVE_SLOPE*10000:.0f} BP/Jahr"],
            ['Basiszins Floor', f"{cfg.BASIS_RATE_FLOOR*100:.1f}%"],
            ['Tech. Zins Duration', f"{cfg.TECHNICAL_RATE_DURATION:.0f} Jahre"],
            ['Tech. Zins Spread', f"{cfg.LIABILITY_DISCOUNT_SPREAD*10000:.0f} BP"],
            ['Tech. Zins Floor', f"{cfg.TECHNICAL_RATE_FLOOR*100:.2f}%"],
            ['', ''],
        
            ['Corporate Bonds Parameter', ''],
            ['Credit Spread', f"{cfg.CORP_BOND_CREDIT_SPREAD*10000:.0f} BP"],
            ['Default Probability', f"{cfg.CORP_BOND_DEFAULT_PROBABILITY*100:.2f}%"],
            ['Loss Given Default', f"{cfg.CORP_BOND_LOSS_GIVEN_DEFAULT*100:.0f}%"],
            ['Default Exposure', f"{cfg.CORP_BOND_DEFAULT_EXPOSURE*100:.1f}%"],
            ['', ''],
        
            ['Mean Reversion Aktien', ''],
            ['Mean Reversion aktiv', 'Ja' if getattr(cfg, 'EQUITY_MEAN_REVERSION_ENABLED', False) else 'Nein'],
            ['Langfrist-Rendite', f"{getattr(cfg, 'EQUITY_LONG_TERM_ANNUAL_RETURN', 0.05)*100:.1f}%"],
            ['Mean Reversion Stärke', f"{getattr(cfg, 'EQUITY_MEAN_REVERSION_STRENGTH', 0.10)*100:.0f}%"],
            ['Mean Reversion Schwelle', f"{getattr(cfg, 'EQUITY_MEAN_REVERSION_THRESHOLD', -0.20)*100:.0f}%"],
            ['', ''],
        
            ['Korrelationsmatrix', ''],
        ]
    
        # Korrelationsmatrix hinzufügen
        asset_names = cfg.ASSET_CLASSES
        for i in range(len(asset_names)):
            for j in range(i+1, len(asset_names)):
                config_recommendation.append([
                    f"Corr({asset_names[i]}, {asset_names[j]})", 
                    f"{cfg.CORR_MATRIX[i][j]:.2f}"
                ])
    
        config_recommendation.append(['', ''])
        config_recommendation.append(['EMPFOHLENE CONFIG-ÄNDERUNGEN (Copy-Paste)', ''])
        config_recommendation.append(['WEIGHTS', f"[0.00, {best_params['w_gov_bonds']:.2f}, {best_params['w_corp_bonds']:.2f}, {best_params['w_equities']:.2f}, {best_params['w_realestate']:.2f}, {best_params['w_alternatives']:.2f}]"])
        config_recommendation.append(['INITIAL_GOV_BOND_DURATION', f"{best_params['gov_bond_duration']:.1f}"])
        config_recommendation.append(['INITIAL_CORP_BOND_DURATION', f"{best_params['corp_bond_duration']:.1f}"])
    
        # Speichern der Empfehlungen
        config_df = pd.DataFrame(config_recommendation, columns=['Parameter', 'Wert'])
        config_filename = f"data/optimization_recommendation_{timestamp_str}.csv"
        config_df.to_csv(config_filename, index=False, sep=';')
        print(f"\n  Empfehlungen gespeichert in: {config_filename}")
    
        # Detaillierte Ergebnisse speichern (nur für grid/random)
        if args.method in ['grid', 'random']:
            results_filename = f"data/optimization_results_{args.method}_{timestamp_str}.csv"
            results_df.to_csv(results_filename, index=False, sep=';')
            print(f"  Alle Iterationen gespeichert in: {results_filename}")



# --------------------------------------------------------------------------
# Finale Tests: alle Kombinationen ohne Default/Unterdeckung nochmals mit 500 Pfaden testen
# und nur jene mit Zielfunktion ~= 0 als CSV speichern.
# --------------------------------------------------------------------------


if __name__ == '__main__':
    mp.freeze_support()
    main()
