import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os
import time
import multiprocessing as mp
from datetime import datetime
import json

# --- IMPORT DER KONFIGURATION ---
try:
    import config_alm as cfg
except ImportError:
    print("FEHLER: Konfigurationsdatei 'config_alm.py' nicht gefunden.")
    exit()

# ==============================================================================
# GLOBALE STEUERUNG: HIER UMSCHALTEN FÜR DEBUGGING
# ==============================================================================
# True: Führt NUR einen einzigen, detaillierten Debug-Pfad aus.
# False: Führt die volle, parallele Monte-Carlo-Simulation aus (Standard).
DEBUG_MODE_SINGLE_PATH = False  # <--- True für debugging, nur ein Pfad

# Flag um zu erkennen ob das Skript direkt ausgeführt wird
_RUNNING_AS_MAIN = False
# ==============================================================================

# Ableitung globaler Konstanten aus der Konfiguration für das Skript
N_ASSETS = cfg.N_ASSETS
CORR_MATRIX_NP = np.array(cfg.CORR_MATRIX)
# Berechnung der Cholesky-Zerlegung, um korrelierte Normalverteilungen zu erzeugen
LOWER_TRIANGLE = np.linalg.cholesky(CORR_MATRIX_NP)
EXPECTED_BASIS_RATE = cfg.MU[0]


# ==============================================================================
# 1. HILFSFUNKTIONEN
# ==============================================================================

def load_data(survival_table_path, initial_population_path):
    """Lädt und bereitet die Daten vor."""
    print("  > Lade Sterbetafel...")
    try:
        survival_table = pd.read_csv(survival_table_path)
        survival_table.set_index(['Age', 'Gender'], inplace=True)
    except FileNotFoundError:
        print(f"FEHLER: Sterbetafel nicht gefunden unter {survival_table_path}")
        return None, None
    
    print("  > Lade Rentnerbestand...")
    try:
        population = pd.read_csv(initial_population_path)
    except FileNotFoundError:
        print(f"FEHLER: Rentnerbestand nicht gefunden unter {initial_population_path}")
        return None, None

    # Vorbereitung der initialen Population
    # SpouseAgeDiff: Altersdifferenz zum Ehepartner (negativ = Partner jünger)
    # Default: Mann 3 Jahre älter als Frau
    if 'SpouseAgeDiff' in population.columns:
        # Verwende Werte aus CSV, fülle fehlende mit Default
        default_diff = np.where(population['Gender'] == 'M', -3, 3)
        population['SpouseAgeDiff'] = population['SpouseAgeDiff'].fillna(
            pd.Series(default_diff, index=population.index)
        )
    else:
        population['SpouseAgeDiff'] = np.where(population['Gender'] == 'M', -3, 3)
    
    # SpousePensionRate: Ehegattenrente in % der Rente des Partners
    # Default: 40% (0.40)
    if 'SpousePensionRate' in population.columns:
        population['SpousePensionRate'] = population['SpousePensionRate'].fillna(0.40)
    else:
        population['SpousePensionRate'] = 0.40
    
    population['SpouseInitialAge'] = population['Age'] + population['SpouseAgeDiff']
    population['Status'] = 'Active'
    population['YearOfDeath'] = 0
    population['CurrentPension'] = population['InitialPension']
    population['Age'] = population['Age'].astype(int)

    # SpouseAlive: Zustandsvariable, ob der Ehepartner (noch) lebt.
    # Nur verheiratete Aktive haben zu Beginn einen lebenden Partner.
    if 'MaritalStatus' in population.columns:
        population['SpouseAlive'] = (population['MaritalStatus'] == 'Married')
    else:
        population['SpouseAlive'] = False

    return survival_table, population



def compute_initial_population_stats(initial_population_df, W0):
    """
    Kennzahlen des ursprünglichen Rentnerbestands für den CSV-Export.
    Erwartete Spalten: Age, Gender, InitialPension; optional: MaritalStatus.
    Robust gegen fehlende Spalten (liefert dann 0/NaN-sichere Defaults).
    """
    pop = initial_population_df.copy()

    n_total = int(len(pop))
    avg_age = float(pop['Age'].mean()) if n_total > 0 and 'Age' in pop.columns else 0.0

    # Geschlecht-Anteile
    if 'Gender' in pop.columns:
        gender_counts = pop['Gender'].value_counts(dropna=False)
        n_m = int(gender_counts.get('M', 0))
        n_f = int(gender_counts.get('F', 0))
    else:
        n_m = 0
        n_f = 0
    share_m = (n_m / n_total) if n_total > 0 else 0.0
    share_f = (n_f / n_total) if n_total > 0 else 0.0

    # Anteil verheiratet (wenn vorhanden)
    if 'MaritalStatus' in pop.columns:
        n_married = int((pop['MaritalStatus'] == 'Married').sum())
        share_married = (n_married / n_total) if n_total > 0 else 0.0
    else:
        n_married = 0
        share_married = 0.0

    # Rentenverteilung (InitialPension)
    pension_col = 'InitialPension' if 'InitialPension' in pop.columns else None
    if pension_col and n_total > 0:
        pens = pop[pension_col].astype(float)
        total_pension = float(pens.sum())
        pension_stats = {
            'pension_min': float(pens.min()),
            'pension_p10': float(pens.quantile(0.10)),
            'pension_p25': float(pens.quantile(0.25)),
            'pension_median': float(pens.median()),
            'pension_mean': float(pens.mean()),
            'pension_std': float(pens.std()),
            'pension_p75': float(pens.quantile(0.75)),
            'pension_p90': float(pens.quantile(0.90)),
            'pension_max': float(pens.max()),
        }
    else:
        total_pension = 0.0
        pension_stats = {k: 0.0 for k in [
            'pension_min','pension_p10','pension_p25','pension_median','pension_mean','pension_std','pension_p75','pension_p90','pension_max'
        ]}

    # Anteil Ledige (Komplement zu verheiratet)
    share_single = 1.0 - share_married

    # Ehegatten-Parameter
    if 'SpouseAgeDiff' in pop.columns and n_total > 0:
        sad = pop['SpouseAgeDiff'].astype(float)
        spouse_age_diff_mean = float(sad.mean())
        spouse_age_diff_std = float(sad.std())
    else:
        spouse_age_diff_mean = 0.0
        spouse_age_diff_std = 0.0

    if 'SpousePensionRate' in pop.columns and n_total > 0:
        spouse_pension_rate_mean = float(pop['SpousePensionRate'].astype(float).mean())
    else:
        spouse_pension_rate_mean = 0.0

    return {
        'n_total': n_total,
        'avg_age': avg_age,
        'n_m': n_m,
        'n_f': n_f,
        'share_m': share_m,
        'share_f': share_f,
        'n_married': n_married,
        'share_married': share_married,
        'share_single': share_single,
        'total_initial_pension': total_pension,
        **pension_stats,
        'W0': float(W0),
        'spouse_age_diff_mean': spouse_age_diff_mean,
        'spouse_age_diff_std': spouse_age_diff_std,
        'spouse_pension_rate': spouse_pension_rate_mean,
    }

def get_qx(age, gender, survival_table):
    """Gibt die Sterbewahrscheinlichkeit q_x zurück."""
    max_age = 110
    if age > max_age or age < 0:
        return 1.0
    try:
        return survival_table.loc[(age, gender), 'qx']
    except KeyError:
        return 1.0


def precompute_mortality_arrays(survival_table, max_age=110):
    """
    Vorberechnung der Sterbetafeln als NumPy-Arrays für schnellen Zugriff.
    Returns: dict mit qx_M[age] und qx_F[age]
    """
    qx_M = np.ones(max_age + 2)
    qx_F = np.ones(max_age + 2)
    
    for age in range(0, max_age + 1):
        try:
            qx_M[age] = survival_table.loc[(age, 'M'), 'qx']
        except KeyError:
            qx_M[age] = 1.0
        try:
            qx_F[age] = survival_table.loc[(age, 'F'), 'qx']
        except KeyError:
            qx_F[age] = 1.0
    
    return {'M': qx_M, 'F': qx_F}


def precompute_annuity_factors(qx_arrays, tech_rate, max_age=110):
    """
    Vorberechnung der Leibrentenbarwertfaktoren ä_x für alle Alter und Geschlechter.
    ä_x = Σ(k=1..n) [p_x(k) × v^k]
    """
    v = 1.0 / (1.0 + tech_rate)
    annuity_factors = {'M': np.zeros(max_age + 2), 'F': np.zeros(max_age + 2)}
    
    for gender in ['M', 'F']:
        qx = qx_arrays[gender]
        annuity_factors[gender][max_age + 1] = 0.0
        annuity_factors[gender][max_age] = 0.0
        
        for age in range(max_age - 1, -1, -1):
            px = 1.0 - qx[age]
            annuity_factors[gender][age] = v * px * (1.0 + annuity_factors[gender][age + 1])
    
    return annuity_factors


def precompute_survival_probs(qx_arrays, max_age=110, max_years=80):
    """
    Vorberechnung der kumulierten Überlebenswahrscheinlichkeiten.
    survival_probs[gender][start_age, k] = P(Person überlebt k Jahre ab start_age)
    """
    survival_probs = {
        'M': np.zeros((max_age + 2, max_years + 1)),
        'F': np.zeros((max_age + 2, max_years + 1))
    }
    
    for gender in ['M', 'F']:
        qx = qx_arrays[gender]
        for start_age in range(0, max_age + 1):
            p_cumulative = 1.0
            survival_probs[gender][start_age, 0] = 1.0
            for k in range(1, max_years + 1):
                current_age = start_age + k - 1
                if current_age > max_age:
                    survival_probs[gender][start_age, k] = 0.0
                else:
                    p_cumulative *= (1.0 - qx[current_age])
                    survival_probs[gender][start_age, k] = p_cumulative
    
    return survival_probs

def calculate_expected_cashflows(population_state, survival_table, qx_arrays, T_horizon,
                                  include_spouse=True):
    """
    Berechnet die erwarteten jährlichen Cashflows basierend auf dem Rentnerbestand.
    
    Für Cash Flow Matching: Projiziert die erwarteten Rentenzahlungen pro Jahr
    unter Berücksichtigung der Sterbewahrscheinlichkeiten und ggf. Ehegattenrenten.
    
    Args:
        population_state: DataFrame mit Rentnerbestand
        survival_table: Sterbetafel
        qx_arrays: Vorberechnete Sterbewahrscheinlichkeiten
        T_horizon: Simulationshorizont
        include_spouse: Ob Ehegattenrenten-Anwartschaften einbezogen werden
    
    Returns:
        np.array der erwarteten Cashflows pro Jahr [CF_1, CF_2, ..., CF_T]
    """
    max_age = 110
    survival_probs = precompute_survival_probs(qx_arrays, max_age)
    
    expected_cf = np.zeros(T_horizon)
    
    active_pensions = population_state[population_state['Status'] != 'Dead']
    
    if len(active_pensions) == 0:
        return expected_cf
    
    ages = active_pensions['Age'].values.astype(int)
    pensions = active_pensions['CurrentPension'].values
    genders = active_pensions['Gender'].values
    statuses = active_pensions['Status'].values
    
    marital_statuses = active_pensions['MaritalStatus'].values if 'MaritalStatus' in active_pensions.columns else np.array(['Single'] * len(ages))
    spouse_age_diffs = active_pensions['SpouseAgeDiff'].values if 'SpouseAgeDiff' in active_pensions.columns else np.zeros(len(ages))
    spouse_pension_rates = active_pensions['SpousePensionRate'].values if 'SpousePensionRate' in active_pensions.columns else np.full(len(ages), 0.40)
    initial_pensions = active_pensions['InitialPension'].values if 'InitialPension' in active_pensions.columns else pensions
    spouse_alive_flags = active_pensions['SpouseAlive'].values if 'SpouseAlive' in active_pensions.columns else (marital_statuses == 'Married')

    annual_admin_fee = float(getattr(cfg, 'ADMIN_FEE_PER_PERSON', 0.0))
    
    for i in range(len(ages)):
        age_int = min(ages[i], max_age)
        if age_int < 0:
            continue
        
        annual_cf = pensions[i] + annual_admin_fee
        gender = genders[i]
        surv = survival_probs[gender]
        
        # 1. EIGENE RENTE: Erwartete Cashflows pro Jahr
        for k in range(1, min(T_horizon + 1, max_age + 1 - age_int)):
            p_survive_k = surv[age_int, k] if k < surv.shape[1] else 0.0
            expected_cf[k-1] += annual_cf * p_survive_k
        
        # 2. EHEGATTENANWARTSCHAFT (nur wenn der Partner noch lebt)
        if include_spouse and marital_statuses[i] == 'Married' and statuses[i] == 'Active' and spouse_alive_flags[i]:
            spouse_age_diff = spouse_age_diffs[i]
            if np.isnan(spouse_age_diff):
                spouse_age_diff = -3 if gender == 'M' else 3
            spouse_age_int = age_int + int(spouse_age_diff)
            spouse_gender = 'F' if gender == 'M' else 'M'
            spouse_pension_rate = spouse_pension_rates[i] if not np.isnan(spouse_pension_rates[i]) else 0.40
            spouse_pension = initial_pensions[i] * spouse_pension_rate
            
            if 0 <= spouse_age_int <= max_age:
                qx_rentner = qx_arrays[gender]
                surv_rentner = survival_probs[gender]
                surv_spouse = survival_probs[spouse_gender]
                
                rest_lifetime = min(max_age + 1 - age_int, T_horizon + 1)
                
                # Für jeden möglichen Todeszeitpunkt k des Rentners
                for k in range(0, rest_lifetime):
                    current_age_rentner = age_int + k
                    if current_age_rentner > max_age:
                        break
                    
                    # P(Rentner stirbt in Jahr k)
                    p_survive_to_k = surv_rentner[age_int, k] if k < surv_rentner.shape[1] else 0.0
                    qx_k = qx_rentner[current_age_rentner]
                    prob_death_in_k = p_survive_to_k * qx_k
                    
                    if prob_death_in_k < 1e-12:
                        continue
                    
                    # Witwe erhält Rente ab Jahr k+1
                    current_age_spouse = spouse_age_int + k
                    for j in range(1, min(max_age + 1 - current_age_spouse, T_horizon - k + 1)):
                        payment_year = k + j  # Jahr relativ zu t=0
                        if payment_year > T_horizon:
                            break

                        # P(Partner lebt in Jahr k+j) direkt ab Eintrittsalter y = k+j p_y
                        # (identisch zu k p_y · j p_{y+k}, ohne Doppelzählung / fehlenden Faktor)
                        p_spouse_survives_j = surv_spouse[spouse_age_int, k + j] if (k + j) < surv_spouse.shape[1] else 0.0

                        expected_cf[payment_year - 1] += spouse_pension * prob_death_in_k * p_spouse_survives_j
    
    return expected_cf


def calculate_cfm_tranches(expected_cashflows, bond_portfolio_value, max_duration=None):
    """
    Berechnet die Cash Flow Matching Bond-Tranchen.
    
    Jede Tranche ist ein Bond mit Duration = Jahr des Cashflows.
    Der Anteil der Tranche entspricht dem relativen Anteil des Cashflows (Barwert-gewichtet).
    
    Args:
        expected_cashflows: Array der erwarteten Cashflows pro Jahr
        bond_portfolio_value: Gesamtwert des Bond-Portfolios (in CHF)
        max_duration: Maximale Duration (Default: None = keine Begrenzung, 
                      verwendet Anzahl Jahre aus expected_cashflows)
    
    Returns:
        Dict mit:
            'durations': Array der Duration pro Tranche
            'weights': Array der relativen Gewichte pro Tranche
            'values': Array der CHF-Beträge pro Tranche
            'expected_cashflows': Die ursprünglichen erwarteten Cashflows
    """
    n_years = len(expected_cashflows)
    
    # Wenn max_duration nicht gesetzt, verwende den vollen Horizont
    if max_duration is None:
        max_duration = n_years
    
    # Initialisiere Arrays
    durations = np.zeros(n_years)
    weights = np.zeros(n_years)
    
    # Berechne die Diskontfaktoren
    discount_rate = EXPECTED_BASIS_RATE + getattr(cfg, 'YIELD_CURVE_SLOPE', 0.001) * 10
    discount_rate = max(discount_rate, 0.01)
    
    # Berechne Barwerte der Cashflows
    pv_cashflows = np.zeros(n_years)
    for t in range(n_years):
        pv_cashflows[t] = expected_cashflows[t] / ((1 + discount_rate) ** (t + 1))
    
    total_pv = np.sum(pv_cashflows)
    
    if total_pv <= 0:
        # Fallback: Gleichgewichtete Verteilung
        return {
            'durations': np.arange(1, n_years + 1, dtype=float),
            'weights': np.ones(n_years) / n_years,
            'values': np.ones(n_years) * bond_portfolio_value / n_years,
            'expected_cashflows': expected_cashflows
        }
    
    # Berechne Gewichte basierend auf Barwerten
    weights = pv_cashflows / total_pv
    
    # Durations = Jahr der Fälligkeit (gedeckelt falls max_duration gesetzt)
    # Für echtes CFM: Tranche für Jahr t hat Duration t (Bond fällig in Jahr t)
    for t in range(n_years):
        durations[t] = min(t + 1, max_duration)
    
    # Berechne CHF-Beträge
    values = weights * bond_portfolio_value
    
    # Aggregiere sehr kleine Tranchen (< 0.5% des Portfolios)
    min_weight_threshold = 0.005
    for t in range(n_years):
        if weights[t] < min_weight_threshold and t > 0:
            weights[t-1] += weights[t]
            values[t-1] += values[t]
            weights[t] = 0
            values[t] = 0
    
    return {
        'durations': durations,
        'weights': weights,
        'values': values,
        'expected_cashflows': expected_cashflows,
        'total_pv_liabilities': total_pv
    }


def get_cfm_weighted_duration(cfm_tranches, year):
    """
    Berechnet die gewichtete durchschnittliche Duration des CFM-Portfolios
    zu einem bestimmten Zeitpunkt.
    
    Args:
        cfm_tranches: Die Tranchen-Info aus calculate_cfm_tranches
        year: Aktuelles Jahr der Simulation (0-basiert)
    
    Returns:
        Gewichtete durchschnittliche Duration der verbleibenden Tranchen
    """
    if cfm_tranches is None:
        return 0.0
    
    remaining_durations = []
    remaining_weights = []
    
    for t in range(year, len(cfm_tranches['durations'])):
        original_duration = cfm_tranches['durations'][t]
        # Restduration nach 'year' Jahren
        remaining_dur = max(0.0, original_duration - year)
        weight = cfm_tranches['weights'][t]
        
        if remaining_dur > 0 and weight > 0:
            remaining_durations.append(remaining_dur)
            remaining_weights.append(weight)
    
    if not remaining_weights:
        return 0.0
    
    total_weight = sum(remaining_weights)
    return sum(d * w for d, w in zip(remaining_durations, remaining_weights)) / total_weight


def get_cfm_coupon_rate(cfm_tranches, year, r1_t, yield_curve_slope, credit_spread=0.0):
    """
    Berechnet den gewichteten durchschnittlichen Coupon-Satz des CFM-Portfolios.
    
    Bei CFM haben verschiedene Tranchen unterschiedliche Coupons basierend auf ihrer
    ursprünglichen Duration (bei Kauf t=0 fixiert).
    
    Args:
        cfm_tranches: Die Tranchen-Info
        year: Aktuelles Jahr
        r1_t: Aktueller Basiszins (nur für Info, Coupons sind fixiert)
        yield_curve_slope: Steigung der Zinsstrukturkurve
        credit_spread: Credit Spread für Corporate Bonds
    
    Returns:
        Gewichteter durchschnittlicher Coupon-Satz
    """
    if cfm_tranches is None:
        return r1_t + credit_spread
    
    # Die Coupons wurden bei t=0 fixiert mit dem damaligen Basiszins
    initial_r1 = EXPECTED_BASIS_RATE
    
    weighted_coupon = 0.0
    total_weight = 0.0
    
    for t in range(year, len(cfm_tranches['durations'])):
        original_duration = cfm_tranches['durations'][t]
        remaining_dur = max(0.0, original_duration - year)
        weight = cfm_tranches['weights'][t]
        
        if remaining_dur > 0 and weight > 0:
            # Coupon wurde bei Kauf fixiert basierend auf ursprünglicher Duration
            original_coupon = initial_r1 + original_duration * yield_curve_slope + credit_spread
            weighted_coupon += original_coupon * weight
            total_weight += weight
    
    if total_weight > 0:
        return weighted_coupon / total_weight
    return r1_t + credit_spread


# ==============================================================================
# DURATION-MANAGEMENT HELPER
# ==============================================================================

def get_duration_for_mode(mode, current_duration, initial_duration, liability_duration,
                          reset_interval, year, is_liability_recalc_year,
                          cfm_tranches=None):
    """
    Berechnet die aktuelle Bond-Duration basierend auf dem gewählten Modus.

    Args:
        mode: "fixed", "fixed_reset", "liability_matching", oder "cashflow_matching"
        current_duration: Aktuelle Duration aus dem Vorjahr
        initial_duration: Initiale Duration (für fixed/fixed_reset)
        liability_duration: Aktuelle Liability-Duration (für liability_matching)
        reset_interval: Reset-Intervall in Jahren (für fixed_reset)
        year: Aktuelles Simulationsjahr (t)
        is_liability_recalc_year: Ob Liability-Duration in diesem Jahr neu berechnet wird
        cfm_tranches: Cash Flow Matching Tranchen-Info (für cashflow_matching Modus)
                      Dict mit 'durations' und 'weights' pro Jahr

    Returns:
        Tuple (neue_duration, duration_was_reset)
    """
    duration_was_reset = False

    if mode == "liability_matching":
        # Bei Liability Matching: Duration JEDES JAHR an Liability-Duration anpassen
        new_duration = liability_duration
        duration_was_reset = True  # Wird jedes Jahr "gematcht"

    elif mode == "cashflow_matching":
        # Bei Cash Flow Matching: Portfolio aus verschiedenen Tranchen mit dedizierter Duration
        # Die effektive Duration ist der gewichtete Durchschnitt der verbleibenden Tranchen
        # Tranchen laufen passiv ab - keine aktive Anpassung

        if cfm_tranches is not None:
            new_duration = get_cfm_weighted_duration(cfm_tranches, year + 1)  # +1 weil für nächstes Jahr
        else:
            # Fallback: Reduziere um 1 pro Jahr
            new_duration = max(0.0, current_duration - 1.0)

        # KEIN Reset bei CFM - passives Halten ist das Kernkonzept
        duration_was_reset = False

    elif mode == "fixed_reset":
        # Reset erfolgt automatisch im fixed_reset Modus
        new_duration = max(0.0, current_duration - 1.0)
        if year > 0 and ((year + 1) % reset_interval == 0):
            new_duration = initial_duration
            duration_was_reset = True

    else:  # "fixed" - kein Reset
        new_duration = max(0.0, current_duration - 1.0)

    return new_duration, duration_was_reset



# ==============================================================================
# 2. MONTE CARLO - ASSET & BARWERT
# ==============================================================================

def simulate_all_returns(mu, sigma, lower_triangle, T_horizon, degrees_of_freedom=None):
    """
    Simuliert alle korrelierten Faktoren (inkl. Zinsfaktor).
    
    Unterstützt Fat Tails via Student-t Verteilung pro Asset-Klasse.
    Die Korrelationsstruktur bleibt erhalten durch Copula-Ansatz:
    1. Generiere korrelierte Normalverteilungen
    2. Transformiere zu uniformen Verteilungen (via CDF)
    3. Transformiere zu Student-t (via inverse CDF) wo gewünscht
    
    Parameters:
    -----------
    degrees_of_freedom : list oder None
        Freiheitsgrade pro Asset. None/inf = Normalverteilung.
        Kleinere Werte = dickere Tails (mehr Extremereignisse)
    """
    from scipy import stats
    
    n_assets = len(mu)
    
    # Schritt 1: Generiere korrelierte Standard-Normalverteilungen
    Z = np.random.normal(0, 1, size=(T_horizon, n_assets))
    X_corr = Z @ lower_triangle.T  # Korrelierte N(0,1)
    
    # Schritt 2 & 3: Transformiere zu Fat Tails wo gewünscht (Gaussian Copula)
    if degrees_of_freedom is not None:
        for i in range(n_assets):
            df = degrees_of_freedom[i]
            if df is not None and df < 100:  # df >= 100 ist praktisch normal
                # Transformiere: N(0,1) → U(0,1) → t(df)
                # Die t-Verteilung wird so skaliert, dass sie Varianz 1 hat
                u = stats.norm.cdf(X_corr[:, i])  # Zu uniform
                # Skalierungsfaktor damit Var(t) = 1: sqrt((df-2)/df) für df > 2
                scale = np.sqrt((df - 2) / df) if df > 2 else 1.0
                X_corr[:, i] = stats.t.ppf(u, df) * scale
    
    # Berechne Log-Returns mit Drift-Korrektur
    # Für t-Verteilung: E[exp(σX)] ist höher, daher Korrektur anpassen
    mu_arr = np.array(mu)
    sigma_arr = np.array(sigma)
    
    log_returns = np.zeros_like(X_corr)
    for i in range(n_assets):
        # Standard Log-Normal Drift-Korrektur
        drift = mu_arr[i] - sigma_arr[i]**2 / 2
        log_returns[:, i] = drift + sigma_arr[i] * X_corr[:, i]
    
    returns = np.exp(log_returns) - 1
    return returns

def calculate_liability_barwert_base(population_state, survival_table, tech_rate, 
                                      qx_arrays=None, annuity_factors=None, survival_probs=None):
    """
    OPTIMIERTE Berechnung des Barwerts W(t) inkl. Anwartschaft auf Ehegattenrente.
    Nutzt vorberechnete Arrays für massive Geschwindigkeitssteigerung.
    """
    tech_rate = max(tech_rate, cfg.TECHNICAL_RATE_FLOOR)
    max_age = 110
    v = 1.0 / (1.0 + tech_rate)
    
    # Vorberechnung falls nicht übergeben
    if qx_arrays is None:
        qx_arrays = precompute_mortality_arrays(survival_table, max_age)
    if annuity_factors is None:
        annuity_factors = precompute_annuity_factors(qx_arrays, tech_rate, max_age)
    if survival_probs is None:
        survival_probs = precompute_survival_probs(qx_arrays, max_age)
    
    active_pensions = population_state[population_state['Status'] != 'Dead']
    
    if len(active_pensions) == 0:
        return 0.0
    
    total_liability = 0.0
    
    # Vektorisierte Extraktion
    ages = active_pensions['Age'].values.astype(int)
    pensions = active_pensions['CurrentPension'].values
    genders = active_pensions['Gender'].values
    statuses = active_pensions['Status'].values
    
    marital_statuses = active_pensions['MaritalStatus'].values if 'MaritalStatus' in active_pensions.columns else np.array(['Single'] * len(ages))
    spouse_age_diffs = active_pensions['SpouseAgeDiff'].values if 'SpouseAgeDiff' in active_pensions.columns else np.zeros(len(ages))
    spouse_pension_rates = active_pensions['SpousePensionRate'].values if 'SpousePensionRate' in active_pensions.columns else np.full(len(ages), 0.40)
    initial_pensions = active_pensions['InitialPension'].values if 'InitialPension' in active_pensions.columns else pensions
    spouse_alive_flags = active_pensions['SpouseAlive'].values if 'SpouseAlive' in active_pensions.columns else (marital_statuses == 'Married')

    for i in range(len(ages)):
        age_int = min(ages[i], max_age)
        if age_int < 0:
            continue

        annual_pension = pensions[i]
        annual_admin_fee = float(getattr(cfg, 'ADMIN_FEE_PER_PERSON', 0.0))
        gender = genders[i]
        annual_admin_fee = float(getattr(cfg, 'ADMIN_FEE_PER_PERSON', 0.0))
        
        # --- 1. BARWERT DER EIGENEN RENTE ---
        barwert_eigene_rente = annuity_factors[gender][age_int]
        total_liability += annual_pension * barwert_eigene_rente
        if annual_admin_fee:
            total_liability += annual_admin_fee * barwert_eigene_rente
        
        # --- 2. ANWARTSCHAFT EHEGATTENRENTE (nur wenn der Partner noch lebt) ---
        if marital_statuses[i] == 'Married' and statuses[i] == 'Active' and spouse_alive_flags[i]:
            spouse_age_diff = spouse_age_diffs[i]
            if np.isnan(spouse_age_diff):
                spouse_age_diff = -3 if gender == 'M' else 3
            spouse_age_int = age_int + int(spouse_age_diff)
            spouse_gender = 'F' if gender == 'M' else 'M'
            spouse_pension_rate = spouse_pension_rates[i] if not np.isnan(spouse_pension_rates[i]) else 0.40
            spouse_pension = initial_pensions[i] * spouse_pension_rate
            
            if spouse_age_int < 0 or spouse_age_int > max_age:
                continue
            
            qx_rentner = qx_arrays[gender]
            surv_rentner = survival_probs[gender]
            surv_spouse = survival_probs[spouse_gender]
            annuity_spouse = annuity_factors[spouse_gender]
            
            barwert_anwartschaft = 0.0
            rest_lifetime = min(max_age + 1 - age_int, 80)
            
            for k in range(0, rest_lifetime):
                current_age_rentner = age_int + k
                current_age_spouse = spouse_age_int + k
                
                if current_age_spouse > max_age or current_age_rentner > max_age:
                    break
                
                # P(Rentner stirbt in Jahr k)
                p_survive_to_k = surv_rentner[age_int, k] if k < surv_rentner.shape[1] else 0.0
                qx_k = qx_rentner[current_age_rentner]
                prob_death_in_k = p_survive_to_k * qx_k
                
                if prob_death_in_k < 1e-12:
                    continue
                
                # P(Ehepartner lebt in Jahr k) = k p_y ab Eintrittsalter y.
                # Zusammen mit ä_{y+k} (zahlt ab j=1) ergibt sich k+j p_y = k p_y · j p_{y+k}.
                p_spouse_k = surv_spouse[spouse_age_int, k] if k < surv_spouse.shape[1] else 0.0
                
                if p_spouse_k < 1e-12:
                    continue
                
                # Barwertfaktor ab Jahr k (diskontiert)
                annuity_from_k = (v ** k) * annuity_spouse[current_age_spouse]
                barwert_anwartschaft += prob_death_in_k * p_spouse_k * annuity_from_k
            
            total_liability += spouse_pension * barwert_anwartschaft
    
    return total_liability


def calculate_liability_duration(population_state, survival_table, tech_rate,
                                  qx_arrays=None, survival_probs=None):
    """
    Berechnet die Macaulay-Duration der Liabilities, GEWICHTET NACH BARWERT.
    
    WICHTIG: Berücksichtigt auch die Ehegattenanwartschaft, da diese in W(t)
    enthalten ist und für korrektes LDI/Duration-Matching relevant ist.
    
    Für jeden Versicherten i:
      - Barwert_i = BW_eigene_Rente + BW_Anwartschaft_Ehegattenrente
      - Duration_i = (Σ k × diskontierte_CF) / Barwert_i
    
    Gesamt-Duration = Σ(Barwert_i × Duration_i) / Σ(Barwert_i)
    """
    tech_rate = max(tech_rate, cfg.TECHNICAL_RATE_FLOOR)
    max_age = 110
    v = 1.0 / (1.0 + tech_rate)
    
    if qx_arrays is None:
        qx_arrays = precompute_mortality_arrays(survival_table, max_age)
    if survival_probs is None:
        survival_probs = precompute_survival_probs(qx_arrays, max_age)
    
    active_pensions = population_state[population_state['Status'] != 'Dead']
    
    if len(active_pensions) == 0:
        return 0.0
    
    ages = active_pensions['Age'].values.astype(int)
    pensions = active_pensions['CurrentPension'].values
    genders = active_pensions['Gender'].values
    statuses = active_pensions['Status'].values
    annual_admin_fee = float(getattr(cfg, 'ADMIN_FEE_PER_PERSON', 0.0))
    
    # Für Ehegattenoption
    marital_statuses = active_pensions['MaritalStatus'].values if 'MaritalStatus' in active_pensions.columns else np.array(['Single'] * len(ages))
    spouse_age_diffs = active_pensions['SpouseAgeDiff'].values if 'SpouseAgeDiff' in active_pensions.columns else np.zeros(len(ages))
    spouse_pension_rates = active_pensions['SpousePensionRate'].values if 'SpousePensionRate' in active_pensions.columns else np.full(len(ages), 0.40)
    initial_pensions = active_pensions['InitialPension'].values if 'InitialPension' in active_pensions.columns else pensions
    spouse_alive_flags = active_pensions['SpouseAlive'].values if 'SpouseAlive' in active_pensions.columns else (marital_statuses == 'Married')

    # Summiere: Σ(Barwert_i × Duration_i) und Σ(Barwert_i)
    sum_pv_times_duration = 0.0
    sum_pv = 0.0
    
    for i in range(len(ages)):
        age_int = min(ages[i], max_age)
        if age_int < 0:
            continue
        
        annual_cf = pensions[i] + annual_admin_fee
        gender = genders[i]
        surv = survival_probs[gender]
        
        rest_lifetime = min(max_age + 1 - age_int, 80)
        
        # === 1. EIGENE RENTE: Barwert und gewichtete Zeit ===
        pv_eigene = 0.0
        weighted_time_eigene = 0.0
        
        for k in range(1, rest_lifetime):
            p_survive_k = surv[age_int, k] if k < surv.shape[1] else 0.0
            discounted_cf = annual_cf * p_survive_k * (v ** k)
            
            pv_eigene += discounted_cf
            weighted_time_eigene += k * discounted_cf
        
        # === 2. EHEGATTENANWARTSCHAFT: Barwert und gewichtete Zeit ===
        pv_spouse = 0.0
        weighted_time_spouse = 0.0
        
        if marital_statuses[i] == 'Married' and statuses[i] == 'Active' and spouse_alive_flags[i]:
            spouse_age_diff = spouse_age_diffs[i]
            if np.isnan(spouse_age_diff):
                spouse_age_diff = -3 if gender == 'M' else 3
            spouse_age_int = age_int + int(spouse_age_diff)
            spouse_gender = 'F' if gender == 'M' else 'M'
            spouse_pension_rate = spouse_pension_rates[i] if not np.isnan(spouse_pension_rates[i]) else 0.40
            spouse_pension = initial_pensions[i] * spouse_pension_rate

            if 0 <= spouse_age_int <= max_age:
                qx_rentner = qx_arrays[gender]
                surv_rentner = survival_probs[gender]
                surv_spouse = survival_probs[spouse_gender]
                
                spouse_rest_lifetime = min(max_age + 1 - spouse_age_int, 80)
                
                # Für jeden möglichen Todeszeitpunkt k des Rentners
                for k in range(0, rest_lifetime):
                    current_age_rentner = age_int + k
                    current_age_spouse = spouse_age_int + k
                    
                    if current_age_spouse > max_age or current_age_rentner > max_age:
                        break
                    
                    # P(Rentner stirbt in Jahr k)
                    p_survive_to_k = surv_rentner[age_int, k] if k < surv_rentner.shape[1] else 0.0
                    qx_k = qx_rentner[current_age_rentner]
                    prob_death_in_k = p_survive_to_k * qx_k
                    
                    if prob_death_in_k < 1e-12:
                        continue
                    
                    # Cashflows der Witwe nach Tod in Jahr k
                    # Witwe erhält Rente ab Jahr k+1 bis zu ihrem Tod
                    for j in range(1, min(max_age + 1 - current_age_spouse, 80)):
                        spouse_age_at_j = current_age_spouse + j
                        if spouse_age_at_j > max_age:
                            break
                        
                        # P(Partner lebt in Jahr k+j) direkt ab Eintrittsalter y = k+j p_y
                        # (identisch zu k p_y · j p_{y+k}, vermeidet Doppelzählung von vornherein)
                        p_spouse_survives_j = surv_spouse[spouse_age_int, k + j] if (k + j) < surv_spouse.shape[1] else 0.0
                        
                        # Zeitpunkt der Zahlung: k + j Jahre ab heute
                        payment_time = k + j
                        discounted_cf = spouse_pension * prob_death_in_k * p_spouse_survives_j * (v ** payment_time)
                        
                        pv_spouse += discounted_cf
                        weighted_time_spouse += payment_time * discounted_cf
        
        # === GESAMTE DURATION für diesen Versicherten ===
        pv_total_i = pv_eigene + pv_spouse
        weighted_time_total_i = weighted_time_eigene + weighted_time_spouse
        
        if pv_total_i > 0:
            duration_i = weighted_time_total_i / pv_total_i
            sum_pv_times_duration += pv_total_i * duration_i
            sum_pv += pv_total_i
    
    if sum_pv > 0:
        return sum_pv_times_duration / sum_pv
    return 0.0


def calculate_liability_barwert_initial(initial_population, survival_table):
    """
    Berechnet den initialen Barwert W(0) mit dem erwarteten Zins, 
    der sich aus r1, der Tech-Duration und dem Spread ergibt.
    """
    initial_tech_rate = EXPECTED_BASIS_RATE + cfg.TECHNICAL_RATE_DURATION * cfg.YIELD_CURVE_SLOPE + cfg.LIABILITY_DISCOUNT_SPREAD
    return calculate_liability_barwert_base(initial_population, survival_table, initial_tech_rate)


# ==============================================================================
# 3. KERN-SIMULATIONSFUNKTION (EIN PFAD)
# ==============================================================================

def run_monte_carlo_path_full(args):
    """
    Führt EINEN Monte-Carlo-Pfad durch, berücksichtigt Zinsstruktur, 
    Duration-Matching und sammelt Renditen/Cashflows.
    """
    # Unterstütze beide Formate: mit und ohne path_idx
    if len(args) == 6:
        initial_population, survival_table, T_horizon, V0, qx_arrays_precomputed, path_idx = args
        # Setze einen einzigartigen Seed basierend auf Pfad-Index und aktueller Zeit
        np.random.seed(path_idx * 1000 + int(time.time() * 1000) % 100000)
    else:
        initial_population, survival_table, T_horizon, V0, qx_arrays_precomputed = args
        # Fallback: zufälliger Seed
        np.random.seed(None)
    
    population_state = initial_population.copy()

    # SpouseAlive-Zustand sicherstellen (falls die Population nicht über load_data kam,
    # z.B. aus externen Aufrufen). Garantiert saubere boolesche Werte im gesamten Pfad.
    if 'SpouseAlive' not in population_state.columns:
        if 'MaritalStatus' in population_state.columns:
            population_state['SpouseAlive'] = (population_state['MaritalStatus'] == 'Married')
        else:
            population_state['SpouseAlive'] = False

    results_path = {
        'deckungsgrad': np.zeros(T_horizon),
        'portfolio_return': np.zeros(T_horizon),
        'cashflow_rent': np.zeros(T_horizon),
        'cashflow_admin_fee': np.zeros(T_horizon),
        'cashflow_total': np.zeros(T_horizon),
        'special_pension_payout': np.zeros(T_horizon),  # Sonder-Rente Auszahlungen
        # Detail-Renditen für Assets
        'gov_bonds_return': np.zeros(T_horizon),
        'corp_bonds_return': np.zeros(T_horizon),
        'equities_return': np.zeros(T_horizon),
        'realestate_return': np.zeros(T_horizon),
        'alternatives_return': np.zeros(T_horizon),
        # Infrastructure Debt (Alternatives) Bond-Komponenten
        'alt_bonds_coupon': np.zeros(T_horizon),
        'alt_bonds_duration_effect': np.zeros(T_horizon),
        'alt_bonds_default_loss': np.zeros(T_horizon),
        'alt_bonds_market_value': np.zeros(T_horizon),
        'alt_bonds_fixed_coupon': np.zeros(T_horizon),
        'alt_bond_duration': np.zeros(T_horizon),
        'i_alt_bonds_t': np.zeros(T_horizon),
        # Government Bond-Komponenten
        'gov_bonds_coupon': np.zeros(T_horizon),
        'gov_bonds_duration_effect': np.zeros(T_horizon),
        'gov_bonds_market_value': np.zeros(T_horizon),    # NEU: Marktwert relativ zu Par
        'gov_bonds_fixed_coupon': np.zeros(T_horizon),    # NEU: Fixierter Coupon-Satz
        # Corporate Bond-Komponenten
        'corp_bonds_coupon': np.zeros(T_horizon),
        'corp_bonds_duration_effect': np.zeros(T_horizon),
        'corp_bonds_default_loss': np.zeros(T_horizon),
        'corp_bonds_market_value': np.zeros(T_horizon),   # NEU: Marktwert relativ zu Par
        'corp_bonds_fixed_coupon': np.zeros(T_horizon),   # NEU: Fixierter Coupon-Satz
        # Zins & Demographie
        'r1_t': np.zeros(T_horizon),  # Basiszins (nach Floor)
        'i_gov_bonds_t': np.zeros(T_horizon),
        'i_corp_bonds_t': np.zeros(T_horizon),
        'i_tech_t': np.zeros(T_horizon),
        'W_t': np.zeros(T_horizon),
        'V_t': np.zeros(T_horizon),
        'liability_duration': np.zeros(T_horizon),
        'gov_bond_duration': np.zeros(T_horizon),
        'corp_bond_duration': np.zeros(T_horizon),
        'num_pensioners': np.zeros(T_horizon),
        'num_widows': np.zeros(T_horizon),
        # Interest Rate Cap
        'interest_rate_cap_payout': np.zeros(T_horizon),  # Auszahlung aus dem Cap
        'interest_rate_cap_active': np.zeros(T_horizon),  # 1 wenn Cap aktiv und im Geld
    }
    
    # Hole Fat-Tail Parameter falls vorhanden
    degrees_of_freedom = getattr(cfg, 'DEGREES_OF_FREEDOM', None)
    
    all_returns = simulate_all_returns(cfg.MU, cfg.SIGMA, LOWER_TRIANGLE, T_horizon, degrees_of_freedom)
    
    r1_t_minus_1 = EXPECTED_BASIS_RATE
    
    # Vorberechnete Mortality-Arrays (übergeben oder neu berechnen)
    qx_arrays = qx_arrays_precomputed
    
    # Cache für Annuity-Faktoren pro Zinssatz (wird bei Bedarf aktualisiert)
    cached_tech_rate = None
    cached_annuity_factors = None
    cached_survival_probs = precompute_survival_probs(qx_arrays)
    
    # Duration-Management für beide Bond-Typen
    current_gov_bond_duration = getattr(cfg, 'INITIAL_GOV_BOND_DURATION', cfg.INITIAL_BOND_DURATION)
    current_corp_bond_duration = getattr(cfg, 'INITIAL_CORP_BOND_DURATION', 5.0)
    current_alt_bond_duration = getattr(cfg, 'INITIAL_ALT_BOND_DURATION', 30.0)

    # Duration-Modi aus Config
    gov_duration_mode = getattr(cfg, 'GOV_BOND_DURATION_MODE', 'fixed_reset')
    corp_duration_mode = getattr(cfg, 'CORP_BOND_DURATION_MODE', 'fixed_reset')
    alt_duration_mode = getattr(cfg, 'ALT_BOND_DURATION_MODE', 'fixed')
    
    # === NEU: CASH FLOW MATCHING INITIALISIERUNG ===
    gov_cfm_tranches = None
    corp_cfm_tranches = None
    alt_cfm_tranches = None

    if gov_duration_mode == "cashflow_matching" or corp_duration_mode == "cashflow_matching" or alt_duration_mode == "cashflow_matching":
        # Berechne erwartete Cashflows EINMALIG zu Beginn
        expected_cfs = calculate_expected_cashflows(
            population_state, survival_table, qx_arrays, T_horizon,
            include_spouse=True
        )
        
        # Government Bonds CFM Tranchen
        if gov_duration_mode == "cashflow_matching":
            gov_bond_value = V0 * cfg.WEIGHTS[1]  # GovBonds Allokation
            gov_cfm_tranches = calculate_cfm_tranches(expected_cfs, gov_bond_value)
            current_gov_bond_duration = get_cfm_weighted_duration(gov_cfm_tranches, 0)
        
        # Corporate Bonds CFM Tranchen
        if corp_duration_mode == "cashflow_matching":
            corp_bond_value = V0 * cfg.WEIGHTS[2]  # CorpBonds Allokation
            corp_cfm_tranches = calculate_cfm_tranches(expected_cfs, corp_bond_value)
            current_corp_bond_duration = get_cfm_weighted_duration(corp_cfm_tranches, 0)

        # Infrastructure Debt (Alternatives) CFM Tranchen
        if alt_duration_mode == "cashflow_matching":
            alt_bond_value = V0 * cfg.WEIGHTS[5]  # Alternatives Allokation
            alt_cfm_tranches = calculate_cfm_tranches(expected_cfs, alt_bond_value)
            current_alt_bond_duration = get_cfm_weighted_duration(alt_cfm_tranches, 0)

    # Reset-Intervall (nur relevant für fixed_reset Modus)
    gov_reset_interval = getattr(cfg, 'GOV_BOND_DURATION_RESET_INTERVAL', cfg.DURATION_RESET_INTERVAL)
    corp_reset_interval = getattr(cfg, 'CORP_BOND_DURATION_RESET_INTERVAL', 5)
    alt_reset_interval = getattr(cfg, 'ALT_BOND_DURATION_RESET_INTERVAL', 5)
    
    # Corporate Bond Parameter
    corp_credit_spread = getattr(cfg, 'CORP_BOND_CREDIT_SPREAD', 0.01)
    corp_default_prob = getattr(cfg, 'CORP_BOND_DEFAULT_PROBABILITY', 0.003)
    corp_lgd = getattr(cfg, 'CORP_BOND_LOSS_GIVEN_DEFAULT', 0.40)
    corp_default_exposure = getattr(cfg, 'CORP_BOND_DEFAULT_EXPOSURE', 0.02)

    # Infrastructure Debt (Alternatives) Bond Parameter
    alt_credit_spread = getattr(cfg, 'ALT_BOND_CREDIT_SPREAD', 0.015)
    alt_default_prob = getattr(cfg, 'ALT_BOND_DEFAULT_PROBABILITY', 0.013)
    alt_lgd = getattr(cfg, 'ALT_BOND_LOSS_GIVEN_DEFAULT', 0.35)
    alt_default_exposure = getattr(cfg, 'ALT_BOND_DEFAULT_EXPOSURE', 0.02)
    
    # === INTEREST RATE CAP PARAMETER ===
    # Der Cap schützt gegen steigende Zinsen im ersten Jahr
    # Notional = Allokation in CHF Bonds (Gov + Corp)
    interest_rate_cap_enabled = getattr(cfg, 'INTEREST_RATE_CAP_ENABLED', False)
    interest_rate_cap_strike = getattr(cfg, 'INTEREST_RATE_CAP_STRIKE', 0.0075)  # Strike über Basiszins (z.B. 75bp)
    interest_rate_cap_premium = getattr(cfg, 'INTEREST_RATE_CAP_PREMIUM', 0.002)  # Prämie in % des Notionals
    interest_rate_cap_duration = getattr(cfg, 'INTEREST_RATE_CAP_DURATION', 1)  # Laufzeit in Jahren
    
    # Notional für den Cap: Bond-Allokation * Anfangsvermögen
    bond_allocation = cfg.WEIGHTS[1] + cfg.WEIGHTS[2]  # GovBonds + CorpBonds
    cap_notional = V0 * bond_allocation
    
    # Cap-Prämie wird am Anfang bezahlt (reduziert V0)
    cap_premium_paid = 0.0
    if interest_rate_cap_enabled and cap_notional > 0:
        cap_premium_paid = cap_notional * interest_rate_cap_premium
        V_t_minus_1 = V0 - cap_premium_paid  # Prämie reduziert Startkapital
    else:
        V_t_minus_1 = V0
    
    # Strike-Level: Basiszins bei t=0 + Strike-Aufschlag
    cap_strike_level = EXPECTED_BASIS_RATE + interest_rate_cap_strike

    # === SAMMELSTIFTUNG-MODUS PARAMETER ===
    # Im Sammelstiftung-Modus wird alle X Jahre ein neuer Rentnerbestand hinzugefügt
    sammelstiftung_enabled = getattr(cfg, 'SAMMELSTIFTUNG_ENABLED', False)
    sammelstiftung_interval = getattr(cfg, 'SAMMELSTIFTUNG_INTERVAL', 5)

    # Speichere den initialen Bestand für Sammelstiftung (Kopie des Originals ohne Statusänderungen)
    initial_population_for_sammelstiftung = None
    if sammelstiftung_enabled:
        # Speichere Kopie des initialen Bestands für spätere Hinzufügungen
        # Wir erstellen eine frische Kopie ohne die Status-Änderungen
        initial_population_for_sammelstiftung = initial_population.copy()
        # Setze Status zurück auf Initial-Werte
        initial_population_for_sammelstiftung['Status'] = 'Active'
        initial_population_for_sammelstiftung['YearOfDeath'] = 0

    # === NEU: Realistisches Bond-Modell State-Variablen ===
    # Coupon wird bei Kauf/Reset fixiert und bleibt konstant bis zur nächsten Neuanlage
    initial_gov_rate = EXPECTED_BASIS_RATE + current_gov_bond_duration * cfg.YIELD_CURVE_SLOPE
    initial_corp_rate = EXPECTED_BASIS_RATE + current_corp_bond_duration * cfg.YIELD_CURVE_SLOPE + corp_credit_spread
    initial_alt_rate = EXPECTED_BASIS_RATE + current_alt_bond_duration * cfg.YIELD_CURVE_SLOPE + alt_credit_spread
    
    # Bei CFM: Gewichteter durchschnittlicher Coupon der Tranchen
    if gov_duration_mode == "cashflow_matching" and gov_cfm_tranches is not None:
        gov_bond_fixed_coupon = get_cfm_coupon_rate(gov_cfm_tranches, 0, EXPECTED_BASIS_RATE, 
                                                     cfg.YIELD_CURVE_SLOPE, 0.0)
    else:
        gov_bond_fixed_coupon = initial_gov_rate
    
    if corp_duration_mode == "cashflow_matching" and corp_cfm_tranches is not None:
        corp_bond_fixed_coupon = get_cfm_coupon_rate(corp_cfm_tranches, 0, EXPECTED_BASIS_RATE,
                                                      cfg.YIELD_CURVE_SLOPE, corp_credit_spread)
    else:
        corp_bond_fixed_coupon = initial_corp_rate

    if alt_duration_mode == "cashflow_matching" and alt_cfm_tranches is not None:
        alt_bond_fixed_coupon = get_cfm_coupon_rate(alt_cfm_tranches, 0, EXPECTED_BASIS_RATE,
                                                     cfg.YIELD_CURVE_SLOPE, alt_credit_spread)
    else:
        alt_bond_fixed_coupon = initial_alt_rate

    gov_bond_market_value = 1.0    # Marktwert relativ zu Par (startet bei 100%)
    corp_bond_market_value = 1.0   # Marktwert relativ zu Par (startet bei 100%)
    alt_bond_market_value = 1.0    # Marktwert relativ zu Par (startet bei 100%)

    gov_bond_initial_duration = current_gov_bond_duration   # Duration bei Kauf (für Pull-to-Par)
    corp_bond_initial_duration = current_corp_bond_duration # Duration bei Kauf (für Pull-to-Par)
    alt_bond_initial_duration = current_alt_bond_duration   # Duration bei Kauf (für Pull-to-Par)
    
    # Index-Mapping für die neue Asset-Struktur
    # [InterestRate, GovBonds, CorpBonds, Equities, RealEstate, Alternatives]
    idx_basisrate = 0
    idx_gov_bonds = 1
    idx_corp_bonds = 2
    idx_equities = 3
    idx_realestate = 4
    idx_alternatives = 5
    
    for t in range(T_horizon):
        
        simulated_returns_t = all_returns[t, :]
        r1_t_raw = simulated_returns_t[idx_basisrate]  # Simulierter Basiszinssatz
        
        # Floor für Basiszins: Nicht unter -1% (konfigurierbar)
        basis_rate_floor = getattr(cfg, 'BASIS_RATE_FLOOR', -0.01)
        r1_t = max(r1_t_raw, basis_rate_floor)
        
        # Zinsänderung im Basisszins (für beide Bond-Typen)
        delta_r1 = r1_t - r1_t_minus_1
        
        # === A1. STAATSANLEIHEN (Government Bonds) - REALISTISCHES MODELL ===
        # 
        # Komponenten der Bond-Rendite:
        # 1. Coupon: Fixiert bei Kauf, bezogen auf Par-Wert (nicht Marktwert)
        # 2. Duration-Effekt: Kursänderung durch Zinsänderung, wirkt auf Marktwert
        # 3. Pull-to-Par: Bond konvergiert gegen 100% bei Fälligkeit
        #
        # Wichtig: Coupon wird auf Par bezahlt, aber Rendite bezieht sich auf Marktwert
        
        i_gov_bonds_t = r1_t + current_gov_bond_duration * cfg.YIELD_CURVE_SLOPE
        results_path['r1_t'][t] = r1_t
        results_path['i_gov_bonds_t'][t] = i_gov_bonds_t
        
        r_GovBonds_t = 0.0
        gov_duration_effect = 0.0
        gov_pull_to_par = 0.0
        
        if current_gov_bond_duration > 0:
            # 1. COUPON-RENDITE (bezogen auf aktuellen Marktwert)
            # Coupon ist fix, aber relative Rendite hängt vom Marktwert ab
            # Wenn Bond bei 90% steht, ist die Coupon-Rendite höher (Coupon/0.9)
            gov_coupon_return = gov_bond_fixed_coupon / gov_bond_market_value
            
            # 2. DURATION-EFFEKT (Kursänderung durch Zinsänderung)
            if abs(delta_r1) > 1e-10:
                gov_convexity = (current_gov_bond_duration**2 + current_gov_bond_duration) / ((1 + i_gov_bonds_t)**2)
                gov_duration_component = -current_gov_bond_duration * delta_r1 / (1 + i_gov_bonds_t)
                gov_convexity_component = 0.5 * gov_convexity * (delta_r1 ** 2)
                gov_duration_effect = gov_duration_component + gov_convexity_component
                gov_duration_effect = max(-0.50, min(gov_duration_effect, 0.50))  # Realistischere Grenzen
            
            # 3. PULL-TO-PAR EFFEKT (korrigierte, rein additive Form)
            # 
            # Der Bond konvergiert gegen Par (100%) bei Fälligkeit.
            # Wir berechnen den jährlichen "Amortisationsbetrag" als Bruchteil
            # der verbleibenden Abweichung von Par.
            #
            # KORREKTUR: Rein additive Form statt multiplikativ gemischt.
            # Bei Discount (MV < 1): Pull-to-Par ist positiv (Wertsteigerung)
            # Bei Premium (MV > 1): Pull-to-Par ist negativ (Wertverlust)
            #
            # Formel: pull_to_par_absolut = (1 - MV) / Restduration
            # Dies ist der absolute Betrag, der zum Marktwert addiert wird.
            
            gov_pull_to_par_absolute = 0.0
            if gov_bond_initial_duration > 0 and current_gov_bond_duration > 0:
                deviation_from_par = 1.0 - gov_bond_market_value
                # Amortisation: Abweichung wird linear über Restlaufzeit abgebaut
                gov_pull_to_par_absolute = deviation_from_par / current_gov_bond_duration
                # Begrenze auf realistische Werte (max 10% des Par-Wertes pro Jahr)
                gov_pull_to_par_absolute = max(-0.10, min(gov_pull_to_par_absolute, 0.10))
            
            # Update Marktwert: Rein additiv (korrekte Separation der Effekte)
            # 1. Duration-Effekt wirkt multiplikativ auf aktuellen Marktwert
            # 2. Pull-to-Par wirkt additiv (unabhängig vom Duration-Effekt)
            mv_after_duration = gov_bond_market_value * (1 + gov_duration_effect)
            new_gov_market_value = mv_after_duration + gov_pull_to_par_absolute
            new_gov_market_value = max(0.5, min(new_gov_market_value, 1.5))
            
            # Relative Rendite für Pull-to-Par (für Reporting)
            gov_pull_to_par = gov_pull_to_par_absolute / gov_bond_market_value if gov_bond_market_value > 0 else 0.0
            
            # Marktwertänderung als Teil der Rendite
            gov_market_value_return = (new_gov_market_value - gov_bond_market_value) / gov_bond_market_value
            
            # GESAMTRENDITE = Coupon-Rendite + Marktwertänderung
            r_GovBonds_t = gov_coupon_return + gov_market_value_return
            
            # Update State für nächste Iteration
            gov_bond_market_value = new_gov_market_value
            
            results_path['gov_bonds_coupon'][t] = gov_coupon_return
            results_path['gov_bonds_duration_effect'][t] = gov_duration_effect
            results_path['gov_bonds_market_value'][t] = gov_bond_market_value
            results_path['gov_bonds_fixed_coupon'][t] = gov_bond_fixed_coupon
        
        results_path['gov_bonds_return'][t] = r_GovBonds_t
        
        # === A2. CORPORATE BONDS - REALISTISCHES MODELL ===
        # Gleiche Logik wie Gov Bonds, plus Credit Spread und Default-Risiko
        
        i_corp_bonds_t = r1_t + current_corp_bond_duration * cfg.YIELD_CURVE_SLOPE + corp_credit_spread
        results_path['i_corp_bonds_t'][t] = i_corp_bonds_t
        
        r_CorpBonds_t = 0.0
        corp_duration_effect = 0.0
        corp_default_loss = 0.0
        corp_pull_to_par = 0.0
        
        if current_corp_bond_duration > 0:
            # 1. COUPON-RENDITE (bezogen auf aktuellen Marktwert)
            corp_coupon_return = corp_bond_fixed_coupon / corp_bond_market_value
            
            # 2. DURATION-EFFEKT
            if abs(delta_r1) > 1e-10:
                corp_convexity = (current_corp_bond_duration**2 + current_corp_bond_duration) / ((1 + i_corp_bonds_t)**2)
                corp_duration_component = -current_corp_bond_duration * delta_r1 / (1 + i_corp_bonds_t)
                corp_convexity_component = 0.5 * corp_convexity * (delta_r1 ** 2)
                corp_duration_effect = corp_duration_component + corp_convexity_component
                corp_duration_effect = max(-0.50, min(corp_duration_effect, 0.50))
            
            # 3. PULL-TO-PAR EFFEKT (korrigierte, rein additive Form)
            corp_pull_to_par_absolute = 0.0
            if corp_bond_initial_duration > 0 and current_corp_bond_duration > 0:
                deviation_from_par = 1.0 - corp_bond_market_value
                corp_pull_to_par_absolute = deviation_from_par / current_corp_bond_duration
                corp_pull_to_par_absolute = max(-0.10, min(corp_pull_to_par_absolute, 0.10))
            
            # 4. DEFAULT-VERLUST
            if np.random.rand() < corp_default_prob:
                corp_default_loss = corp_default_exposure * corp_lgd
            
            # Update Marktwert: Rein additiv
            mv_after_duration = corp_bond_market_value * (1 + corp_duration_effect)
            new_corp_market_value = mv_after_duration + corp_pull_to_par_absolute
            new_corp_market_value = max(0.5, min(new_corp_market_value, 1.5))
            
            # Relative Rendite für Pull-to-Par (für Reporting)
            corp_pull_to_par = corp_pull_to_par_absolute / corp_bond_market_value if corp_bond_market_value > 0 else 0.0
            
            corp_market_value_return = (new_corp_market_value - corp_bond_market_value) / corp_bond_market_value
            
            # GESAMTRENDITE = Coupon + Marktwertänderung - Default-Verlust
            r_CorpBonds_t = corp_coupon_return + corp_market_value_return - corp_default_loss
            
            corp_bond_market_value = new_corp_market_value
            
            results_path['corp_bonds_coupon'][t] = corp_coupon_return
            results_path['corp_bonds_duration_effect'][t] = corp_duration_effect
            results_path['corp_bonds_default_loss'][t] = corp_default_loss
            results_path['corp_bonds_market_value'][t] = corp_bond_market_value
            results_path['corp_bonds_fixed_coupon'][t] = corp_bond_fixed_coupon
        
        results_path['corp_bonds_return'][t] = r_CorpBonds_t
        
        # === B. PORTFOLIO-RENDITE ===
        
        # === A3. INFRASTRUCTURE DEBT (Alternatives) - BOND-MODELL ===
        # Gleiche Logik wie Corp Bonds, mit eigenen Parametern (Spread, Duration, LGD, Default Prob)

        i_alt_bonds_t = r1_t + current_alt_bond_duration * cfg.YIELD_CURVE_SLOPE + alt_credit_spread
        results_path['i_alt_bonds_t'][t] = i_alt_bonds_t

        r_Alternatives_t = 0.0
        alt_duration_effect = 0.0
        alt_default_loss = 0.0
        alt_pull_to_par = 0.0

        if current_alt_bond_duration > 0:
            # 1. COUPON-RENDITE (bezogen auf aktuellen Marktwert)
            alt_coupon_return = alt_bond_fixed_coupon / alt_bond_market_value

            # 2. DURATION-EFFEKT
            if abs(delta_r1) > 1e-10:
                alt_convexity = (current_alt_bond_duration**2 + current_alt_bond_duration) / ((1 + i_alt_bonds_t)**2)
                alt_duration_component = -current_alt_bond_duration * delta_r1 / (1 + i_alt_bonds_t)
                alt_convexity_component = 0.5 * alt_convexity * (delta_r1 ** 2)
                alt_duration_effect = alt_duration_component + alt_convexity_component
                alt_duration_effect = max(-0.50, min(alt_duration_effect, 0.50))

            # 3. PULL-TO-PAR EFFEKT
            alt_pull_to_par_absolute = 0.0
            if alt_bond_initial_duration > 0 and current_alt_bond_duration > 0:
                deviation_from_par = 1.0 - alt_bond_market_value
                alt_pull_to_par_absolute = deviation_from_par / current_alt_bond_duration
                alt_pull_to_par_absolute = max(-0.10, min(alt_pull_to_par_absolute, 0.10))

            # 4. DEFAULT-VERLUST
            if np.random.rand() < alt_default_prob:
                alt_default_loss = alt_default_exposure * alt_lgd

            # Update Marktwert
            mv_after_duration = alt_bond_market_value * (1 + alt_duration_effect)
            new_alt_market_value = mv_after_duration + alt_pull_to_par_absolute
            new_alt_market_value = max(0.5, min(new_alt_market_value, 1.5))

            alt_pull_to_par = alt_pull_to_par_absolute / alt_bond_market_value if alt_bond_market_value > 0 else 0.0

            alt_market_value_return = (new_alt_market_value - alt_bond_market_value) / alt_bond_market_value

            # GESAMTRENDITE = Coupon + Marktwertänderung - Default-Verlust
            r_Alternatives_t = alt_coupon_return + alt_market_value_return - alt_default_loss

            alt_bond_market_value = new_alt_market_value

            results_path['alt_bonds_coupon'][t] = alt_coupon_return
            results_path['alt_bonds_duration_effect'][t] = alt_duration_effect
            results_path['alt_bonds_default_loss'][t] = alt_default_loss
            results_path['alt_bonds_market_value'][t] = alt_bond_market_value
            results_path['alt_bonds_fixed_coupon'][t] = alt_bond_fixed_coupon

        # Rendite der Non-Bond Asset-Klassen (Equities, RealEstate)
        r_Equities_t = simulated_returns_t[idx_equities]
        r_RealEstate_t = simulated_returns_t[idx_realestate]
        
        # === MEAN REVERSION FÜR AKTIEN ===
        # Wenn aktiviert, wird bei stark negativer kumulierter Performance
        # ein zusätzlicher positiver Drift hinzugefügt
        equity_mean_reversion_enabled = getattr(cfg, 'EQUITY_MEAN_REVERSION_ENABLED', False)
        
        if equity_mean_reversion_enabled and t > 0:
            # Berechne kumulierte Aktienrendite bis jetzt
            cum_equity_return = np.prod(1 + results_path['equities_return'][:t]) - 1
            
            # Parameter aus Config
            equity_long_term_mean = getattr(cfg, 'EQUITY_LONG_TERM_ANNUAL_RETURN', 0.05)  # 5% p.a.
            equity_mean_reversion_strength = getattr(cfg, 'EQUITY_MEAN_REVERSION_STRENGTH', 0.10)
            equity_mean_reversion_threshold = getattr(cfg, 'EQUITY_MEAN_REVERSION_THRESHOLD', -0.20)
            
            # Erwartete kumulierte Rendite nach t Jahren
            expected_cum_return = (1 + equity_long_term_mean) ** t - 1
            
            # Wenn kumulierte Rendite deutlich unter Erwartung liegt
            cum_shortfall = cum_equity_return - expected_cum_return
            
            if cum_shortfall < equity_mean_reversion_threshold:
                # Mean Reversion Adjustment: Positiver Drift proportional zum Shortfall
                mean_reversion_adjustment = -equity_mean_reversion_strength * cum_shortfall
                # Cap the adjustment to avoid unrealistic jumps
                mean_reversion_adjustment = min(mean_reversion_adjustment, 0.15)  # Max 15% Boost
                r_Equities_t = r_Equities_t + mean_reversion_adjustment
        
        results_path['equities_return'][t] = r_Equities_t
        results_path['realestate_return'][t] = r_RealEstate_t
        results_path['alternatives_return'][t] = r_Alternatives_t
        
        # Gesamte Portfoliorendite: Gewichtete Summe aller Assets
        r_port_t = (
            cfg.WEIGHTS[idx_gov_bonds] * r_GovBonds_t +
            cfg.WEIGHTS[idx_corp_bonds] * r_CorpBonds_t +
            cfg.WEIGHTS[idx_equities] * r_Equities_t +
            cfg.WEIGHTS[idx_realestate] * r_RealEstate_t +
            cfg.WEIGHTS[idx_alternatives] * r_Alternatives_t
        )
        results_path['portfolio_return'][t] = r_port_t
        
        # === INTEREST RATE CAP AUSZAHLUNG ===
        # Der Cap zahlt aus, wenn r1_t > Strike während der Cap-Laufzeit
        # Auszahlung = (r1_t - Strike) * Notional
        cap_payout = 0.0
        if interest_rate_cap_enabled and t < interest_rate_cap_duration:
            if r1_t > cap_strike_level:
                # Cap ist im Geld - Auszahlung berechnen
                rate_excess = r1_t - cap_strike_level
                # Aktueller Notional basiert auf aktuellem Vermögen * Bond-Allokation
                current_notional = V_t_minus_1 * bond_allocation
                cap_payout = rate_excess * current_notional
                results_path['interest_rate_cap_active'][t] = 1.0
            else:
                results_path['interest_rate_cap_active'][t] = 0.0
        
        results_path['interest_rate_cap_payout'][t] = cap_payout

        # --- C. Liabilities (Alterung, Tod, Rentenzahlungen) ---
        # 
        # TIMING-KONVENTION (korrigiert):
        # - Jahresanfang: Rentner ist x Jahre alt
        # - Rentenzahlung erfolgt am Jahresende (nachschüssig) WENN überlebt
        # - Sterblichkeit wird am Jahresende geprüft (nach Rentenzahlung)
        # - Barwertformeln: ä_x = Σ v^k × ₖp_x (Zahlung k Jahre später wenn überlebt)
        #
        # REIHENFOLGE: 1. Cashflow (alle Lebenden zahlen), 2. Tod prüfen, 3. Alterung
        
        # 1. RENTENZAHLUNG (alle aktuell Lebenden erhalten ihre Rente)
        active_before_death = population_state[population_state['Status'] != 'Dead']
        num_pensioners_before = active_before_death.shape[0]
        num_widows_before = (active_before_death['Status'] == 'Widow/er').sum()
        
        CF_Rent_t = active_before_death['CurrentPension'].sum()
        results_path['cashflow_rent'][t] = CF_Rent_t
        
        annual_admin_fee = float(getattr(cfg, 'ADMIN_FEE_PER_PERSON', 0.0))
        CF_AdminFee_t = annual_admin_fee * num_pensioners_before
        results_path['cashflow_admin_fee'][t] = CF_AdminFee_t
        CF_Total_t = CF_Rent_t + CF_AdminFee_t
        results_path['cashflow_total'][t] = CF_Total_t
        
        results_path['num_pensioners'][t] = num_pensioners_before
        results_path['num_widows'][t] = num_widows_before
        
        # 2a. PARTNERSTERBLICHKEIT (VOR der Rentnersterblichkeit).
        # Entspricht exakt der k+1 p_y-Bedingung: stirbt der Partner im selben Jahr
        # wie der Rentner, entsteht keine Anwartschaft (keine Witwenrente).
        if 'SpouseAlive' in population_state.columns:
            active_spouses = population_state[population_state['Status'] != 'Dead']
            for index, person in active_spouses.iterrows():
                if not bool(person.get('SpouseAlive', False)):
                    continue
                sp_diff = person.get('SpouseAgeDiff', np.nan)
                if pd.isna(sp_diff):
                    continue
                sp_gender = 'F' if person['Gender'] == 'M' else 'M'
                sp_age = int(person['Age']) + int(round(sp_diff))
                if np.random.rand() <= get_qx(sp_age, sp_gender, survival_table):
                    population_state.loc[index, 'SpouseAlive'] = False

        # 2. STERBLICHKEIT (am Jahresende, nach Rentenzahlung)
        active_pensions = population_state[population_state['Status'] != 'Dead'].copy()
        widows_data = []

        for index, person in active_pensions.iterrows():
            age = person['Age']  # Aktuelles Alter (vor Alterung)
            gender = person['Gender']
            qx = get_qx(age, gender, survival_table)

            if np.random.rand() <= qx:
                population_state.loc[index, 'Status'] = 'Dead'

                # Prüfe auf Witwenrente: nur wenn Partner beim Tod des Rentners noch lebt.
                # Fehlt die Zustandsspalte (z.B. externer Aufruf), Rückfall auf altes Verhalten.
                spouse_currently_alive = (bool(population_state.loc[index, 'SpouseAlive'])
                                          if 'SpouseAlive' in population_state.columns else True)
                if person['MaritalStatus'] == 'Married' and person['Status'] == 'Active' \
                   and spouse_currently_alive:
                    spouse_gender = 'F' if gender == 'M' else 'M'
                    # KORREKTUR Problem 3: Witwenalter von SpouseInitialAge entkoppeln.
                    # SpouseInitialAge + t ist für Sammelstiftungs-Kohorten falsch
                    # (t ist die absolute Simulationszeit, nicht die Zeit seit Beitritt).
                    # Robust: aktuelles Rentneralter + Altersdifferenz.
                    diff = person.get('SpouseAgeDiff', np.nan)
                    if pd.isna(diff):
                        diff = -3 if gender == 'M' else 3
                    spouse_age = int(person['Age']) + int(round(diff))  # Alter jetzt, vor Alterung
                    if not (0 <= spouse_age <= 110):
                        continue  # keine Witwenrente, konsistent mit den Bewertungsfunktionen
                    # Individuelle Ehegattenrente-Rate verwenden (Default: 40%)
                    spouse_pension_rate = person.get('SpousePensionRate', 0.40)
                    if pd.isna(spouse_pension_rate):
                        spouse_pension_rate = 0.40
                    widow_pension = person['InitialPension'] * spouse_pension_rate
                    
                    widows_data.append({
                        'ID': f"W{person['ID']}_{t}", 
                        'Age': spouse_age,  # Wird gleich mit allen anderen gealtert
                        'Gender': spouse_gender,
                        'MaritalStatus': 'Widow', 
                        'InitialPension': widow_pension,
                        'CurrentPension': widow_pension, 
                        'Status': 'Widow/er',
                        'YearOfDeath': 0, 
                        'SpouseAgeDiff': np.nan,
                        'SpouseInitialAge': np.nan,
                        'SpousePensionRate': np.nan,  # Witwen haben keine eigene Ehegattenrente
                        'SpouseAlive': False,  # Witwen begründen keine weitere Anwartschaft
                    })
        
        if widows_data:
            new_widows_df = pd.DataFrame(widows_data)
            population_state = pd.concat([population_state, new_widows_df], ignore_index=True)
        
        # 3. ALTERUNG (alle Überlebenden werden ein Jahr älter für nächstes Jahr)
        population_state.loc[population_state['Status'] != 'Dead', 'Age'] = \
            (population_state.loc[population_state['Status'] != 'Dead', 'Age'] + 1).astype(int)

        # --- D. Asset-Entwicklung und Deckungsgrad ---

        # V_t = Vorjahresvermögen * (1 + Rendite) - Cashflows + Cap-Auszahlung
        V_t = V_t_minus_1 * (1 + r_port_t) - CF_Total_t + cap_payout
        
        # Diskontierungssatz für Liabilities (i_tech_t = r1_t + D_Tech * Slope + Spread)
        i_tech_t = r1_t + cfg.TECHNICAL_RATE_DURATION * cfg.YIELD_CURVE_SLOPE + cfg.LIABILITY_DISCOUNT_SPREAD
        i_tech_t = max(i_tech_t, cfg.TECHNICAL_RATE_FLOOR)
        results_path['i_tech_t'][t] = i_tech_t
        
        # Update Annuity-Faktoren nur wenn sich der Zins signifikant geändert hat
        if cached_tech_rate is None or abs(i_tech_t - cached_tech_rate) > 0.0001:
            cached_annuity_factors = precompute_annuity_factors(qx_arrays, i_tech_t)
            cached_tech_rate = i_tech_t
        
        W_t = calculate_liability_barwert_base(population_state, survival_table, i_tech_t,
                                               qx_arrays, cached_annuity_factors, cached_survival_probs)
        
        # Prüfe ob liability_matching Modus aktiv ist (für Gov oder Corp Bonds)
        needs_yearly_liability_duration = (gov_duration_mode == "liability_matching" or
                                            corp_duration_mode == "liability_matching" or
                                            alt_duration_mode == "liability_matching")
        
        # Berechne Liability-Duration:
        # - Jedes Jahr wenn liability_matching Modus aktiv ist (für präzises Matching)
        # - Sonst nur alle 5 Jahre (für Performance)
        if needs_yearly_liability_duration or (t % 5 == 0):
            liability_dur = calculate_liability_duration(population_state, survival_table, i_tech_t,
                                                          qx_arrays, cached_survival_probs)
            results_path['liability_duration'][t] = liability_dur
        else:
            # Interpoliere: reduziere um ~1 pro Jahr als Näherung
            last_dur = results_path['liability_duration'][max(0, t-1)]
            results_path['liability_duration'][t] = max(0.0, last_dur - 1.0)
        
        # Speichere W_t und V_t
        results_path['W_t'][t] = W_t
        results_path['V_t'][t] = V_t
        
        # Wenn W_t -> 0 (alle Verpflichtungen erfüllt) und kein Default (V_t > 0),
        # setze Deckungsgrad auf 200% statt 0, um die Auswertung nicht zu verzerren
        if W_t > 0:
            results_path['deckungsgrad'][t] = max(0.0, V_t / W_t)
        elif V_t > 0:
            results_path['deckungsgrad'][t] = 2.0
        else:
            results_path['deckungsgrad'][t] = 0.0

        # --- D2. SONDER-RENTE (Special Pension Payout) ---
        # Prüfe ob Sonder-Rente aktiviert ist und Deckungsgrad über Schwelle liegt
        special_pension_payout = 0.0
        if (hasattr(cfg, 'SPECIAL_PENSION_ENABLED') and cfg.SPECIAL_PENSION_ENABLED and
            W_t > 0 and results_path['deckungsgrad'][t] > getattr(cfg, 'SPECIAL_PENSION_THRESHOLD', 1.15)):

            # Berechne erforderliche Auszahlung um Ziel-Deckungsgrad zu erreichen
            # DG_target = V_target / W_t
            # V_target = W_t * DG_target
            # payout = V_t - V_target
            target_dg = getattr(cfg, 'SPECIAL_PENSION_TARGET', 1.145)
            target_V = W_t * target_dg
            special_pension_payout = max(0.0, V_t - target_V)

            # Reduziere Vermögen um Sonder-Rente
            V_t -= special_pension_payout

            # Aktualisiere Deckungsgrad nach Sonder-Rente
            results_path['deckungsgrad'][t] = V_t / W_t if W_t > 0 else 0.0

            # Erhöhe Cashflow um Sonder-Rente
            CF_Total_t += special_pension_payout
            results_path['cashflow_total'][t] = CF_Total_t

            # Speichere V_t nach Sonder-Rente
            results_path['V_t'][t] = V_t

        # Speichere Sonder-Rente Auszahlung
        results_path['special_pension_payout'][t] = special_pension_payout

        # --- D3. SAMMELSTIFTUNG-MODUS (Neuer Bestand) ---
        # Im Sammelstiftung-Modus wird alle X Jahre ein neuer Rentnerbestand hinzugefügt
        # Voraussetzung: Deckungsgrad > 100% (V_t > W_t)
        if (sammelstiftung_enabled and
            initial_population_for_sammelstiftung is not None and
            (t + 1) % sammelstiftung_interval == 0 and
            V_t > W_t):

            # 1. Erstelle eine frische Kopie des initialen Bestands
            new_cohort = initial_population_for_sammelstiftung.copy()

            # 2. Aktualisiere die IDs, damit sie eindeutig sind (füge Kohorten-Suffix hinzu)
            cohort_suffix = f"_K{(t + 1) // sammelstiftung_interval}"
            new_cohort['ID'] = new_cohort['ID'].astype(str) + cohort_suffix

            # 3. Setze Status und andere Felder für die neuen Personen
            new_cohort['Status'] = 'Active'
            new_cohort['YearOfDeath'] = 0
            new_cohort['CurrentPension'] = new_cohort['InitialPension']
            new_cohort['SpouseInitialAge'] = new_cohort['Age'] + new_cohort['SpouseAgeDiff']
            # Zustandsvariable für neue Kohorte (nur Verheiratete haben lebenden Partner)
            if 'MaritalStatus' in new_cohort.columns:
                new_cohort['SpouseAlive'] = (new_cohort['MaritalStatus'] == 'Married')
            else:
                new_cohort['SpouseAlive'] = False

            # 4. Berechne den Barwert der neuen Verpflichtungen zum aktuellen i_tech_t
            new_W = calculate_liability_barwert_base(new_cohort, survival_table, i_tech_t,
                                                      qx_arrays, cached_annuity_factors, cached_survival_probs)

            # 5. Berechne den Beitrag zum Vermögen: Barwert + General Reserve Rate
            new_V = new_W * (1 + cfg.GENERAL_RESERVE_RATE)

            # 6. Aktualisiere W_t und V_t
            W_t += new_W
            V_t += new_V

            # 7. Füge die neuen Personen zum population_state hinzu
            population_state = pd.concat([population_state, new_cohort], ignore_index=True)

            # 8. Aktualisiere CFM-Tranchen wenn Cash Flow Matching aktiv ist
            if gov_duration_mode == "cashflow_matching" or corp_duration_mode == "cashflow_matching" or alt_duration_mode == "cashflow_matching":
                # Berechne erwartete Cashflows für den neuen erweiterten Bestand
                updated_expected_cfs = calculate_expected_cashflows(
                    population_state, survival_table, qx_arrays, T_horizon - t,
                    include_spouse=True
                )

                # Government Bonds CFM Tranchen aktualisieren
                if gov_duration_mode == "cashflow_matching":
                    # Aktualisiere Gov Bond Portfolio Wert (neues Vermögen * Gov Bond Gewicht)
                    new_gov_bond_value = new_V * cfg.WEIGHTS[1]
                    gov_cfm_new = calculate_cfm_tranches(updated_expected_cfs, new_gov_bond_value)
                    # Merge mit bestehenden Tranchen (addiere Werte)
                    if gov_cfm_tranches is not None:
                        for i in range(min(len(gov_cfm_tranches['values']), len(gov_cfm_new['values']))):
                            gov_cfm_tranches['values'][i] += gov_cfm_new['values'][i]
                        # Recalculate weights
                        total_value = np.sum(gov_cfm_tranches['values'])
                        if total_value > 0:
                            gov_cfm_tranches['weights'] = gov_cfm_tranches['values'] / total_value
                    else:
                        gov_cfm_tranches = gov_cfm_new

                # Corporate Bonds CFM Tranchen aktualisieren
                if corp_duration_mode == "cashflow_matching":
                    new_corp_bond_value = new_V * cfg.WEIGHTS[2]
                    corp_cfm_new = calculate_cfm_tranches(updated_expected_cfs, new_corp_bond_value)
                    if corp_cfm_tranches is not None:
                        for i in range(min(len(corp_cfm_tranches['values']), len(corp_cfm_new['values']))):
                            corp_cfm_tranches['values'][i] += corp_cfm_new['values'][i]
                        total_value = np.sum(corp_cfm_tranches['values'])
                        if total_value > 0:
                            corp_cfm_tranches['weights'] = corp_cfm_tranches['values'] / total_value
                    else:
                        corp_cfm_tranches = corp_cfm_new

                # Infrastructure Debt (Alternatives) CFM Tranchen aktualisieren
                if alt_duration_mode == "cashflow_matching":
                    new_alt_bond_value = new_V * cfg.WEIGHTS[5]
                    alt_cfm_new = calculate_cfm_tranches(updated_expected_cfs, new_alt_bond_value)
                    if alt_cfm_tranches is not None:
                        for i in range(min(len(alt_cfm_tranches['values']), len(alt_cfm_new['values']))):
                            alt_cfm_tranches['values'][i] += alt_cfm_new['values'][i]
                        total_value = np.sum(alt_cfm_tranches['values'])
                        if total_value > 0:
                            alt_cfm_tranches['weights'] = alt_cfm_tranches['values'] / total_value
                    else:
                        alt_cfm_tranches = alt_cfm_new

            # 9. Aktualisiere gespeicherte Werte
            results_path['W_t'][t] = W_t
            results_path['V_t'][t] = V_t

            # 10. Aktualisiere Deckungsgrad nach Sammelstiftung-Transaktion
            if W_t > 0:
                results_path['deckungsgrad'][t] = max(0.0, V_t / W_t)
            elif V_t > 0:
                results_path['deckungsgrad'][t] = 2.0
            else:
                results_path['deckungsgrad'][t] = 0.0

        # --- E. UPDATE FÜR NÄCHSTE ITERATION ---
        
        # Speichere den Basiszins r1 für die nächste Iteration
        r1_t_minus_1 = r1_t
        
        # Hole die aktuelle Liability-Duration (wurde oben bereits berechnet)
        current_liability_duration = results_path['liability_duration'][t]
        is_liability_recalc_year = (t % 5 == 0)  # Für fixed_reset Modus noch relevant
        
        # Speichere alte Durations für Reset-Erkennung
        old_gov_duration = current_gov_bond_duration
        old_corp_duration = current_corp_bond_duration
        old_alt_duration = current_alt_bond_duration
        
        # Duration-Management für Government Bonds (mit Duration-Mode)
        current_gov_bond_duration, gov_was_reset = get_duration_for_mode(
            mode=gov_duration_mode,
            current_duration=current_gov_bond_duration,
            initial_duration=getattr(cfg, 'INITIAL_GOV_BOND_DURATION', cfg.INITIAL_BOND_DURATION),
            liability_duration=current_liability_duration,
            reset_interval=gov_reset_interval,
            year=t,
            is_liability_recalc_year=is_liability_recalc_year,
            cfm_tranches=gov_cfm_tranches
        )

        # Duration-Management für Corporate Bonds (mit Duration-Mode)
        current_corp_bond_duration, corp_was_reset = get_duration_for_mode(
            mode=corp_duration_mode,
            current_duration=current_corp_bond_duration,
            initial_duration=getattr(cfg, 'INITIAL_CORP_BOND_DURATION', 5.0),
            liability_duration=current_liability_duration,
            reset_interval=corp_reset_interval,
            year=t,
            is_liability_recalc_year=is_liability_recalc_year,
            cfm_tranches=corp_cfm_tranches
        )

        # Duration-Management für Infrastructure Debt (Alternatives) (mit Duration-Mode)
        current_alt_bond_duration, alt_was_reset = get_duration_for_mode(
            mode=alt_duration_mode,
            current_duration=current_alt_bond_duration,
            initial_duration=getattr(cfg, 'INITIAL_ALT_BOND_DURATION', 30.0),
            liability_duration=current_liability_duration,
            reset_interval=alt_reset_interval,
            year=t,
            is_liability_recalc_year=is_liability_recalc_year,
            cfm_tranches=alt_cfm_tranches
        )

        # === RESET BOND STATE BEI NEUANLAGE ===
        # Bei Duration-Reset werden Bonds verkauft und neue gekauft
        # -> Neuer Coupon zum aktuellen Zinsniveau, Marktwert zurück auf Par
        
        # Government Bonds Reset
        if gov_duration_mode == "cashflow_matching":
            # Bei CFM: Update Coupon-Satz basierend auf verbleibenden Tranchen
            # (Coupons sind fixiert, aber die Gewichtung ändert sich wenn Tranchen auslaufen)
            if gov_cfm_tranches is not None:
                gov_bond_fixed_coupon = get_cfm_coupon_rate(gov_cfm_tranches, t + 1, r1_t,
                                                        cfg.YIELD_CURVE_SLOPE, 0.0)
            # KEIN Market Value Reset bei CFM - passives Halten
            gov_bond_initial_duration = current_gov_bond_duration
        elif gov_was_reset and gov_duration_mode != "liability_matching":
            # Bei fixed_reset: Bonds werden verkauft und neu gekauft
            gov_bond_fixed_coupon = r1_t + current_gov_bond_duration * cfg.YIELD_CURVE_SLOPE
            gov_bond_market_value = 1.0
            gov_bond_initial_duration = current_gov_bond_duration
        elif gov_duration_mode == "liability_matching":
            # Bei liability_matching: Kontinuierliches Rebalancing
            # Hier könnten wir annehmen, dass nur teilweise gehandelt wird
            # Vereinfachung: Bei signifikanter Duration-Änderung wird neu gekauft
            duration_change = abs(current_gov_bond_duration - old_gov_duration)
            if duration_change > 1.0:  # Signifikante Änderung
                # Teilweiser Reset: Interpolation basierend auf Änderungsgrösse
                rebalance_fraction = min(duration_change / old_gov_duration, 1.0) if old_gov_duration > 0 else 1.0
                new_coupon = r1_t + current_gov_bond_duration * cfg.YIELD_CURVE_SLOPE
                gov_bond_fixed_coupon = (1 - rebalance_fraction) * gov_bond_fixed_coupon + rebalance_fraction * new_coupon
                gov_bond_market_value = (1 - rebalance_fraction) * gov_bond_market_value + rebalance_fraction * 1.0
                gov_bond_initial_duration = current_gov_bond_duration
        
        # Corporate Bonds Reset
        if corp_duration_mode == "cashflow_matching":
            if corp_cfm_tranches is not None:
                corp_bond_fixed_coupon = get_cfm_coupon_rate(corp_cfm_tranches, t + 1, r1_t,
                                                         cfg.YIELD_CURVE_SLOPE, corp_credit_spread)
            corp_bond_initial_duration = current_corp_bond_duration
        elif corp_was_reset and corp_duration_mode != "liability_matching":
            corp_bond_fixed_coupon = r1_t + current_corp_bond_duration * cfg.YIELD_CURVE_SLOPE + corp_credit_spread
            corp_bond_market_value = 1.0
            corp_bond_initial_duration = current_corp_bond_duration
        elif corp_duration_mode == "liability_matching":
            duration_change = abs(current_corp_bond_duration - old_corp_duration)
            if duration_change > 1.0:
                rebalance_fraction = min(duration_change / old_corp_duration, 1.0) if old_corp_duration > 0 else 1.0
                new_coupon = r1_t + current_corp_bond_duration * cfg.YIELD_CURVE_SLOPE + corp_credit_spread
                corp_bond_fixed_coupon = (1 - rebalance_fraction) * corp_bond_fixed_coupon + rebalance_fraction * new_coupon
                corp_bond_market_value = (1 - rebalance_fraction) * corp_bond_market_value + rebalance_fraction * 1.0
                corp_bond_initial_duration = current_corp_bond_duration

        # Infrastructure Debt (Alternatives) Reset
        if alt_duration_mode == "cashflow_matching":
            if alt_cfm_tranches is not None:
                alt_bond_fixed_coupon = get_cfm_coupon_rate(alt_cfm_tranches, t + 1, r1_t,
                                                         cfg.YIELD_CURVE_SLOPE, alt_credit_spread)
            alt_bond_initial_duration = current_alt_bond_duration
        elif alt_was_reset and alt_duration_mode != "liability_matching":
            alt_bond_fixed_coupon = r1_t + current_alt_bond_duration * cfg.YIELD_CURVE_SLOPE + alt_credit_spread
            alt_bond_market_value = 1.0
            alt_bond_initial_duration = current_alt_bond_duration
        elif alt_duration_mode == "liability_matching":
            duration_change = abs(current_alt_bond_duration - old_alt_duration)
            if duration_change > 1.0:
                rebalance_fraction = min(duration_change / old_alt_duration, 1.0) if old_alt_duration > 0 else 1.0
                new_coupon = r1_t + current_alt_bond_duration * cfg.YIELD_CURVE_SLOPE + alt_credit_spread
                alt_bond_fixed_coupon = (1 - rebalance_fraction) * alt_bond_fixed_coupon + rebalance_fraction * new_coupon
                alt_bond_market_value = (1 - rebalance_fraction) * alt_bond_market_value + rebalance_fraction * 1.0
                alt_bond_initial_duration = current_alt_bond_duration

        # Speichere Bond-Durations für Analyse
        results_path['gov_bond_duration'][t] = current_gov_bond_duration
        results_path['corp_bond_duration'][t] = current_corp_bond_duration
        results_path['alt_bond_duration'][t] = current_alt_bond_duration
        
        V_t_minus_1 = V_t
        
        # Abbruchkriterium (Insolvenz)
        if V_t < -V0:
             results_path['deckungsgrad'][t:] = 0
             break

    return results_path


# ==============================================================================
# 4. AGGREGATION UND VISUALISIERUNG
# ==============================================================================

def analyze_results(full_results, T_horizon):
    """Berechnet die statistischen Kennzahlen für DG, Rendite und Cashflow."""
    
    dg_matrix = np.array([res['deckungsgrad'] for res in full_results])
    ret_matrix = np.array([res['portfolio_return'] for res in full_results])
    cf_matrix = np.array([res['cashflow_rent'] for res in full_results])
    cf_admin_matrix = np.array([res['cashflow_admin_fee'] for res in full_results])
    cf_total_matrix = np.array([res['cashflow_total'] for res in full_results])
    
    # Detail-Renditen
    gov_bonds_matrix = np.array([res['gov_bonds_return'] for res in full_results])
    corp_bonds_matrix = np.array([res['corp_bonds_return'] for res in full_results])
    equities_matrix = np.array([res['equities_return'] for res in full_results])
    realestate_matrix = np.array([res['realestate_return'] for res in full_results])
    alternatives_matrix = np.array([res['alternatives_return'] for res in full_results])
    
    # Bond-Komponenten
    gov_bonds_coupon_matrix = np.array([res['gov_bonds_coupon'] for res in full_results])
    gov_bonds_dur_matrix = np.array([res['gov_bonds_duration_effect'] for res in full_results])
    gov_bonds_mv_matrix = np.array([res['gov_bonds_market_value'] for res in full_results])
    gov_bonds_fixed_coupon_matrix = np.array([res['gov_bonds_fixed_coupon'] for res in full_results])
    
    corp_bonds_coupon_matrix = np.array([res['corp_bonds_coupon'] for res in full_results])
    corp_bonds_dur_matrix = np.array([res['corp_bonds_duration_effect'] for res in full_results])
    corp_bonds_default_matrix = np.array([res['corp_bonds_default_loss'] for res in full_results])
    corp_bonds_mv_matrix = np.array([res['corp_bonds_market_value'] for res in full_results])
    corp_bonds_fixed_coupon_matrix = np.array([res['corp_bonds_fixed_coupon'] for res in full_results])

    # Infrastructure Debt (Alternatives) Bond-Komponenten
    alt_bonds_coupon_matrix = np.array([res['alt_bonds_coupon'] for res in full_results])
    alt_bonds_dur_matrix = np.array([res['alt_bonds_duration_effect'] for res in full_results])
    alt_bonds_default_matrix = np.array([res['alt_bonds_default_loss'] for res in full_results])
    alt_bonds_mv_matrix = np.array([res['alt_bonds_market_value'] for res in full_results])
    alt_bonds_fixed_coupon_matrix = np.array([res['alt_bonds_fixed_coupon'] for res in full_results])

    # Zinsen
    r1_matrix = np.array([res['r1_t'] for res in full_results])
    i_gov_matrix = np.array([res['i_gov_bonds_t'] for res in full_results])
    i_corp_matrix = np.array([res['i_corp_bonds_t'] for res in full_results])
    i_tech_matrix = np.array([res['i_tech_t'] for res in full_results])
    i_alt_matrix = np.array([res['i_alt_bonds_t'] for res in full_results])
    
    # Vermögen und Verpflichtungen
    V_matrix = np.array([res['V_t'] for res in full_results])
    W_matrix = np.array([res['W_t'] for res in full_results])
    liab_dur_matrix = np.array([res['liability_duration'] for res in full_results])
    
    # Bond-Durations (neu für Liability Matching Tracking)
    gov_bond_dur_matrix = np.array([res['gov_bond_duration'] for res in full_results])
    corp_bond_dur_matrix = np.array([res['corp_bond_duration'] for res in full_results])
    alt_bond_dur_matrix = np.array([res['alt_bond_duration'] for res in full_results])
    
    # Demographie
    num_pensioners_matrix = np.array([res['num_pensioners'] for res in full_results])
    num_widows_matrix = np.array([res['num_widows'] for res in full_results])
    
    years = np.arange(1, T_horizon + 1)
    
    summary = pd.DataFrame({
        'Year': years,
        'Mean_DG': np.mean(dg_matrix, axis=0),
        'Median_DG': np.median(dg_matrix, axis=0),
        'P5_DG': np.percentile(dg_matrix, 5, axis=0),
        'P25_DG': np.percentile(dg_matrix, 25, axis=0),
        'P75_DG': np.percentile(dg_matrix, 75, axis=0),
        'P95_DG': np.percentile(dg_matrix, 95, axis=0),
        'Mean_Portfolio_Return': np.mean(ret_matrix, axis=0),
        'Mean_GovBonds_Return': np.mean(gov_bonds_matrix, axis=0),
        'Mean_GovBonds_Coupon': np.mean(gov_bonds_coupon_matrix, axis=0),
        'Mean_GovBonds_Duration_Effect': np.mean(gov_bonds_dur_matrix, axis=0),
        'Mean_GovBonds_MarketValue': np.mean(gov_bonds_mv_matrix, axis=0),
        'Mean_GovBonds_FixedCoupon': np.mean(gov_bonds_fixed_coupon_matrix, axis=0),
        'Mean_CorpBonds_Return': np.mean(corp_bonds_matrix, axis=0),
        'Mean_CorpBonds_Coupon': np.mean(corp_bonds_coupon_matrix, axis=0),
        'Mean_CorpBonds_Duration_Effect': np.mean(corp_bonds_dur_matrix, axis=0),
        'Mean_CorpBonds_Default_Loss': np.mean(corp_bonds_default_matrix, axis=0),
        'Mean_CorpBonds_MarketValue': np.mean(corp_bonds_mv_matrix, axis=0),
        'Mean_CorpBonds_FixedCoupon': np.mean(corp_bonds_fixed_coupon_matrix, axis=0),
        'Mean_Equities_Return': np.mean(equities_matrix, axis=0),
        'Mean_RealEstate_Return': np.mean(realestate_matrix, axis=0),
        'Mean_Alternatives_Return': np.mean(alternatives_matrix, axis=0),
        'Mean_AltBonds_Coupon': np.mean(alt_bonds_coupon_matrix, axis=0),
        'Mean_AltBonds_Duration_Effect': np.mean(alt_bonds_dur_matrix, axis=0),
        'Mean_AltBonds_Default_Loss': np.mean(alt_bonds_default_matrix, axis=0),
        'Mean_AltBonds_MarketValue': np.mean(alt_bonds_mv_matrix, axis=0),
        'Mean_AltBonds_FixedCoupon': np.mean(alt_bonds_fixed_coupon_matrix, axis=0),
        'Mean_r1_t': np.mean(r1_matrix, axis=0),
        'Mean_i_GovBonds_t': np.mean(i_gov_matrix, axis=0),
        'Mean_i_CorpBonds_t': np.mean(i_corp_matrix, axis=0),
        'Mean_i_AltBonds_t': np.mean(i_alt_matrix, axis=0),
        'Mean_i_Tech_t': np.mean(i_tech_matrix, axis=0),
        'Mean_Cashflow_Rent': np.mean(cf_matrix, axis=0),
        'Mean_Admin_Fee': np.mean(cf_admin_matrix, axis=0),
        'Mean_Cashflow_Total': np.mean(cf_total_matrix, axis=0),
        'Mean_V_t': np.mean(V_matrix, axis=0),
        'Mean_W_t': np.mean(W_matrix, axis=0),
        'Mean_Liability_Duration': np.mean(liab_dur_matrix, axis=0),
        'Mean_GovBond_Duration': np.mean(gov_bond_dur_matrix, axis=0),    # NEU
        'Mean_CorpBond_Duration': np.mean(corp_bond_dur_matrix, axis=0),  # NEU
        'Mean_AltBond_Duration': np.mean(alt_bond_dur_matrix, axis=0),   # NEU
        'Mean_Num_Pensioners': np.mean(num_pensioners_matrix, axis=0),
        'Mean_Num_Widows': np.mean(num_widows_matrix, axis=0),
    })
    
    return summary


def calculate_risk_metrics(full_results, dg_threshold=0.80):
    """
    Berechnet erweiterte Risikometriken inkl. Ursachenanalyse.
    """
    n_paths = len(full_results)
    T_horizon = len(full_results[0]['deckungsgrad'])
    
    # Analyse pro Pfad
    underfunding_events = []
    default_events = []
    
    for path_idx, res in enumerate(full_results):
        dg = res['deckungsgrad']
        V_t = res['V_t']
        W_t = res['W_t']
        
        # Erstmal prüfen ob und wann Ereignisse auftreten
        underfunding_years = np.where(dg < dg_threshold)[0]
        default_years = np.where(V_t <= 0)[0]
        
        if len(underfunding_years) > 0:
            first_year = underfunding_years[0] + 1
            
            # Kumulative Renditen bis zum Event
            cum_portfolio = np.prod(1 + res['portfolio_return'][:first_year]) - 1
            cum_gov_bonds = np.prod(1 + res['gov_bonds_return'][:first_year]) - 1
            cum_corp_bonds = np.prod(1 + res['corp_bonds_return'][:first_year]) - 1
            cum_equities = np.prod(1 + res['equities_return'][:first_year]) - 1
            cum_corp_defaults = np.sum(res['corp_bonds_default_loss'][:first_year])
            
            # Zinsänderung
            delta_rate = (res['r1_t'][first_year-1] - res['r1_t'][0]) * 10000  # in BP
            
            # Demographie
            rentner_change = res['num_pensioners'][first_year-1] - res['num_pensioners'][0]
            witwen_change = res['num_widows'][first_year-1] - res['num_widows'][0]
            
            # Liability und Asset Wachstum
            liability_growth = (W_t[first_year-1] / W_t[0]) - 1 if W_t[0] > 0 else 0
            asset_growth = (V_t[first_year-1] / V_t[0]) - 1 if V_t[0] > 0 else 0
            
            # Ursachen-String
            causes = []
            if cum_equities < -0.30:
                causes.append("Aktiencrash")
            if delta_rate < -100:
                causes.append("Zinssenkung")
            elif delta_rate > 200:
                causes.append("Zinsanstieg")
            if cum_corp_defaults > 0.01:
                causes.append("Corp-Defaults")
            if liability_growth > 0.5:
                causes.append("Liability-Explosion")
            if len(causes) == 0:
                causes.append("Kumulativ")
            
            underfunding_events.append({
                'path_nr': path_idx + 1,
                'event_year': first_year,
                'V_at_event': V_t[first_year-1],
                'W_at_event': W_t[first_year-1],
                'cum_portfolio': cum_portfolio,
                'cum_gov_bonds': cum_gov_bonds,
                'cum_corp_bonds': cum_corp_bonds,
                'cum_equities': cum_equities,
                'cum_corp_defaults': cum_corp_defaults,
                'delta_rate_bp': delta_rate,
                'rentner_change': rentner_change,
                'witwen_change': witwen_change,
                'liability_growth': liability_growth,
                'asset_growth': asset_growth,
                'causes_str': ", ".join(causes)
            })
        
        if len(default_years) > 0:
            first_year = default_years[0] + 1
            
            # Kumulative Renditen bis zum Event
            cum_portfolio = np.prod(1 + res['portfolio_return'][:first_year]) - 1
            cum_gov_bonds = np.prod(1 + res['gov_bonds_return'][:first_year]) - 1
            cum_corp_bonds = np.prod(1 + res['corp_bonds_return'][:first_year]) - 1
            cum_equities = np.prod(1 + res['equities_return'][:first_year]) - 1
            cum_corp_defaults = np.sum(res['corp_bonds_default_loss'][:first_year])
            
            delta_rate = (res['r1_t'][first_year-1] - res['r1_t'][0]) * 10000
            rentner_change = res['num_pensioners'][first_year-1] - res['num_pensioners'][0]
            witwen_change = res['num_widows'][first_year-1] - res['num_widows'][0]
            liability_growth = (W_t[first_year-1] / W_t[0]) - 1 if W_t[0] > 0 else 0
            
            causes = []
            if cum_equities < -0.50:
                causes.append("Aktiencrash")
            if delta_rate < -150:
                causes.append("Zinssenkung")
            elif delta_rate > 300:
                causes.append("Zinsanstieg")
            if cum_corp_defaults > 0.02:
                causes.append("Corp-Defaults")
            if liability_growth > 1.0:
                causes.append("Liability-Explosion")
            if len(causes) == 0:
                causes.append("Kumulativ")
            
            default_events.append({
                'path_nr': path_idx + 1,
                'event_year': first_year,
                'W_at_default': W_t[first_year-1],
                'cum_portfolio': cum_portfolio,
                'cum_gov_bonds': cum_gov_bonds,
                'cum_corp_bonds': cum_corp_bonds,
                'cum_equities': cum_equities,
                'cum_corp_defaults': cum_corp_defaults,
                'delta_rate_bp': delta_rate,
                'rentner_change': rentner_change,
                'witwen_change': witwen_change,
                'liability_growth': liability_growth,
                'causes_str': ", ".join(causes)
            })
    
    # Aggregierte Statistiken
    n_underfunding = len(underfunding_events)
    n_default = len(default_events)
    
    risk_metrics = {
        'n_paths_total': n_paths,
        'n_paths_underfunding': n_underfunding,
        'prob_underfunding': n_underfunding / n_paths,
        'n_paths_default': n_default,
        'prob_default': n_default / n_paths,
        'underfunding_details': underfunding_events,
        'default_details': default_events,
    }
    
    if n_underfunding > 0:
        years = [e['event_year'] for e in underfunding_events]
        V_vals = [e['V_at_event'] for e in underfunding_events]
        W_vals = [e['W_at_event'] for e in underfunding_events]
        risk_metrics['year_underfunding_mean'] = np.mean(years)
        risk_metrics['year_underfunding_min'] = np.min(years)
        risk_metrics['V_at_underfunding_mean'] = np.mean(V_vals)
        risk_metrics['V_at_underfunding_P5'] = np.percentile(V_vals, 5)
        risk_metrics['W_at_underfunding_mean'] = np.mean(W_vals)
        risk_metrics['W_at_underfunding_P5'] = np.percentile(W_vals, 5)
    else:
        for key in ['year_underfunding_mean', 'year_underfunding_min', 
                    'V_at_underfunding_mean', 'V_at_underfunding_P5',
                    'W_at_underfunding_mean', 'W_at_underfunding_P5']:
            risk_metrics[key] = None
    
    if n_default > 0:
        years = [e['event_year'] for e in default_events]
        W_vals = [e['W_at_default'] for e in default_events]
        risk_metrics['year_default_mean'] = np.mean(years)
        risk_metrics['year_default_min'] = np.min(years)
        risk_metrics['W_at_default_mean'] = np.mean(W_vals)
        risk_metrics['W_at_default_P5'] = np.percentile(W_vals, 5)
    else:
        for key in ['year_default_mean', 'year_default_min', 
                    'W_at_default_mean', 'W_at_default_P5']:
            risk_metrics[key] = None
    
    # Ursachen-Statistik
    all_causes = []
    for e in underfunding_events + default_events:
        all_causes.extend(e['causes_str'].split(", "))
    
    if all_causes:
        from collections import Counter
        cause_counts = Counter(all_causes)
        risk_metrics['cause_distribution'] = dict(cause_counts.most_common())
    else:
        risk_metrics['cause_distribution'] = {}
    
    return risk_metrics


# ==============================================================================
# NEU: Export aller Pfade als CSV (nur bei direktem Aufruf)
# ==============================================================================

def export_all_paths_csv(full_results, T_horizon, output_dir='data', population_stats=None):
    """
    Exportiert alle Monte-Carlo-Pfade in eine CSV-Datei.
    Jede Zeile enthält: Pfad-Nr, Jahr, Deckungsgrad, V_t, W_t, Portfolio-Return, etc.
    Plus: Asset Allocation, Zinsstruktur und Duration-Parameter aus config_alm.py
    Plus: Kennzahlen des ursprünglichen Rentnerbestands (population_stats)
    
    Wird nur ausgeführt wenn das Skript direkt gestartet wird (nicht bei Import durch optimize_alm.py).
    """
    os.makedirs(output_dir, exist_ok=True)
    
    # Konfigurationsparameter sammeln
    config_params = {
        # Asset Allocation
        'weight_gov_bonds': cfg.WEIGHTS[1] if len(cfg.WEIGHTS) > 1 else 0,
        'weight_corp_bonds': cfg.WEIGHTS[2] if len(cfg.WEIGHTS) > 2 else 0,
        'weight_equities': cfg.WEIGHTS[3] if len(cfg.WEIGHTS) > 3 else 0,
        'weight_real_estate': cfg.WEIGHTS[4] if len(cfg.WEIGHTS) > 4 else 0,
        'weight_alternatives': cfg.WEIGHTS[5] if len(cfg.WEIGHTS) > 5 else 0,
        
        # Erwartete Renditen
        'mu_interest_rate': cfg.MU[0] if len(cfg.MU) > 0 else 0,
        'mu_gov_bonds': cfg.MU[1] if len(cfg.MU) > 1 else 0,
        'mu_corp_bonds': cfg.MU[2] if len(cfg.MU) > 2 else 0,
        'mu_equities': cfg.MU[3] if len(cfg.MU) > 3 else 0,
        'mu_real_estate': cfg.MU[4] if len(cfg.MU) > 4 else 0,
        'mu_alternatives': cfg.MU[5] if len(cfg.MU) > 5 else 0,
        
        # Volatilitäten
        'sigma_interest_rate': cfg.SIGMA[0] if len(cfg.SIGMA) > 0 else 0,
        'sigma_gov_bonds': cfg.SIGMA[1] if len(cfg.SIGMA) > 1 else 0,
        'sigma_corp_bonds': cfg.SIGMA[2] if len(cfg.SIGMA) > 2 else 0,
        'sigma_equities': cfg.SIGMA[3] if len(cfg.SIGMA) > 3 else 0,
        'sigma_real_estate': cfg.SIGMA[4] if len(cfg.SIGMA) > 4 else 0,
        'sigma_alternatives': cfg.SIGMA[5] if len(cfg.SIGMA) > 5 else 0,
        
        # Zinsstruktur
        'yield_curve_slope': cfg.YIELD_CURVE_SLOPE,
        'basis_rate_floor': getattr(cfg, 'BASIS_RATE_FLOOR', -0.01),
        'technical_rate_duration': cfg.TECHNICAL_RATE_DURATION,
        'liability_discount_spread': cfg.LIABILITY_DISCOUNT_SPREAD,
        'technical_rate_floor': cfg.TECHNICAL_RATE_FLOOR,
        
        # Government Bonds Duration
        'initial_gov_bond_duration': getattr(cfg, 'INITIAL_GOV_BOND_DURATION', cfg.INITIAL_BOND_DURATION),
        'gov_bond_duration_mode': getattr(cfg, 'GOV_BOND_DURATION_MODE', 'fixed_reset'),
        'gov_bond_duration_reset_interval': getattr(cfg, 'GOV_BOND_DURATION_RESET_INTERVAL', cfg.DURATION_RESET_INTERVAL),

        # Corporate Bonds
        'initial_corp_bond_duration': getattr(cfg, 'INITIAL_CORP_BOND_DURATION', 5.0),
        'corp_bond_duration_mode': getattr(cfg, 'CORP_BOND_DURATION_MODE', 'fixed_reset'),
        'corp_bond_duration_reset_interval': getattr(cfg, 'CORP_BOND_DURATION_RESET_INTERVAL', 5),
        'corp_bond_credit_spread': getattr(cfg, 'CORP_BOND_CREDIT_SPREAD', 0.01),
        'corp_bond_default_probability': getattr(cfg, 'CORP_BOND_DEFAULT_PROBABILITY', 0.003),
        'corp_bond_loss_given_default': getattr(cfg, 'CORP_BOND_LOSS_GIVEN_DEFAULT', 0.40),
        'corp_bond_default_exposure': getattr(cfg, 'CORP_BOND_DEFAULT_EXPOSURE', 0.02),

        # Infrastructure Debt (Alternatives)
        'initial_alt_bond_duration': getattr(cfg, 'INITIAL_ALT_BOND_DURATION', 30.0),
        'alt_bond_duration_mode': getattr(cfg, 'ALT_BOND_DURATION_MODE', 'fixed'),
        'alt_bond_duration_reset_interval': getattr(cfg, 'ALT_BOND_DURATION_RESET_INTERVAL', 5),
        'alt_bond_credit_spread': getattr(cfg, 'ALT_BOND_CREDIT_SPREAD', 0.015),
        'alt_bond_default_probability': getattr(cfg, 'ALT_BOND_DEFAULT_PROBABILITY', 0.013),
        'alt_bond_loss_given_default': getattr(cfg, 'ALT_BOND_LOSS_GIVEN_DEFAULT', 0.35),
        'alt_bond_default_exposure': getattr(cfg, 'ALT_BOND_DEFAULT_EXPOSURE', 0.02),

        # Finanzierung
        'general_reserve_rate': cfg.GENERAL_RESERVE_RATE,
        'admin_fee_per_person': getattr(cfg, 'ADMIN_FEE_PER_PERSON', 0),
        
        # Mean Reversion
        'equity_mean_reversion_enabled': getattr(cfg, 'EQUITY_MEAN_REVERSION_ENABLED', False),
        'equity_long_term_annual_return': getattr(cfg, 'EQUITY_LONG_TERM_ANNUAL_RETURN', 0.05),
        'equity_mean_reversion_strength': getattr(cfg, 'EQUITY_MEAN_REVERSION_STRENGTH', 0.10),
        'equity_mean_reversion_threshold': getattr(cfg, 'EQUITY_MEAN_REVERSION_THRESHOLD', -0.20),
        
        # Interest Rate Cap
        'interest_rate_cap_enabled': getattr(cfg, 'INTEREST_RATE_CAP_ENABLED', False),
        'interest_rate_cap_strike': getattr(cfg, 'INTEREST_RATE_CAP_STRIKE', 0.0075),
        'interest_rate_cap_premium': getattr(cfg, 'INTEREST_RATE_CAP_PREMIUM', 0.002),
        'interest_rate_cap_duration': getattr(cfg, 'INTEREST_RATE_CAP_DURATION', 1),

        # Special Pension Payout (Sonder-Rente)
        'special_pension_enabled': getattr(cfg, 'SPECIAL_PENSION_ENABLED', False),
        'special_pension_threshold': getattr(cfg, 'SPECIAL_PENSION_THRESHOLD', 1.15),
        'special_pension_target': getattr(cfg, 'SPECIAL_PENSION_TARGET', 1.145),

        # Sammelstiftung-Modus
        'sammelstiftung_enabled': getattr(cfg, 'SAMMELSTIFTUNG_ENABLED', False),
        'sammelstiftung_interval': getattr(cfg, 'SAMMELSTIFTUNG_INTERVAL', 5),

        # Simulation
        'n_paths': cfg.N_PATHS,
        't_horizon': cfg.T_HORIZON,
    }
    
    # Ursprünglicher Rentnerbestand hinzufügen (falls übergeben)
    if population_stats is not None:
        # Präfix 'bestand_' für Konsistenz mit optimize_alm_beta.py
        for key, val in population_stats.items():
            config_params[f'bestand_{key}'] = val
    
    rows = []
    for path_idx, res in enumerate(full_results):
        for t in range(T_horizon):
            row = {
                # Pfad-Identifikation
                'path_nr': path_idx + 1,
                'year': t + 1,
                
                # Haupt-Ergebnisse
                'deckungsgrad': res['deckungsgrad'][t],
                'V_t': res['V_t'][t],
                'W_t': res['W_t'][t],
                
                # Portfolio-Renditen
                'portfolio_return': res['portfolio_return'][t],
                'gov_bonds_return': res['gov_bonds_return'][t],
                'corp_bonds_return': res['corp_bonds_return'][t],
                'equities_return': res['equities_return'][t],
                'realestate_return': res['realestate_return'][t],
                'alternatives_return': res['alternatives_return'][t],
                
                # Bond-Komponenten
                'gov_bonds_coupon': res['gov_bonds_coupon'][t],
                'gov_bonds_duration_effect': res['gov_bonds_duration_effect'][t],
                'corp_bonds_coupon': res['corp_bonds_coupon'][t],
                'corp_bonds_duration_effect': res['corp_bonds_duration_effect'][t],
                'corp_bonds_default_loss': res['corp_bonds_default_loss'][t],
                'alt_bonds_coupon': res['alt_bonds_coupon'][t],
                'alt_bonds_duration_effect': res['alt_bonds_duration_effect'][t],
                'alt_bonds_default_loss': res['alt_bonds_default_loss'][t],

                # Zinsen
                'r1_t': res['r1_t'][t],
                'i_gov_bonds_t': res['i_gov_bonds_t'][t],
                'i_corp_bonds_t': res['i_corp_bonds_t'][t],
                'i_alt_bonds_t': res['i_alt_bonds_t'][t],
                'i_tech_t': res['i_tech_t'][t],
                
                # Cashflows
                'cashflow_rent': res['cashflow_rent'][t],
                'cashflow_admin_fee': res['cashflow_admin_fee'][t],
                'cashflow_total': res['cashflow_total'][t],
                'special_pension_payout': res['special_pension_payout'][t],

                # Demographie
                'liability_duration': res['liability_duration'][t],
                'num_pensioners': res['num_pensioners'][t],
                'num_widows': res['num_widows'][t],

                # Interest Rate Cap
                'interest_rate_cap_payout': res['interest_rate_cap_payout'][t],
                'interest_rate_cap_active': res['interest_rate_cap_active'][t],
            }
            
            # Konfigurationsparameter hinzufügen
            row.update(config_params)
            
            rows.append(row)
    
    df = pd.DataFrame(rows)
    
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = f"mc_all_paths_{timestamp}.csv"
    filepath = os.path.join(output_dir, filename)
    
    # Schweizer Format
    df.to_csv(filepath, index=False, sep=';', decimal='.')
    
    print(f"✅ Alle {len(full_results)} Pfade exportiert nach: {filepath}")
    print(f"   Spalten: {len(df.columns)} (inkl. {len(config_params)} Konfig-Parameter)")
    return filepath


def export_paths_json(full_results, T_horizon, output_dir='data', max_paths=100):
    """
    Exportiert Pfad-Daten als JSON für die Web-Visualisierung.
    Enthält nur die Deckungsgrad-Zeitreihen für Performance.
    """
    os.makedirs(output_dir, exist_ok=True)
    
    # Begrenze auf max_paths für Performance
    n_paths = min(len(full_results), max_paths)
    
    # Berechne Risikometriken
    risk_metrics = calculate_risk_metrics(full_results, dg_threshold=0.80)
    
    paths_data = {
        'n_paths': len(full_results),
        'n_paths_displayed': n_paths,
        'T_horizon': T_horizon,
        'start_dg': (1 + cfg.GENERAL_RESERVE_RATE),
        'years': list(range(0, T_horizon + 1)),
        'paths': [],
        'risk_metrics': {
            'prob_underfunding': risk_metrics['prob_underfunding'],
            'prob_default': risk_metrics['prob_default'],
            'n_paths_underfunding': risk_metrics['n_paths_underfunding'],
            'n_paths_default': risk_metrics['n_paths_default'],
            'W_at_default_mean': risk_metrics.get('W_at_default_mean'),
            'W_at_default_P5': risk_metrics.get('W_at_default_P5'),
            'year_underfunding_mean': risk_metrics.get('year_underfunding_mean'),
            'year_default_mean': risk_metrics.get('year_default_mean'),
        }
    }
    
    # Wähle gleichmässig verteilte Pfade
    indices = np.linspace(0, len(full_results) - 1, n_paths, dtype=int)
    
    for idx in indices:
        res = full_results[idx]
        # Füge Startpunkt hinzu (Jahr 0)
        dg_series = [paths_data['start_dg']] + list(res['deckungsgrad'])
        paths_data['paths'].append({
            'path_nr': int(idx + 1),
            'deckungsgrad': [float(x) for x in dg_series],
            'final_dg': float(res['deckungsgrad'][-1]),
            'min_dg': float(np.min(res['deckungsgrad'])),
            'has_underfunding': bool(np.any(res['deckungsgrad'] < 0.80)),
            'has_default': bool(np.any(res['V_t'] <= 0))
        })
    
    # Statistiken
    all_dg = np.array([res['deckungsgrad'] for res in full_results])
    paths_data['statistics'] = {
        'mean_dg': [float(paths_data['start_dg'])] + [float(x) for x in np.mean(all_dg, axis=0)],
        'p5_dg': [float(paths_data['start_dg'])] + [float(x) for x in np.percentile(all_dg, 5, axis=0)],
        'p95_dg': [float(paths_data['start_dg'])] + [float(x) for x in np.percentile(all_dg, 95, axis=0)],
        'median_dg': [float(paths_data['start_dg'])] + [float(x) for x in np.median(all_dg, axis=0)],
    }
    
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = f"mc_paths_chart_{timestamp}.json"
    filepath = os.path.join(output_dir, filename)
    
    with open(filepath, 'w') as f:
        json.dump(paths_data, f)
    
    print(f"✅ Chart-Daten exportiert nach: {filepath}")
    return filepath, paths_data


def plot_deckungsgrad_evolution(summary_results_dg):
    """Plotet den Verlauf des Deckungsgrads."""
    
    start_dg = (1 + cfg.GENERAL_RESERVE_RATE) * 100
    
    plot_df = summary_results_dg.copy()
    
    start_row = pd.DataFrame({
        'Year': [0],
        'Mean_DG': [start_dg / 100],
        'Median_DG': [start_dg / 100],
        'P5_DG': [start_dg / 100],
        'P95_DG': [start_dg / 100]
    })
    plot_df = pd.concat([start_row, plot_df], ignore_index=True)
    
    plot_df[['Mean_DG', 'Median_DG', 'P5_DG', 'P95_DG']] = plot_df[['Mean_DG', 'Median_DG', 'P5_DG', 'P95_DG']] * 100
    
    plt.figure(figsize=(12, 7))
    
    plt.plot(plot_df['Year'], plot_df['Mean_DG'],
             label='Erwarteter Deckungsgrad (Mittelwert)', color='#004c99', linewidth=2)
    
    plt.plot(plot_df['Year'], plot_df['P5_DG'],
             label='5%-Quantil (Risikokapital-Minimum)', color='#e3000f', linestyle='--', linewidth=2)
    
    plt.axhline(100, color='grey', linestyle='-', linewidth=1.5, alpha=0.7, label='100% Deckungsgrad (Gesetzliches Minimum)')

    spread_bp = cfg.LIABILITY_DISCOUNT_SPREAD * 10000
    
    title_text = f'DG-Prognose (Start-DG: {start_dg:.1f}%, D_Bond: {cfg.INITIAL_BOND_DURATION:.1f}, Spread: {spread_bp:.0f} BP, N={cfg.N_PATHS:,})'
    plt.title(title_text, fontsize=14)
    
    plt.xlabel('Simulationsjahr (t)', fontsize=12)
    plt.ylabel('Deckungsgrad in %', fontsize=12)
    plt.legend(fontsize=10)
    plt.grid(True, linestyle=':', alpha=0.6)
    plt.xlim(0, cfg.T_HORIZON)
    y_min = min(90, plot_df['P5_DG'].min())
    y_max = min(500, plot_df['P95_DG'].max()) + 5
    plt.ylim(y_min, y_max)
    plt.tight_layout()
    plt.show()


# ==============================================================================
# 5. HAUPTPROGRAMM
# ==============================================================================

if __name__ == '__main__':
    _RUNNING_AS_MAIN = True
    
    if DEBUG_MODE_SINGLE_PATH:
        print("\n*** DEBUG-MODUS AKTIVIERT: Führe nur einen Einzelpfad aus. ***")
        # Debug-Modus Code hier...
        pass
        
    else:
        print("--- START DATENVORBEREITUNG ---")
        
        NUM_CORES = mp.cpu_count() - 1
        if NUM_CORES < 1: NUM_CORES = 1
        print(f"Nutze {NUM_CORES} Kerne für die parallele Berechnung.")

        start_time = time.time()
        
        survival_table_df, initial_population_df = load_data(cfg.SURVIVAL_TABLE_PATH, cfg.POPULATION_PATH)
        
        if survival_table_df is None:
            exit()
            
        W0 = calculate_liability_barwert_initial(initial_population_df, survival_table_df)
            
        pop_stats = compute_initial_population_stats(initial_population_df, W0)
        INITIAL_ASSETS_V0 = W0 * (1 + cfg.GENERAL_RESERVE_RATE)
        
        initial_tech_rate = EXPECTED_BASIS_RATE + cfg.TECHNICAL_RATE_DURATION * cfg.YIELD_CURVE_SLOPE + cfg.LIABILITY_DISCOUNT_SPREAD
        initial_bond_rate = EXPECTED_BASIS_RATE + cfg.INITIAL_BOND_DURATION * cfg.YIELD_CURVE_SLOPE

        print(f"  > Erw. Basiszinssatz (r1): {EXPECTED_BASIS_RATE*100:.2f}%")
        print(f"  > Liability Discount Spread: {cfg.LIABILITY_DISCOUNT_SPREAD*10000:.0f} Basispunkte")
        print(f"  > Init. Tech. Zins (D={cfg.TECHNICAL_RATE_DURATION:.0f} J, inkl. Spread): {initial_tech_rate*100:.2f}%")
        print(f"  > Startkapital V(0) (DG={(INITIAL_ASSETS_V0/W0)*100:.1f}%): {INITIAL_ASSETS_V0:,.0f} CHF")

        print(f"\n--- STARTE PARALLELE MC-SIMULATION ({cfg.N_PATHS:,} Pfade) ---")

        print("  > Vorberechnung der Sterbetafeln...")
        qx_arrays_precomputed = precompute_mortality_arrays(survival_table_df)
        
        # Jeder Pfad bekommt einen eigenen Index für den Random Seed
        map_arguments = [
            (initial_population_df, survival_table_df, cfg.T_HORIZON, INITIAL_ASSETS_V0, qx_arrays_precomputed, path_idx)
            for path_idx in range(cfg.N_PATHS)
        ]

        try:
            pool = mp.Pool(processes=NUM_CORES)
            full_results = pool.map(run_monte_carlo_path_full, map_arguments)
            pool.close()
            pool.join()
            
        except Exception as e:
            print(f"FEHLER bei der Parallelisierung: {e}")
            exit()
        
        end_time = time.time()
        total_time = end_time - start_time
        
        print("\n--- SIMULATION BEENDET. STARTE ANALYSE UND EXPORT. ---")

        summary_results = analyze_results(full_results, cfg.T_HORIZON)
        
        print("  > Berechne Risikometriken...")
        risk_metrics = calculate_risk_metrics(full_results, dg_threshold=0.80)
        
        # =====================================================================
        # NEU: Export aller Pfade als CSV (nur bei direktem Aufruf!)
        # =====================================================================
        print("  > Exportiere alle Pfade als CSV...")
        all_paths_csv = export_all_paths_csv(full_results, cfg.T_HORIZON, population_stats=pop_stats)
        
        print("  > Exportiere Chart-Daten als JSON...")
        chart_json_path, chart_data = export_paths_json(full_results, cfg.T_HORIZON)
        
        # Zusammenfassung ausgeben
        print(f"\n{'='*70}")
        print("SIMULATIONS-ERGEBNISSE")
        print(f"{'='*70}")
        print(f"  Pfade simuliert:        {cfg.N_PATHS:,}")
        print(f"  Horizont:               {cfg.T_HORIZON} Jahre")
        print(f"  Rechenzeit:             {total_time:.1f} Sekunden")
        print(f"  Start-Deckungsgrad:     {(1+cfg.GENERAL_RESERVE_RATE)*100:.1f}%")
        print(f"  End-DG (Mittelwert):    {summary_results['Mean_DG'].iloc[-1]*100:.1f}%")
        print(f"  End-DG (5%-Quantil):    {summary_results['P5_DG'].iloc[-1]*100:.1f}%")
        print(f"\n  P(Unterdeckung <80%):   {risk_metrics['prob_underfunding']*100:.1f}%")
        print(f"  P(Default):             {risk_metrics['prob_default']*100:.1f}%")
        print(f"\n  Dateien exportiert:")
        print(f"    - {all_paths_csv}")
        print(f"    - {chart_json_path}")
        
        # Visualisierung (optional, kann auskommentiert werden für Server)
        # plot_deckungsgrad_evolution(summary_results)
