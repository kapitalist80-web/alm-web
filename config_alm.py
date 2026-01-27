# ==============================================================================
# 0. ALLGEMEINE SIMULATIONS-PARAMETER
# ==============================================================================

N_PATHS = 500            # Anzahl der Monte-Carlo-Pfade
T_HORIZON = 40            # Simulationshorizont in Jahren

# --- DATEN-PFADE ---
SURVIVAL_TABLE_PATH = 'data/sterbetabelle_LPP.csv'
POPULATION_PATH = 'data/rentnerbestand_initial.csv'

# ==============================================================================
# 1. KAPITALMARKT-PARAMETER
# ==============================================================================

ASSET_CLASSES = ['InterestRate', 'GovBonds', 'CorpBonds', 'Equities', 'RealEstate', 'Alternatives']
N_ASSETS = len(ASSET_CLASSES)

MU =      [0.0015, 0.0, 0.0, 0.055, 0.025, 0.035]
SIGMA =   [0.0061, 0.03, 0.04, 0.15, 0.04, 0.1]

DEGREES_OF_FREEDOM = [None, None, None, 5, 6, 5]

WEIGHTS = [0.0, 0.1, 0.55, 0.0, 0.35, 0.0]

CORR_MATRIX = [
    [1.00, 0.9, 0.8, 0.2, 0.4, 0.3],
    [0.9, 1.00, 0.85, 0.3, 0.2, 0.4],
    [0.8, 0.85, 1.00, 0.45, 0.3, 0.5],
    [0.2, 0.3, 0.45, 1.00, 0.5, 0.6],
    [0.4, 0.2, 0.3, 0.5, 1.00, 0.3],
    [0.3, 0.4, 0.5, 0.6, 0.3, 1.00],
]

# ==============================================================================
# 2. ZINSSTRUKTUR & DURATION-PARAMETER
# ==============================================================================

YIELD_CURVE_SLOPE = 0.00025
BASIS_RATE_FLOOR = -0.01

# --- STAATSANLEIHEN (Government Bonds) ---
INITIAL_GOV_BOND_DURATION = 22.0
GOV_BOND_DURATION_MODE = "cashflow_matching"
GOV_BOND_DURATION_RESET_INTERVAL = 5

# --- CORPORATE BONDS ---
INITIAL_CORP_BOND_DURATION = 8.0
CORP_BOND_DURATION_MODE = "cashflow_matching"
CORP_BOND_DURATION_RESET_INTERVAL = 4

CORP_BOND_CREDIT_SPREAD = 0.005
CORP_BOND_DEFAULT_PROBABILITY = 0.003
CORP_BOND_LOSS_GIVEN_DEFAULT = 0.4
CORP_BOND_DEFAULT_EXPOSURE = 0.02

# --- PRIVATE IG CORE INFRASTRUCTURE DEBT CHF (Alternatives) ---
# Art. 53 BVV2 Abs.1 lit. d_bis und d_ter: max. 25% Allokation
INITIAL_ALT_BOND_DURATION = 30.0
ALT_BOND_DURATION_MODE = "fixed"
ALT_BOND_DURATION_RESET_INTERVAL = 5

ALT_BOND_CREDIT_SPREAD = 0.015        # 150 Basispunkte
ALT_BOND_DEFAULT_PROBABILITY = 0.013  # 1.3% p.a.
ALT_BOND_LOSS_GIVEN_DEFAULT = 0.35    # 35% LGD
ALT_BOND_DEFAULT_EXPOSURE = 0.02      # 2%

INITIAL_BOND_DURATION = INITIAL_GOV_BOND_DURATION
DURATION_RESET_INTERVAL = GOV_BOND_DURATION_RESET_INTERVAL

TECHNICAL_RATE_DURATION = 12.0
LIABILITY_DISCOUNT_SPREAD = 0.003
TECHNICAL_RATE_FLOOR = 0.005

# ==============================================================================
# 3. FINANZIERUNGSPARAMETER
# ==============================================================================

GENERAL_RESERVE_RATE = 0.045
ADMIN_FEE_PER_PERSON = 150.0

# ==============================================================================
# 5. MEAN REVERSION FÜR AKTIEN
# ==============================================================================

EQUITY_MEAN_REVERSION_ENABLED = True
EQUITY_LONG_TERM_ANNUAL_RETURN = 0.05
EQUITY_MEAN_REVERSION_STRENGTH = 0.1
EQUITY_MEAN_REVERSION_THRESHOLD = -0.3

# ==============================================================================
# 6. INTEREST RATE CAP (Zinsabsicherung)
# ==============================================================================

# Der Interest Rate Cap schützt gegen steigende Zinsen (Zinsschock)
# Notional = Bond-Allokation (Gov + Corp) * Anfangsvermögen
# Strike = Basiszins bei t=0 + INTEREST_RATE_CAP_STRIKE
# Bei r1_t > Strike erhält die PK eine Auszahlung = (r1_t - Strike) * Notional

INTEREST_RATE_CAP_ENABLED = False
INTEREST_RATE_CAP_STRIKE = 0.0075  # 75 BP über Basiszins
INTEREST_RATE_CAP_PREMIUM = 0.002  # 0.2% = 20 BP des Notionals
INTEREST_RATE_CAP_DURATION = 1  # Laufzeit in Jahren

# ==============================================================================
# 7. SPECIAL PENSION PAYOUT (Sonder-Rente)
# ==============================================================================

# Bei Deckungsgrad > SPECIAL_PENSION_THRESHOLD wird eine Sonder-Rente ausgezahlt
# Die Höhe wird so gewählt, dass der Deckungsgrad auf ca. SPECIAL_PENSION_TARGET sinkt
# Dadurch steigt der Cashflow entsprechend

SPECIAL_PENSION_ENABLED = False
SPECIAL_PENSION_THRESHOLD = 1.15  # Schwelle: 115%
SPECIAL_PENSION_TARGET = 1.145  # Ziel: 114.5%

# ==============================================================================
# 8. SAMMELSTIFTUNG-MODUS (Collective Foundation Mode)
# ==============================================================================

# Im Sammelstiftung-Modus wird alle SAMMELSTIFTUNG_INTERVAL Jahre ein neuer
# Rentnerbestand hinzugefügt. Die Transaktion findet nur statt, wenn der
# Deckungsgrad > 100% ist (V_t > W_t).
#
# Der neue Bestand entspricht dem initialen Bestand aus rentnerbestand_initial.csv.
# Der Barwert wird zum aktuellen technischen Zinssatz i_tech_t diskontiert.
# Dem Vermögen wird der Barwert plus die General Reserve Rate hinzugefügt.
# Bei Cash Flow Matching werden die neuen CFM-Tranchen entsprechend aktualisiert.

SAMMELSTIFTUNG_ENABLED = True
SAMMELSTIFTUNG_INTERVAL = 5  # Alle X Jahre
