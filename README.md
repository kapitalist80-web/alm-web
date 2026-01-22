# ALM Optimizer Web Interface

Eine FastAPI-basierte Webanwendung zur Asset-Liability-Management (ALM) Simulation und Optimierung für Pensionskassen.

## Inhaltsverzeichnis

- [Überblick](#überblick)
- [Projektstruktur](#projektstruktur)
- [Kern-Skripte](#kern-skripte)
  - [app.py - Web-Server](#apppy---web-server)
  - [main_alm_simulation.py - Simulations-Engine](#main_alm_simulationpy---simulations-engine)
  - [optimize_alm_beta.py - Optimierungs-Algorithmen](#optimize_alm_betapy---optimierungs-algorithmen)
  - [config_alm.py - Konfiguration](#config_almpy---konfiguration)
  - [functions.R - R-Analyse-Suite](#functionsr---r-analyse-suite)
  - [functions_parallel.R - Parallelisierte R-Analyse](#functions_parallelr---parallelisierte-r-analyse)
- [Installation](#installation)
- [Verwendung](#verwendung)
- [API-Endpunkte](#api-endpunkte)
- [Standard-Konfiguration](#standard-konfiguration)
- [Technische Details](#technische-details)

---

## Überblick

Das ALM Optimizer System ist eine umfassende Lösung zur Simulation und Optimierung von Pensionskassen-Portfolios. Es verwendet Monte-Carlo-Simulationen, um zukünftige Finanzszenarien zu modellieren und die Asset-Allokation zu optimieren, um Unterdeckungsrisiken und Ausfallwahrscheinlichkeiten zu minimieren.

**Hauptfunktionen:**
- Monte-Carlo-Simulation mit 200-500 Pfaden über 40 Jahre
- 6 Asset-Klassen (Zinsen, Staatsanleihen, Unternehmensanleihen, Aktien, Immobilien, Alternative)
- Sterblichkeitsmodellierung mit Ehepartnern-Leistungen
- Bayesianische Optimierung der Asset-Allokation
- Echtzeit-WebSocket-Updates
- Interaktive Web-Oberfläche
- Detaillierte Analyse und Visualisierung

---

## Projektstruktur

```
/home/user/alm-web/
├── app.py                          # Haupt-Webanwendung (1.032 Zeilen)
├── main_alm_simulation.py          # Simulations-Engine (2.139 Zeilen)
├── optimize_alm_beta.py            # Optimierungs-Algorithmen (1.088 Zeilen)
├── config_alm.py                   # Konfigurations-Parameter (95 Zeilen)
├── functions.R                     # R-Analyse-Funktionen (1.138 Zeilen)
├── functions_parallel.R            # Parallelisierte R-Analyse (565 Zeilen)
├── templates/
│   └── index.html                  # Web-Interface Frontend (86KB)
├── data/                           # Ausgabe-Verzeichnis für Ergebnisse
└── static/                         # Statische Dateien
```

---

## Kern-Skripte

### app.py - Web-Server

**Zweck:** Haupt-FastAPI-Webanwendung mit REST-API und WebSocket-Interface

#### Hauptfunktionen

##### Server-Endpunkte

```python
@app.get("/")
async def index()
```
Liefert die Haupt-HTML-Seite der Webanwendung.

```python
@app.get("/api/config")
async def get_config()
```
Gibt die aktuelle Simulations-Konfiguration als JSON zurück.

```python
@app.get("/api/status")
async def get_status()
```
Gibt den Status der laufenden Optimierung zurück (läuft/gestoppt).

```python
@app.get("/api/files")
async def list_files()
```
Listet alle generierten Datendateien im `data/` Verzeichnis auf.

```python
@app.get("/api/download/{filename}")
async def download_file(filename: str)
```
Lädt eine spezifische Ergebnisdatei herunter.

```python
@app.post("/api/upload-population")
async def upload_population(file: UploadFile)
```
Lädt eine benutzerdefinierte Populations-CSV-Datei hoch.

```python
@app.post("/api/stop")
async def stop_optimization()
```
Stoppt die laufende Optimierung oder Simulation.

```python
@app.post("/api/generate-population")
async def generate_population(n_persons: int = 10000)
```
Generiert synthetische Populationsdaten für Tests.

```python
@app.get("/api/chart-data/{filename}")
async def get_chart_data(filename: str)
```
Ruft JSON-Chartdaten für Visualisierungen ab.

```python
@app.get("/api/analysis-files")
async def list_analysis_files()
```
Listet alle Analyse-CSV-Dateien auf.

```python
@app.get("/api/analysis/{filename}")
async def get_analysis_data(filename: str)
```
Gibt Analysedaten mit Box-Plot-Statistiken zurück.

```python
@app.get("/api/analysis/{filename}/path/{path_nr}")
async def get_path_analysis(filename: str, path_nr: int)
```
Liefert detaillierte Analyse für einen spezifischen Simulationspfad.

```python
@app.get("/api/latest-chart")
async def get_latest_chart()
```
Ruft die aktuellsten Chart-Daten ab.

##### WebSocket-Kommunikation

```python
@app.websocket("/ws")
async def websocket_endpoint(websocket: WebSocket)
```
Echtzeit-WebSocket-Verbindung für Live-Updates während der Simulation.

```python
async def broadcast_message(message: str)
```
Sendet Nachrichten an alle verbundenen WebSocket-Clients.

##### Kern-Funktionalität

```python
async def run_optimization(mode: str, method: str, params: dict)
```
Orchestriert den Optimierungsprozess:
- Validiert und konvertiert Parameter
- Generiert `config_alm.py`
- Führt Optimierung mit gewählter Methode aus (Bayesian/Grid/Random)
- Streamt Fortschritt über WebSocket

```python
async def run_simulation(params: dict)
```
Führt eine einzelne Monte-Carlo-Simulation aus:
- Validiert Parameter
- Generiert Konfigurationsdatei
- Führt Simulation aus
- Exportiert Ergebnisse als CSV und JSON

```python
def generate_config_file(params: dict) -> str
```
Generiert `config_alm.py` aus den übergebenen Parametern mit:
- Typ-Konvertierung (int/float/bool)
- Listen- und Tupel-Formatierung
- Validierung der Parameterstruktur

#### Standard-Konfiguration

Das Skript definiert eine umfassende Standard-Konfiguration mit über 60 Parametern, organisiert in Kategorien:

- **Simulations-Parameter:** N_PATHS, T_HORIZON, RANDOM_SEED
- **Kapitalmarkt-Erwartungen:** MU (Renditen), SIGMA (Volatilitäten), DEGREES_OF_FREEDOM
- **Asset-Gewichte:** Allokation über 6 Asset-Klassen
- **Korrelationsmatrix:** 6×6 Korrelationsstruktur
- **Zinsstruktur:** Zinskurve, Basis-Zins-Floor
- **Anleihen-Parameter:** Duration-Modi, Kreditspread, Ausfallwahrscheinlichkeit
- **Verbindlichkeiten-Parameter:** Technischer Zins, Duration, Discount-Spread
- **Risikomanagement:** Zinsrate-Caps, spezielle Rentenauszahlungen
- **Populationsdaten-Pfade:** Sterbetafel und Populationsdateien

#### Wichtige Features

- **Multi-Threading:** Unterstützt parallele Simulation mit konfigurierbarer Prozesszahl
- **Parameter-Validierung:** Typ-Konvertierung und Constraints-Prüfung
- **Fehlerbehandlung:** Robuste Exception-Handling für alle Endpunkte
- **Datei-Management:** Automatische Verwaltung von Upload/Download-Dateien
- **Logging:** Detailliertes Logging für Debugging

---

### main_alm_simulation.py - Simulations-Engine

**Zweck:** Führt Monte-Carlo-Simulationen für Pensionskassen-Verbindlichkeiten aus

#### Hauptfunktionen

##### Daten-Laden und Vorbereitung

```python
def load_data(mortality_table_path: str, population_path: str) -> tuple
```
Lädt Sterbetafeln und Populationsdaten aus CSV-Dateien.

**Returns:** `(mort_table_df, pop_df)`

```python
def compute_initial_population_stats(pop_df: pd.DataFrame) -> dict
```
Berechnet Populations-Statistiken:
- Gesamt-Anzahl Versicherte
- Durchschnittsalter
- Geschlechterverteilung
- Durchschnittliche Jahresrente
- Verteilung nach Rentner/Witwe/Waise

```python
def get_qx(mort_df: pd.DataFrame, age: int, gender: str) -> float
```
Gibt die Sterbewahrscheinlichkeit für ein bestimmtes Alter und Geschlecht zurück.

##### Vorberechnung für Performance

```python
def precompute_mortality_arrays(mort_df: pd.DataFrame, max_age: int = 120) -> tuple
```
Vorberechnung von Sterbewahrscheinlichkeiten für schnelleren Zugriff.

**Returns:** `(qx_male_array, qx_female_array)`

```python
def precompute_annuity_factors(zins: float, max_age: int, mort_df: pd.DataFrame) -> tuple
```
Berechnet Annuitäts-Faktoren für alle Altersgruppen und Geschlechter.

```python
def precompute_survival_probs(max_age: int, qx_male: np.ndarray, qx_female: np.ndarray) -> tuple
```
Vorberechnung von Überlebenswahrscheinlichkeiten für mehrere Jahre.

##### Verbindlichkeiten-Berechnung

```python
def calculate_expected_cashflows(pop_df: pd.DataFrame, qx_male: np.ndarray,
                                qx_female: np.ndarray, T: int,
                                special_pension_enabled: bool = False,
                                special_pension_year: int = 0,
                                special_pension_months: float = 0.0) -> tuple
```
Projiziert Pensions-Verbindlichkeiten über den Zeitraum T:
- Berechnet erwartete Cashflows pro Jahr
- Berücksichtigt Sterblichkeit für Versicherte und Ehepartner
- Implementiert spezielle Pensions-Auszahlungen
- Behandelt Rentner, Witwen und Waisen separat

**Returns:** `(cf_array, surviving_pop_array)`

```python
def calculate_cfm_tranches(cf_array: np.ndarray, T: int, yield_curve: list) -> list
```
Erstellt Cash-Flow-Matching-Tranches für Anleihen-Portfolios.

**Returns:** Liste von Dictionaries mit `{year, cf, pv, yield, weight}`

```python
def get_cfm_weighted_duration(cfm_data: list) -> float
```
Berechnet die gewichtete Duration des Anleihen-Portfolios.

```python
def get_cfm_coupon_rate(yield_curve: list, cfm_weighted_duration: float) -> float
```
Bestimmt den Kuponsatz basierend auf der Duration.

##### Duration-Strategien

```python
def get_duration_for_mode(mode: str, liability_duration: float,
                         cfm_weighted_dur: float, fixed_dur: float) -> float
```
Wählt Duration basierend auf der Strategie:
- `"cfm"`: Cash-Flow-Matching (verwendet CFM-Duration)
- `"liability"`: Liability-Matching (verwendet Verbindlichkeiten-Duration)
- `"fixed"`: Feste Duration (verwendet vorgegebenen Wert)

##### Asset-Returns-Simulation

```python
def simulate_all_returns(T: int, MU: tuple, SIGMA: tuple, CORR: tuple,
                        DOF: tuple, mean_reversion_enabled: bool,
                        mu_eq_long_term: float, mean_rev_speed: float,
                        seed: int = None) -> np.ndarray
```
Generiert korrelierte Asset-Returns mit:
- **Cholesky-Zerlegung** für Korrelationen
- **Student-t-Verteilungen** für Fat-Tail-Risiken
- **Mean-Reversion** für Aktien-Returns
- 6 Asset-Klassen gleichzeitig

**Returns:** `returns_arr` mit Shape (T, 6)

##### Barwert-Berechnungen

```python
def calculate_liability_barwert_base(zins: float, cf_array: np.ndarray) -> float
```
Berechnet den Barwert der Verbindlichkeiten mit gegebenem Zinssatz.

```python
def calculate_liability_duration(zins: float, cf_array: np.ndarray, PV: float) -> float
```
Berechnet die Macaulay-Duration der Verbindlichkeiten.

```python
def calculate_liability_barwert_initial(zins: float, cf_array: np.ndarray,
                                       duration_discount_spread: float) -> float
```
Berechnet den initialen Barwert mit Duration-Discount-Spread.

##### Monte-Carlo-Simulation

```python
def run_monte_carlo_path_full(path_nr: int, T: int, returns_arr: np.ndarray,
                              initial_vermögen: float, weights: tuple,
                              cf_array: np.ndarray, liability_barwert_0: float,
                              durations_gov: float, durations_corp: float,
                              coupon_rate_gov: float, coupon_rate_corp: float,
                              basis_zins_floor: float, zinskurve: list,
                              spread_corp: float, corp_default_prob: float,
                              interest_rate_cap_enabled: bool,
                              interest_rate_cap_strike: float,
                              interest_rate_cap_notional_fraction: float) -> pd.DataFrame
```
Führt einen einzelnen Monte-Carlo-Pfad aus mit:
- **Asset-Evolution** über alle 6 Klassen
- **Duration-angepasste Anleihen-Returns**
- **Kupon-Zahlungen** für Staatsanleihen und Unternehmensanleihen
- **Corporate-Bond-Defaults**
- **Zinsrate-Caps** für Hedging
- **Verbindlichkeiten-Bewertung** mit aktuellen Zinsen
- **Deckungsgrad-Berechnung** (Vermögen/Verbindlichkeiten)

**Returns:** DataFrame mit jährlichen Werten für alle Variablen

##### Ergebnis-Analyse

```python
def analyze_results(all_paths_df: pd.DataFrame) -> dict
```
Analysiert alle Simulationsergebnisse:
- Finale Deckungsgrade pro Pfad
- Durchschnitt, Median, Standardabweichung
- Perzentile (5%, 25%, 75%, 95%)
- Min/Max-Werte

```python
def calculate_risk_metrics(all_paths_df: pd.DataFrame, T: int) -> dict
```
Berechnet Risiko-Metriken:
- **Unterdeckungs-Wahrscheinlichkeit:** P(Deckungsgrad < 100%)
- **Default-Wahrscheinlichkeit:** P(Deckungsgrad < 80%)
- **Erwarteter Shortfall:** Durchschnittliche Unterdeckung bei Default
- **Worst-Case-Szenarien:** Niedrigste Deckungsgrade

##### Export und Visualisierung

```python
def export_all_paths_csv(all_paths_df: pd.DataFrame, filename: str)
```
Exportiert alle Simulationspfade als CSV-Datei.

```python
def export_paths_json(all_paths_df: pd.DataFrame, filename: str,
                     max_paths_to_plot: int = 20)
```
Exportiert Pfade als JSON für Web-Visualisierung.

```python
def plot_deckungsgrad_evolution(all_paths_df: pd.DataFrame,
                               output_png_path: str, max_paths: int = 50)
```
Erstellt Matplotlib-Plot der Deckungsgrad-Evolution über Zeit.

#### Technische Details

**Asset-Klassen:**
1. **Zinsen (Interest Rate):** Basis-Zinssatz mit Floor
2. **Staatsanleihen (Gov Bonds):** Duration 8-25 Jahre, niedriges Risiko
3. **Unternehmensanleihen (Corp Bonds):** Duration 3-12 Jahre, Kreditrisiko
4. **Aktien (Equities):** Höchste Returns, höchste Volatilität, Mean-Reversion
5. **Immobilien (Real Estate):** Moderate Returns, moderate Volatilität
6. **Alternative (Alternatives):** Diversifikation, geringe Korrelation

**Sterblichkeits-Modellierung:**
- Verwendet DAV 2004 R Sterbetafeln (oder ähnlich)
- Geschlechts-spezifische Sterbewahrscheinlichkeiten
- Ehepartner-Überlebensleistungen
- Waisen-Renten (temporär)

**Performance-Optimierungen:**
- Numpy-Arrays für numerische Berechnungen
- Vorberechnete Lookup-Tabellen
- Multiprocessing für parallele Pfade
- Effiziente DataFrame-Operationen

---

### optimize_alm_beta.py - Optimierungs-Algorithmen

**Zweck:** Optimiert Asset-Allokation und Anleihen-Duration-Parameter

#### Hauptfunktionen

##### Adaptive Pfad-Berechnung

```python
def calculate_adaptive_n_paths(weights: tuple, sigma: tuple, corr: tuple,
                              base_n_paths: int = 200,
                              min_n_paths: int = 100,
                              max_n_paths: int = 500) -> int
```
Passt die Anzahl der Simulationspfade basierend auf Portfolio-Volatilität an:
- Höhere Volatilität → mehr Pfade für Stabilität
- Niedrigere Volatilität → weniger Pfade für Geschwindigkeit

**Volatilitäts-Kategorien:**
- **Niedrig (< 2.5%):** min_n_paths
- **Mittel (2.5% - 4.5%):** base_n_paths
- **Hoch (> 4.5%):** max_n_paths

```python
def get_volatility_category(port_vol: float) -> str
```
Kategorisiert Portfolio-Volatilität als "low", "medium" oder "high".

##### Parameter-Validierung

```python
def validate_weights(weights: tuple, tolerance: float = 1e-4) -> bool
```
Validiert, dass Asset-Gewichte:
- Zwischen 0.0 und 1.0 liegen
- Summe = 1.0 (innerhalb Toleranz)
- Korrekte Anzahl (6 Asset-Klassen)

```python
def _round_params_for_key(params: dict) -> tuple
```
Rundet Parameter für konsistente Cache-Keys.

##### Logging und Monitoring

```python
def log_evaluation(iteration: int, params: dict, objective_value: float,
                  risk_metrics: dict)
```
Protokolliert jeden Optimierungs-Schritt mit:
- Iterations-Nummer
- Parameter-Werte
- Zielfunktions-Wert
- Risiko-Metriken (Unterdeckung, Default, Shortfall)

```python
def summarize_initial_population(pop_df: pd.DataFrame) -> dict
```
Extrahiert Populations-Statistiken für Logging.

##### Validierung optimaler Kandidaten

```python
def final_validate_zero_objective_candidates(candidates: list) -> list
```
Validiert Kandidaten mit Zielfunktions-Wert = 0:
- Führt Simulations erneut mit mehr Pfaden aus
- Verifiziert Stabilität der Lösung
- Filtert falsch-positive Ergebnisse

#### Simulations-Ausführung

```python
def run_simulation_with_params(params: dict, n_paths: int) -> tuple
```
Führt vollständige Simulation mit gegebenen Parametern aus:
1. Lädt Daten (Sterbetafel, Population)
2. Berechnet erwartete Cashflows
3. Simuliert Returns
4. Führt Monte-Carlo-Pfade aus (optional parallel)
5. Berechnet Risiko-Metriken

**Returns:** `(risk_metrics_dict, all_paths_df)`

#### Zielfunktion

```python
def objective_function(params_array: np.ndarray, param_names: list,
                      base_config: dict, n_paths: int,
                      use_cache: bool = True) -> float
```
Multi-objektive Zielfunktion:

**Formel:**
```
Objective = P(Unterdeckung) + 5.0 × P(Default) + 0.00001 × E[Shortfall]
```

**Komponenten:**
- **P(Unterdeckung):** Wahrscheinlichkeit Deckungsgrad < 100%
- **P(Default):** Wahrscheinlichkeit Deckungsgrad < 80% (5× gewichtet)
- **E[Shortfall]:** Erwarteter Fehlbetrag bei Default

**Features:**
- Parameter-Caching für identische Konfigurationen
- Constraint-Handling (ungültige Gewichte → hohe Penalty)
- Exception-Handling für numerische Fehler

#### Optimierungs-Methoden

##### Bayesianische Optimierung

```python
def optimize_bayesian(base_config: dict, n_iterations: int = 50,
                     n_paths: int = 200) -> dict
```
Verwendet `scipy.optimize.minimize` mit SLSQP-Algorithmus:

**Optimierungs-Parameter:**
- `w_gov`: Staatsanleihen-Gewicht (30% - 80%)
- `w_corp`: Unternehmensanleihen-Gewicht (5% - 40%)
- `w_eq`: Aktien-Gewicht (0% - 20%)
- `w_re`: Immobilien-Gewicht (0% - 25%)
- `dur_gov`: Staatsanleihen-Duration (8 - 25 Jahre)
- `dur_corp`: Unternehmensanleihen-Duration (3 - 12 Jahre)

**Constraints:**
```python
{'type': 'eq', 'fun': lambda x: x[0] + x[1] + x[2] + x[3] + x[4] + x[5] - 1.0}
```
(Summe aller Gewichte = 1.0)

**Bounds:**
Individuell für jeden Parameter definiert.

**Prozess:**
1. Startet mit initialen Parametern aus `base_config`
2. Iteriert mit SLSQP-Optimizer
3. Validiert Kandidaten mit hoher Qualität
4. Gibt beste Lösung zurück

##### Grid Search

```python
def optimize_grid_search(base_config: dict, n_paths: int = 200) -> dict
```
Systematische Suche über diskretes Parameter-Gitter:

**Grid-Definition:**
```python
w_gov_vals = [0.40, 0.50, 0.60, 0.70]
w_corp_vals = [0.10, 0.20, 0.30]
w_eq_vals = [0.00, 0.05, 0.10, 0.15]
w_re_vals = [0.00, 0.10, 0.20]
dur_gov_vals = [10, 15, 20]
dur_corp_vals = [5, 7, 10]
```

**Kombinationen:** ~720 (mit Gewichts-Constraint-Filterung)

**Prozess:**
1. Generiert alle Kombinationen
2. Filtert ungültige Gewichts-Summen
3. Evaluiert jede Kombination
4. Gibt beste Lösung zurück

##### Random Search

```python
def optimize_random_search(base_config: dict, n_iterations: int = 100,
                          n_paths: int = 200) -> dict
```
Stochastische Suche mit zufälligen Parametern:

**Sampling:**
```python
w_gov = random.uniform(0.30, 0.80)
w_corp = random.uniform(0.05, 0.40)
w_eq = random.uniform(0.00, 0.20)
w_re = random.uniform(0.00, 0.25)
dur_gov = random.uniform(8, 25)
dur_corp = random.uniform(3, 12)
```

**Normalisierung:**
Gewichte werden normalisiert, damit Summe = 1.0

**Prozess:**
1. Generiert `n_iterations` zufällige Kombinationen
2. Normalisiert Gewichte
3. Evaluiert jede Kombination
4. Gibt beste Lösung zurück

#### Haupt-Orchestrierung

```python
def main(mode: str = "bayesian", n_iterations: int = 50, n_paths: int = 200)
```
Haupt-Einstiegspunkt für Optimierung:

**Workflow:**
1. Lädt `config_alm` Konfiguration
2. Konvertiert zu Dictionary
3. Wählt Optimierungs-Methode basierend auf `mode`
4. Führt Optimierung aus
5. Loggt Ergebnisse
6. Gibt optimale Parameter zurück

**Unterstützte Modi:**
- `"bayesian"`: Bayesianische Optimierung
- `"grid"`: Grid Search
- `"random"`: Random Search

#### Performance-Features

**Caching:**
- Parameter-Kombinationen werden gecacht
- Vermeidet redundante Simulationen
- Dictionary-basierter In-Memory-Cache

**Parallele Ausführung:**
- Multiprocessing für Monte-Carlo-Pfade
- Konfigurierbare Prozess-Anzahl
- Automatische Workload-Verteilung

**Adaptive Strategien:**
- Pfad-Anzahl basiert auf Portfolio-Risiko
- Mehr Pfade für volatile Portfolios
- Weniger Pfade für stabile Portfolios

---

### config_alm.py - Konfiguration

**Zweck:** Zentrale Konfigurationsdatei für alle Simulations-Parameter

#### Parameter-Kategorien

##### 1. Simulations-Parameter

```python
N_PATHS = 500                    # Anzahl Monte-Carlo-Pfade
T_HORIZON = 40                   # Simulations-Horizont in Jahren
RANDOM_SEED = 42                 # Seed für Reproduzierbarkeit
N_PROCESSES = 4                  # Anzahl paralleler Prozesse
```

##### 2. Kapitalmarkt-Erwartungen

**Erwartete Returns (MU):**
```python
MU = (
    0.02,      # Zinsen (Interest Rate)
    0.02,      # Staatsanleihen (Gov Bonds)
    0.025,     # Unternehmensanleihen (Corp Bonds)
    0.055,     # Aktien (Equities) - 5.5% p.a.
    0.025,     # Immobilien (Real Estate) - 2.5% p.a.
    0.03       # Alternative - 3.0% p.a.
)
```

**Volatilitäten (SIGMA):**
```python
SIGMA = (
    0.01,      # Zinsen - 1% Volatilität
    0.03,      # Staatsanleihen - 3%
    0.045,     # Unternehmensanleihen - 4.5%
    0.150,     # Aktien - 15% Volatilität
    0.08,      # Immobilien - 8%
    0.10       # Alternative - 10%
)
```

**Freiheitsgrade (Student-t):**
```python
DEGREES_OF_FREEDOM = (
    30,        # Zinsen
    30,        # Staatsanleihen
    15,        # Unternehmensanleihen
    5,         # Aktien - Fat Tails
    10,        # Immobilien
    8          # Alternative
)
```

##### 3. Asset-Gewichte

```python
WEIGHT_InterestRate = 0.0        # Keine direkte Zins-Investition
WEIGHT_GovBonds = 0.65           # 65% Staatsanleihen
WEIGHT_CorpBonds = 0.23          # 23% Unternehmensanleihen
WEIGHT_Equities = 0.05           # 5% Aktien
WEIGHT_RealEstate = 0.05         # 5% Immobilien
WEIGHT_Alternatives = 0.02       # 2% Alternative

WEIGHTS = (WEIGHT_InterestRate, WEIGHT_GovBonds, WEIGHT_CorpBonds,
           WEIGHT_Equities, WEIGHT_RealEstate, WEIGHT_Alternatives)
```

##### 4. Korrelationsmatrix

```python
CORRELATION_MATRIX = (
    (1.00, 0.20, 0.15, -0.30, 0.10, 0.00),   # Zinsen
    (0.20, 1.00, 0.75,  0.10, 0.30, 0.20),   # Staatsanleihen
    (0.15, 0.75, 1.00,  0.20, 0.35, 0.25),   # Unternehmensanleihen
    (-0.30, 0.10, 0.20, 1.00, 0.50, 0.40),   # Aktien
    (0.10, 0.30, 0.35,  0.50, 1.00, 0.50),   # Immobilien
    (0.00, 0.20, 0.25,  0.40, 0.50, 1.00)    # Alternative
)
```

##### 5. Zinsstruktur

```python
BASIS_ZINS_T0 = 0.02             # Initialer Basis-Zinssatz 2%
BASIS_ZINS_FLOOR = 0.00          # Zins-Untergrenze 0%

YIELD_CURVE = [                  # Zinskurve für verschiedene Laufzeiten
    0.015, 0.018, 0.020, 0.022, 0.023, 0.024, 0.025, 0.026,
    0.027, 0.028, 0.028, 0.029, 0.029, 0.030, 0.030, 0.030,
    # ... bis 40 Jahre
]
```

##### 6. Anleihen-Parameter

**Staatsanleihen:**
```python
DURATION_MODE_GOV = "cfm"        # "cfm", "liability" oder "fixed"
FIXED_DURATION_GOV = 15.0        # Falls "fixed" Modus
```

**Unternehmensanleihen:**
```python
DURATION_MODE_CORP = "fixed"
FIXED_DURATION_CORP = 7.0
CORPORATE_SPREAD = 0.005         # 50 Basispunkte Kreditspread
CORPORATE_DEFAULT_PROB = 0.001   # 0.1% jährliche Ausfallwahrscheinlichkeit
```

##### 7. Verbindlichkeiten-Parameter

```python
TECHNICAL_RATE = 0.02            # Technischer Zinssatz 2%
TECHNICAL_RATE_DURATION = 12.0   # Verbindlichkeiten-Duration
DURATION_DISCOUNT_SPREAD = 0.0   # Zusätzlicher Discount-Spread
```

##### 8. Aktien-Mean-Reversion

```python
MEAN_REVERSION_ENABLED = True
MU_EQUITIES_LONG_TERM = 0.055    # Langfristige Renditeerwartung
MEAN_REVERSION_SPEED = 0.20      # Geschwindigkeit der Rückkehr zum Mittel
```

##### 9. Risikomanagement

**Zinsrate-Caps:**
```python
INTEREST_RATE_CAP_ENABLED = False
INTEREST_RATE_CAP_STRIKE = 0.03           # Strike bei 3%
INTEREST_RATE_CAP_NOTIONAL_FRACTION = 0.5 # 50% der Verbindlichkeiten
```

**Spezielle Pensions-Auszahlungen:**
```python
SPECIAL_PENSION_ENABLED = False
SPECIAL_PENSION_YEAR = 5                  # Jahr der Auszahlung
SPECIAL_PENSION_MONTHS_EQUIVALENT = 2.0   # 2 Monatsrenten
```

##### 10. Datenpfade

```python
MORTALITY_TABLE_PATH = "data/DAV2004R.csv"
POPULATION_PATH = "data/population.csv"
```

#### Verwendung

Die Konfigurationsdatei wird dynamisch von `app.py` generiert basierend auf Benutzer-Eingaben. Sie kann auch manuell bearbeitet werden für Batch-Simulationen.

**Import in anderen Skripten:**
```python
import config_alm as config

n_paths = config.N_PATHS
weights = config.WEIGHTS
```

---

### functions.R - R-Analyse-Suite

**Zweck:** Post-Simulations-Datenanalyse und Visualisierung in R

#### Haupt-Funktionalitäten

##### 1. Daten-Import

```r
load_mc_data <- function(data_dir = "data/")
```
Lädt alle Monte-Carlo-Simulations-CSV-Dateien:
- Findet alle CSV-Dateien im Verzeichnis
- Kombiniert zu einem einzigen Data Frame
- Konvertiert Spalten zu numerischen Typen
- Fügt Pfad-Identifikatoren hinzu

**Typische Datenmenge:** 1,5M+ Zeilen mit 80+ Variablen

##### 2. Daten-Preprocessing

**Spalten-Typ-Konvertierung:**
```r
convert_numeric_columns <- function(df)
```
Konvertiert alle numerischen Spalten von Character zu Numeric.

**Pfad-Identifikation:**
```r
identify_paths <- function(df)
```
Extrahiert Pfad-Nummern aus Dateinamen oder Index.

##### 3. Statistische Analyse

**Aggregation:**
```r
aggregate_by_year <- function(df)
```
Aggregiert Metriken pro Jahr über alle Pfade:
- Mittelwert
- Median
- Standardabweichung
- Quantile (5%, 25%, 75%, 95%)

**Risiko-Metriken:**
```r
calculate_risk_metrics <- function(df)
```
Berechnet:
- Unterdeckungs-Wahrscheinlichkeit pro Jahr
- Default-Wahrscheinlichkeit
- Value-at-Risk (VaR)
- Conditional Value-at-Risk (CVaR)

##### 4. Visualisierung

**Deckungsgrad-Evolution:**
```r
plot_deckungsgrad <- function(df, max_paths = 50)
```
Erstellt Spaghetti-Plot der Deckungsgrad-Entwicklung.

**Verteilungs-Plots:**
```r
plot_distribution <- function(df, year)
```
Histogramm und Dichte-Plot für spezifisches Jahr.

**Box-Plots:**
```r
create_boxplots <- function(df, variables)
```
Box-Plots für Schlüssel-Variablen über Zeit.

##### 5. Export

**Summary-Statistiken:**
```r
export_summary <- function(df, filename)
```
Exportiert aggregierte Statistiken als CSV.

**Plots:**
```r
save_plot <- function(plot, filename, width = 12, height = 8)
```
Speichert ggplot2-Grafiken als PNG/PDF.

#### Verwendete R-Packages

```r
library(data.table)    # Schnelle Daten-Manipulation
library(dplyr)         # Daten-Transformation
library(tidyr)         # Daten-Tidying
library(ggplot2)       # Visualisierung
library(readr)         # CSV-Import
library(lubridate)     # Datums-Handling
```

#### Typischer Workflow

```r
# 1. Laden
df <- load_mc_data("data/")

# 2. Analysieren
stats <- aggregate_by_year(df)
risks <- calculate_risk_metrics(df)

# 3. Visualisieren
plot_deckungsgrad(df, max_paths = 100)
create_boxplots(df, c("Deckungsgrad", "Vermögen", "Verbindlichkeiten"))

# 4. Exportieren
export_summary(stats, "summary_statistics.csv")
```

---

### functions_parallel.R - Parallelisierte R-Analyse

**Zweck:** Optimierte R-Funktionen mit Parallel-Processing

#### Haupt-Funktionen

##### Parallel-Initialisierung

```r
init_parallel <- function(n_cores = NULL)
```
Initialisiert parallele Verarbeitung:
- Erkennt verfügbare CPU-Kerne
- Standard: Alle Kerne - 1
- Erstellt Worker-Pool mit `doParallel`
- Registriert Backend für `foreach`

**Verwendung:**
```r
init_parallel()              # Auto-detect cores
init_parallel(n_cores = 4)   # Spezifische Anzahl
```

##### Parallel-Beendigung

```r
stop_parallel <- function()
```
Beendet parallele Worker:
- Stoppt Cluster
- Gibt Ressourcen frei
- Wichtig für Clean-up

##### Parallel-Daten-Laden

```r
load_mc_data <- function(data_dir = "data/", parallel = TRUE)
```
Lädt CSV-Dateien parallel:
- Verteilt Dateien auf Worker
- Parallele `fread()` mit `data.table`
- Kombiniert Ergebnisse mit `rbindlist()`
- Bis zu 4× schneller als sequentiell

**Verwendung:**
```r
# Parallel (Standard)
df <- load_mc_data("data/")

# Sequentiell
df <- load_mc_data("data/", parallel = FALSE)
```

##### Parallel-Aggregation

```r
aggregate_by_year_parallel <- function(df)
```
Parallele Aggregation über Jahre:
- Teilt Daten nach Jahren
- Berechnet Statistiken parallel
- Kombiniert Ergebnisse

**Speedup:** 3-6× schneller für große Datasets

##### Parallel-Plotting

```r
create_plots_parallel <- function(df, output_dir = "plots/")
```
Erstellt mehrere Plots parallel:
- Jeder Plot auf separatem Worker
- Gleichzeitige Generierung
- Automatisches Speichern

#### Verwendete Packages

```r
library(data.table)      # Schnelle Daten-Operationen
library(doParallel)      # Parallel-Backend
library(foreach)         # Parallel-Loops
library(parallel)        # Basis-Parallelität
```

#### Performance-Vergleich

**Sequentiell vs. Parallel (Beispiel-Daten):**

| Operation | Sequentiell | Parallel (8 Cores) | Speedup |
|-----------|-------------|-------------------|---------|
| Load 50 CSVs (500MB) | 45s | 12s | 3.75× |
| Aggregate by Year | 30s | 6s | 5.0× |
| Generate 20 Plots | 60s | 10s | 6.0× |

#### Typischer Workflow

```r
# 1. Initialisieren
init_parallel(n_cores = 8)

# 2. Parallel laden
df <- load_mc_data("data/", parallel = TRUE)

# 3. Parallel analysieren
stats <- aggregate_by_year_parallel(df)

# 4. Parallel visualisieren
create_plots_parallel(df, "plots/")

# 5. Clean-up
stop_parallel()
```

#### Best Practices

**Wann Parallel-Processing verwenden:**
- Große Datasets (> 100MB)
- Viele unabhängige Operationen
- CPU-intensive Berechnungen
- Mehr als 4 CPU-Kerne verfügbar

**Wann NICHT verwenden:**
- Kleine Datasets (< 10MB)
- Overhead überwiegt Benefit
- I/O-bound Operationen
- Wenige CPU-Kerne (< 4)

**Memory Management:**
```r
# Explizite Garbage Collection
gc()

# Memory-effiziente data.table Operationen
setDTthreads(threads = 4)
```

---

## Installation

### Voraussetzungen

**Python (>= 3.8):**
```bash
python --version
```

**R (>= 4.0):**
```bash
R --version
```

### Python-Dependencies

```bash
pip install fastapi uvicorn pandas numpy scipy matplotlib websockets python-multipart
```

**Detaillierte Anforderungen:**
- `fastapi`: Web-Framework
- `uvicorn`: ASGI-Server
- `pandas`: Daten-Manipulation
- `numpy`: Numerische Berechnungen
- `scipy`: Optimierungs-Algorithmen
- `matplotlib`: Visualisierung
- `websockets`: Echtzeit-Kommunikation
- `python-multipart`: Datei-Upload

### R-Packages

```r
install.packages(c("data.table", "dplyr", "tidyr", "ggplot2",
                   "readr", "doParallel", "foreach"))
```

### Daten-Vorbereitung

**Sterbetafel (DAV2004R.csv):**
```csv
Alter,qx_male,qx_female
0,0.00512,0.00412
1,0.00035,0.00029
...
120,1.00000,1.00000
```

**Population (population.csv):**
```csv
id,alter,geschlecht,status,jahresrente,alter_partner
1,65,M,Rentner,24000,62
2,70,F,Rentnerin,18000,
...
```

---

## Verwendung

### Web-Interface starten

```bash
cd /home/user/alm-web
python app.py
```

**Server läuft auf:** `http://localhost:8000`

### Simulation ausführen

1. Öffne Browser: `http://localhost:8000`
2. Wähle **"Simulation"** Modus
3. Konfiguriere Parameter in Tabs:
   - Simulation (N_PATHS, T_HORIZON)
   - Kapitalmarkt (MU, SIGMA, DOF)
   - Asset-Gewichte
   - Korrelationsmatrix
   - Anleihen-Strategien
   - Risikomanagement
4. Klicke **"Start Simulation"**
5. Beobachte Echtzeit-Konsole
6. Lade Ergebnisse herunter

### Optimierung ausführen

1. Wähle **"Optimization"** Modus
2. Wähle Methode:
   - **Bayesian:** Beste Qualität, langsam
   - **Grid Search:** Systematisch, mittel
   - **Random Search:** Schnell, explorativ
3. Setze Iterations-Anzahl
4. Konfiguriere Basis-Parameter
5. Klicke **"Start Optimization"**
6. Warte auf optimale Lösung
7. Lade Ergebnisse herunter

### Kommandozeilen-Verwendung

**Simulation:**
```bash
python main_alm_simulation.py
```

**Optimierung:**
```bash
python optimize_alm_beta.py --mode bayesian --iterations 50 --paths 200
```

### R-Analyse

```r
# R-Konsole oder RStudio
source("functions_parallel.R")

init_parallel()
df <- load_mc_data("data/")
stats <- aggregate_by_year_parallel(df)
risks <- calculate_risk_metrics(df)
stop_parallel()
```

---

## API-Endpunkte

### REST-Endpunkte

#### GET /
Liefert Haupt-HTML-Seite.

#### GET /api/config
Gibt aktuelle Konfiguration als JSON zurück.

**Response:**
```json
{
  "N_PATHS": 500,
  "T_HORIZON": 40,
  "WEIGHTS": [0.0, 0.65, 0.23, 0.05, 0.05, 0.02],
  ...
}
```

#### GET /api/status
Gibt Optimierungs-Status zurück.

**Response:**
```json
{
  "running": true
}
```

#### GET /api/files
Listet alle Datendateien auf.

**Response:**
```json
{
  "files": [
    "mc_results_20260122_143022.csv",
    "chart_data_20260122_143022.json",
    ...
  ]
}
```

#### GET /api/download/{filename}
Lädt Datei herunter.

**Beispiel:** `/api/download/mc_results_20260122_143022.csv`

#### POST /api/upload-population
Lädt Population-CSV hoch.

**Form-Data:**
- `file`: CSV-Datei

#### POST /api/stop
Stoppt laufende Optimierung.

#### POST /api/generate-population
Generiert synthetische Population.

**Query-Parameter:**
- `n_persons`: Anzahl Personen (default: 10000)

#### GET /api/chart-data/{filename}
Gibt Chart-Daten als JSON zurück.

#### GET /api/analysis-files
Listet Analyse-CSV-Dateien auf.

#### GET /api/analysis/{filename}
Gibt Analyse-Daten mit Box-Plot-Statistiken zurück.

**Response:**
```json
{
  "years": [0, 1, 2, ...],
  "variables": {
    "Deckungsgrad": {
      "mean": [...],
      "median": [...],
      "q25": [...],
      "q75": [...],
      "min": [...],
      "max": [...]
    },
    ...
  }
}
```

#### GET /api/analysis/{filename}/path/{path_nr}
Gibt detaillierte Analyse für spezifischen Pfad zurück.

**Response:**
```json
{
  "path_nr": 5,
  "years": [0, 1, 2, ...],
  "Deckungsgrad": [...],
  "Vermögen": [...],
  ...
}
```

#### GET /api/latest-chart
Gibt neueste Chart-Daten zurück.

### WebSocket-Endpunkt

#### WS /ws
Echtzeit-Verbindung für Live-Updates.

**Nachrichten-Format:**
```json
{
  "type": "log",
  "message": "Starting simulation..."
}
```

**Verbindung (JavaScript):**
```javascript
const ws = new WebSocket('ws://localhost:8000/ws');
ws.onmessage = (event) => {
  console.log(event.data);
};
```

---

## Standard-Konfiguration

### Simulations-Parameter

| Parameter | Wert | Beschreibung |
|-----------|------|--------------|
| `N_PATHS` | 500 | Anzahl Monte-Carlo-Pfade |
| `T_HORIZON` | 40 | Simulations-Horizont (Jahre) |
| `RANDOM_SEED` | 42 | Reproduzierbarkeits-Seed |
| `N_PROCESSES` | 4 | Parallele Prozesse |

### Asset-Allokation (Standard)

| Asset-Klasse | Gewicht | Erwartete Rendite | Volatilität |
|--------------|---------|-------------------|-------------|
| Zinsen | 0% | 2.0% | 1.0% |
| Staatsanleihen | 65% | 2.0% | 3.0% |
| Unternehmensanleihen | 23% | 2.5% | 4.5% |
| Aktien | 5% | 5.5% | 15.0% |
| Immobilien | 5% | 2.5% | 8.0% |
| Alternative | 2% | 3.0% | 10.0% |

### Anleihen-Strategien

| Parameter | Wert |
|-----------|------|
| Staatsanleihen-Duration | CFM (Cash-Flow-Matching) |
| Unternehmensanleihen-Duration | 7 Jahre (Fixed) |
| Kreditspread | 0.5% (50 bps) |
| Ausfallwahrscheinlichkeit | 0.1% p.a. |

### Verbindlichkeiten

| Parameter | Wert |
|-----------|------|
| Technischer Zinssatz | 2.0% |
| Duration | 12 Jahre |
| Discount-Spread | 0.0% |

### Risikomanagement

| Feature | Status | Parameter |
|---------|--------|-----------|
| Zinsrate-Caps | Deaktiviert | Strike: 3%, Notional: 50% |
| Spezielle Pensionen | Deaktiviert | Jahr: 5, Monate: 2.0 |
| Mean-Reversion (Aktien) | Aktiviert | Speed: 0.20 |

---

## Technische Details

### Architektur

```
┌─────────────────────────────────────────────────────────┐
│                    Web Browser (Client)                  │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │
│  │ HTML/CSS/JS  │  │ WebSocket    │  │ Chart.js     │  │
│  └──────────────┘  └──────────────┘  └──────────────┘  │
└─────────────────────────────────────────────────────────┘
                           │ HTTP/WS
                           ▼
┌─────────────────────────────────────────────────────────┐
│                FastAPI Server (app.py)                   │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │
│  │ REST API     │  │ WebSocket    │  │ File Handler │  │
│  └──────────────┘  └──────────────┘  └──────────────┘  │
└─────────────────────────────────────────────────────────┘
                           │
        ┌──────────────────┼──────────────────┐
        ▼                  ▼                  ▼
┌────────────────┐  ┌────────────────┐  ┌──────────────┐
│ Simulation     │  │ Optimization   │  │ Config Gen   │
│ Engine         │  │ Engine         │  │              │
│ (main_alm...)  │  │ (optimize_...) │  │ (config_alm) │
└────────────────┘  └────────────────┘  └──────────────┘
        │                  │                  │
        └──────────────────┼──────────────────┘
                           ▼
                    ┌──────────────┐
                    │  Data Files  │
                    │  (.csv/.json)│
                    └──────────────┘
                           │
                           ▼
                    ┌──────────────┐
                    │  R Analysis  │
                    │  (functions) │
                    └──────────────┘
```

### Algorithmen

#### Monte-Carlo-Simulation

**Schritte:**
1. **Initialisierung:** Lade Daten, setze Parameter
2. **Return-Generierung:** Cholesky + Student-t + Mean-Reversion
3. **Pfad-Simulation:** Für jeden Pfad:
   - Berechne Asset-Returns
   - Update Portfolio-Werte
   - Berechne Verbindlichkeiten
   - Berechne Deckungsgrad
4. **Aggregation:** Sammle alle Pfade
5. **Analyse:** Berechne Risiko-Metriken
6. **Export:** CSV + JSON

**Mathematik:**
```
R[t] = μ + L × Z[t]
L × L^T = Σ  (Cholesky-Zerlegung)
Z[t] ~ t_ν  (Student-t mit ν Freiheitsgraden)
```

#### Bayesianische Optimierung

**Algorithmus:** SLSQP (Sequential Least Squares Programming)

**Objective:**
```
min f(w, d) = P(Unterdeckung) + 5×P(Default) + 10^-5×E[Shortfall]
```

**Constraints:**
```
g(w) = Σw_i - 1 = 0  (Gewichte summieren zu 1)
w_i ∈ [lb_i, ub_i]   (Bounds pro Asset-Klasse)
```

**Prozess:**
1. Start mit initialem w₀, d₀
2. Berechne ∇f (Gradient via finite differences)
3. Löse quadratisches Sub-Problem
4. Update Parameter: w_{k+1} = w_k + α×Δw
5. Wiederhole bis Konvergenz

#### Cash-Flow-Matching

**Ziel:** Anleihen-Cashflows matchen Verbindlichkeiten-Cashflows

**Methode:**
1. Projiziere Verbindlichkeiten-Cashflows: CF_L[t]
2. Diskontiere mit Zinskurve: PV[t] = CF_L[t] / (1 + y[t])^t
3. Berechne Gewichte: w[t] = PV[t] / Σ PV[t]
4. Berechne Duration: D = Σ t × w[t]
5. Wähle Anleihen mit Duration ≈ D

### Performance-Optimierungen

**Python:**
- Numpy-Vektorisierung für numerische Operationen
- Multiprocessing für parallele Monte-Carlo-Pfade
- Vorberechnete Lookup-Tabellen für Sterblichkeit
- In-Memory-Caching für wiederholte Simulationen

**R:**
- `data.table` für schnelle Daten-Operationen (5-10× schneller als `data.frame`)
- `doParallel` + `foreach` für Parallel-Processing
- Memory-mapped Dateien für große Datasets
- Lazy-Evaluation für effiziente Pipelines

**Web:**
- WebSocket für Push-Updates (statt Polling)
- Asynchrone FastAPI-Handler
- Streaming-Responses für große Dateien
- Client-seitige Chart-Rendering

### Datei-Formate

**CSV-Output (Monte-Carlo-Ergebnisse):**
```csv
path_nr,jahr,Vermögen,Verbindlichkeiten,Deckungsgrad,Zinssatz,...
0,0,1000000,1000000,100.0,0.020,...
0,1,1015230,995420,102.0,0.022,...
...
```

**JSON-Output (Chart-Daten):**
```json
{
  "labels": [0, 1, 2, ..., 40],
  "datasets": [
    {
      "label": "Path 0",
      "data": [100.0, 102.0, 101.5, ...]
    },
    ...
  ]
}
```

### Fehlerbehandlung

**Simulation-Fehler:**
- Numerische Instabilität → Erhöhe Pfad-Anzahl
- Ungültige Parameter → Validierungs-Fehler mit Details
- Out-of-Memory → Reduziere N_PATHS oder T_HORIZON

**Optimierung-Fehler:**
- Konvergenz-Fehler → Ändere initiale Parameter
- Constraints verletzt → Prüfe Bounds und Gewichts-Summe
- Timeout → Erhöhe max_iterations

**Web-Fehler:**
- WebSocket-Disconnect → Automatisches Reconnect
- Datei-Upload-Fehler → Validiere CSV-Format
- 500 Server-Fehler → Check Logs in Console

---

## Lizenz

[Bitte Lizenz hinzufügen]

## Kontakt

[Bitte Kontaktinformationen hinzufügen]

## Changelog

### Version 1.0.0 (2026-01-22)
- Initiale Version
- Monte-Carlo-Simulation mit 6 Asset-Klassen
- Bayesianische Optimierung
- Web-Interface mit Echtzeit-Updates
- R-Analyse mit Parallel-Processing
