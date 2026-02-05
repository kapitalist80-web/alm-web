# CLAUDE.md - ALM Web Application Guide

## Project Overview

**ALM Optimizer** is a FastAPI-based web application for Asset-Liability Management (ALM) simulation and optimization for Swiss pension funds (Pensionskassen). It uses Monte Carlo simulations to model future financial scenarios and optimize asset allocation to minimize underfunding risks.

**Primary Language:** Python (with R for post-simulation analysis)
**Domain Context:** Swiss pension fund management, financial modeling, actuarial science
**Language Note:** Most documentation and UI text is in German (Swiss context)

## Quick Reference

### Running the Application
```bash
cd /home/user/alm-web
python app.py                    # Start web server on localhost:8000
python main_alm_simulation.py    # Run simulation directly
python optimize_alm_beta.py      # Run optimization (default: Bayesian)
python optimize_alm_beta.py --method grid --iterations 100
```

### Key URLs (when running)
- Web UI: http://localhost:8000
- Health check: http://localhost:8000/health
- API config: http://localhost:8000/api/config

## Project Structure

```
/home/user/alm-web/
├── app.py                      # FastAPI web server (main entry point)
├── main_alm_simulation.py      # Monte Carlo simulation engine
├── optimize_alm_beta.py        # Bayesian/Grid/Random optimization
├── config_alm.py               # Generated configuration (auto-created from UI)
├── rentnerbestand_generator.py # Sample population data generator
├── functions.R                 # R analysis suite for post-processing
├── functions_parallel.R        # Parallelized R analysis functions
├── templates/
│   └── index.html              # Single-page web UI (Vue.js-like reactive)
├── static/
│   └── favicon.svg             # Application favicon
└── data/                       # Output directory (created at runtime)
    ├── sterbetabelle_LPP.csv   # Swiss mortality tables (LPP = BVG)
    ├── rentnerbestand_initial.csv  # Population/pensioner data
    ├── mc_all_paths_*.csv      # Simulation results
    └── mc_paths_chart_*.json   # Chart data for visualization
```

## Core Components

### 1. app.py - Web Server
FastAPI application providing:
- REST API endpoints for config, files, analysis
- WebSocket endpoint (`/ws`) for real-time simulation updates
- Population file upload/generation
- Config file generation from UI parameters

**Key Functions:**
- `generate_config_file(params)`: Creates `config_alm.py` from web form
- `run_simulation(config)`: Executes Monte Carlo simulation
- `run_optimization(config, method, iterations)`: Runs parameter optimization
- `broadcast_message(message)`: Sends WebSocket updates to all clients

### 2. main_alm_simulation.py - Simulation Engine
Monte Carlo simulation with:
- 6 asset classes: Interest Rate, Gov Bonds, Corp Bonds, Equities, Real Estate, Alternatives
- Correlated returns via Cholesky decomposition
- Student-t distributions for fat-tail risk modeling
- Mortality modeling with spouse benefits
- Duration-based bond return calculations
- Cash-flow matching (CFM) strategies

**Key Functions:**
- `load_data()`: Loads mortality tables and population data
- `simulate_asset_returns()`: Generates correlated multi-asset returns
- `run_monte_carlo_path_full()`: Executes a single simulation path
- `calculate_risk_metrics()`: Computes underfunding/default probabilities

### 3. optimize_alm_beta.py - Optimization
Optimization methods:
- **Bayesian** (default): SLSQP via `scipy.optimize.minimize`
- **Grid Search**: Systematic parameter grid evaluation
- **Random Search**: Stochastic parameter sampling

**Objective Function:**
```
Objective = P(Underfunding) + 5.0 × P(Default) + 0.00001 × E[Shortfall]
```

**Optimized Parameters:**
- Asset weights (Gov Bonds, Corp Bonds, Equities, Real Estate)
- Bond durations (Government, Corporate)

### 4. config_alm.py - Configuration
Dynamically generated configuration file containing:
- Simulation parameters (N_PATHS, T_HORIZON)
- Capital market assumptions (MU, SIGMA, DOF)
- Asset allocation weights
- Correlation matrix (6x6)
- Duration strategies and parameters
- Risk management features

## Key Domain Concepts

### Asset Classes (6)
| Index | Class | German | Description |
|-------|-------|--------|-------------|
| 0 | InterestRate | Zinsen | Base interest rate (cash) |
| 1 | GovBonds | Staatsanleihen | Government bonds |
| 2 | CorpBonds | Unternehmensanleihen | Corporate bonds |
| 3 | Equities | Aktien | Stocks |
| 4 | RealEstate | Immobilien | Real estate |
| 5 | Alternatives | Alternative | Alternative investments |

### Duration Modes
- `fixed`: Constant duration throughout simulation
- `fixed_reset`: Reset to initial duration at intervals
- `liability_matching`: Match liability duration
- `cashflow_matching`: CFM-based duration calculation

### Key Metrics
- **Deckungsgrad** (Funding Ratio): V_t / W_t (Assets / Liabilities)
- **V_t**: Vermögen (Assets/Portfolio value)
- **W_t**: Verpflichtungen (Liabilities/Present value of obligations)
- **r1_t**: Base interest rate at time t
- **i_tech_t**: Technical interest rate

### Special Features
- **Sammelstiftung Mode**: Collective foundation mode with periodic population additions
- **Special Pension Payout**: One-time payments when funding ratio exceeds threshold
- **Interest Rate Cap**: Hedging against rising interest rates
- **Mean Reversion**: Equity return mean reversion after large drawdowns

## Development Conventions

### Code Style
- Python 3.8+ with type hints where beneficial
- German variable names in domain-specific contexts (Deckungsgrad, Vermögen)
- English for general programming constructs
- Extensive docstrings in German

### Configuration Pattern
Configuration flows: Web UI → `app.py` → `config_alm.py` → Simulation scripts

When modifying configuration:
1. Add new parameter to `DEFAULT_CONFIG` in `app.py`
2. Add to `generate_config_file()` template string
3. Add to UI form in `templates/index.html`
4. Use in `main_alm_simulation.py` via `cfg.PARAMETER_NAME`

### File Naming Conventions
- `mc_all_paths_YYYYMMDD_HHMMSS.csv`: Full simulation results
- `mc_paths_chart_YYYYMMDD_HHMMSS.json`: Chart data for visualization
- `rentnerbestand_*.csv`: Population/pensioner data files

### Data Formats
- CSV files use semicolon (`;`) as delimiter
- Decimal point is `.` (not comma)
- Dates in ISO format: YYYYMMDD_HHMMSS

## API Endpoints Summary

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/` | GET | Main web interface |
| `/health` | GET | Health check |
| `/api/config` | GET | Current configuration |
| `/api/status` | GET | Optimization running status |
| `/api/files` | GET | List data files |
| `/api/download/{filename}` | GET | Download data file |
| `/api/upload-population` | POST | Upload population CSV |
| `/api/generate-population` | POST | Generate synthetic population |
| `/api/stop` | POST | Stop running optimization |
| `/api/analysis-files` | GET | List analysis CSV files |
| `/api/analysis/{filename}` | GET | Get analysis data with box plot stats |
| `/api/analysis/{filename}/path/{path_nr}` | GET | Detailed path analysis |
| `/api/latest-chart` | GET | Most recent chart data |
| `/ws` | WebSocket | Real-time simulation updates |

## Common Tasks

### Adding a New Configuration Parameter
1. Add to `DEFAULT_CONFIG` in `app.py`
2. Add to `generate_config_file()` output template
3. Add UI input in `templates/index.html`
4. Import from `config_alm` in simulation scripts

### Modifying the Simulation
- Core simulation logic: `main_alm_simulation.py`
- Return calculations: `simulate_asset_returns()` and `run_monte_carlo_path_full()`
- Risk metrics: `calculate_risk_metrics()`

### Modifying the Optimization
- Objective function: `objective_function()` in `optimize_alm_beta.py`
- Parameter bounds: `PARAM_BOUNDS` dictionary
- Optimization weights: `ALPHA_UNDERFUNDING`, `BETA_DEFAULT`, `GAMMA_SHORTFALL`

### Running R Analysis
```r
source("functions.R")
df <- load_mc_data("data/")
# or for parallel processing:
source("functions_parallel.R")
init_parallel()
df <- load_mc_data("data/", parallel = TRUE)
```

## Dependencies

### Python
- `fastapi`, `uvicorn`: Web framework and server
- `pandas`, `numpy`: Data manipulation
- `scipy`: Optimization algorithms
- `matplotlib`: Plotting (for CLI mode)
- `python-multipart`: File uploads

### R (for analysis)
- `data.table`, `dplyr`, `tidyr`: Data manipulation
- `ggplot2`: Visualization
- `doParallel`, `foreach`: Parallel processing

## Testing Notes

- No automated test suite exists
- Manual testing via web interface
- Debug mode available: Set `DEBUG_MODE_SINGLE_PATH = True` in `main_alm_simulation.py`
- Use small N_PATHS (50-100) for quick iteration

## Important Constraints

### Swiss Pension Regulations (BVV2)
- Art. 53 BVV2 limits on alternative investments (max 25%)
- Technical interest rate floors and ceilings
- Reserve requirements (General Reserve Rate)

### Numerical Stability
- Cholesky decomposition requires positive-definite correlation matrix
- Weight constraints: must sum to 1.0
- Duration values must be positive
- Interest rates can go negative (floor applies)

## Git Workflow

- Main development on feature branches
- Commit messages in English
- No pre-commit hooks configured
- `.gitignore` excludes `__pycache__/` and `.pyc` files

## Troubleshooting

### Common Issues
1. **Config import error**: Ensure `config_alm.py` exists (generated by web UI or manually)
2. **Data not found**: Create `data/` directory and required CSV files
3. **WebSocket disconnect**: Check browser console, server may have restarted
4. **Slow simulation**: Reduce N_PATHS or use fewer cores

### Debug Tips
- Enable `DEBUG_MODE_SINGLE_PATH = True` for single-path tracing
- Check console output for simulation progress
- WebSocket messages provide real-time status
- CSV output contains all path data for analysis
