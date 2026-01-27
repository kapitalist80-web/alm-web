"""
ALM Optimizer Web Interface
============================
FastAPI-basierte Webanwendung für die ALM-Simulation und -Optimierung.
"""

import asyncio
import json
import os
import shutil
import subprocess
import sys
import tempfile
import multiprocessing as mp
from datetime import datetime
from pathlib import Path
from typing import Optional
from concurrent.futures import ProcessPoolExecutor

from fastapi import FastAPI, WebSocket, WebSocketDisconnect, UploadFile, File, Form, Request
from fastapi.responses import HTMLResponse, FileResponse, JSONResponse, Response
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates

app = FastAPI(title="ALM Optimizer", description="Pensionskassen ALM Simulation & Optimierung")

# Directories
BASE_DIR = Path(__file__).parent
DATA_DIR = BASE_DIR / "data"
TEMPLATES_DIR = BASE_DIR / "templates"
STATIC_DIR = BASE_DIR / "static"

DATA_DIR.mkdir(exist_ok=True)

# Templates
templates = Jinja2Templates(directory=str(TEMPLATES_DIR))

# Mount static files
STATIC_DIR.mkdir(exist_ok=True)
app.mount("/static", StaticFiles(directory=str(STATIC_DIR)), name="static")


@app.get("/favicon.ico")
async def favicon():
    """Serve favicon."""
    favicon_path = STATIC_DIR / "favicon.svg"
    if favicon_path.exists():
        return FileResponse(favicon_path, media_type="image/svg+xml")
    return Response(status_code=204)

# Store active WebSocket connections
active_connections: list[WebSocket] = []

# Current optimization process
current_process: Optional[subprocess.Popen] = None
optimization_running = False

# Default config values
DEFAULT_CONFIG = {
    # Simulation Parameters
    "N_PATHS": 200,
    "T_HORIZON": 40,
    
    # Expected Returns (MU)
    "MU_InterestRate": 0.0025,
    "MU_GovBonds": 0.000,
    "MU_CorpBonds": 0.000,
    "MU_Equities": 0.055,
    "MU_RealEstate": 0.025,
    "MU_Alternatives": 0.035,
    
    # Volatilities (SIGMA)
    "SIGMA_InterestRate": 0.0061,
    "SIGMA_GovBonds": 0.030,
    "SIGMA_CorpBonds": 0.040,
    "SIGMA_Equities": 0.150,
    "SIGMA_RealEstate": 0.040,
    "SIGMA_Alternatives": 0.100,
    
    # Degrees of Freedom (None = Normal distribution)
    "DOF_InterestRate": None,
    "DOF_GovBonds": None,
    "DOF_CorpBonds": None,
    "DOF_Equities": 5,
    "DOF_RealEstate": 6,
    "DOF_Alternatives": 5,
    
    # Asset Allocation Weights
    "WEIGHT_InterestRate": 0.00,
    "WEIGHT_GovBonds": 0.65,
    "WEIGHT_CorpBonds": 0.23,
    "WEIGHT_Equities": 0.05,
    "WEIGHT_RealEstate": 0.07,
    "WEIGHT_Alternatives": 0.00,
    
    # Correlation Matrix (flattened upper triangle)
    "CORR_IntRate_GovBonds": 0.90,
    "CORR_IntRate_CorpBonds": 0.80,
    "CORR_IntRate_Equities": 0.20,
    "CORR_IntRate_RealEstate": 0.40,
    "CORR_IntRate_Alternatives": 0.30,
    "CORR_GovBonds_CorpBonds": 0.85,
    "CORR_GovBonds_Equities": 0.30,
    "CORR_GovBonds_RealEstate": 0.20,
    "CORR_GovBonds_Alternatives": 0.40,
    "CORR_CorpBonds_Equities": 0.45,
    "CORR_CorpBonds_RealEstate": 0.30,
    "CORR_CorpBonds_Alternatives": 0.50,
    "CORR_Equities_RealEstate": 0.50,
    "CORR_Equities_Alternatives": 0.60,
    "CORR_RealEstate_Alternatives": 0.30,
    
    # Interest Rate Structure
    "YIELD_CURVE_SLOPE": 0.0005,
    "BASIS_RATE_FLOOR": -0.01,
    
    # Government Bonds Duration
    "INITIAL_GOV_BOND_DURATION": 22.0,
    "GOV_BOND_DURATION_MODE": "fixed",  # "fixed", "fixed_reset", "liability_matching"
    "GOV_BOND_DURATION_RESET_INTERVAL": 5,

    # Corporate Bonds
    "INITIAL_CORP_BOND_DURATION": 8.0,
    "CORP_BOND_DURATION_MODE": "liability_matching",  # "fixed", "fixed_reset", "liability_matching"
    "CORP_BOND_DURATION_RESET_INTERVAL": 4,
    "CORP_BOND_CREDIT_SPREAD": 0.005,
    "CORP_BOND_DEFAULT_PROBABILITY": 0.003,
    "CORP_BOND_LOSS_GIVEN_DEFAULT": 0.40,
    "CORP_BOND_DEFAULT_EXPOSURE": 0.02,

    # Private IG Core Infrastructure Debt CHF (Alternatives)
    # Art. 53 BVV2 Abs.1 lit. d_bis und d_ter: max. 25% Allokation
    "INITIAL_ALT_BOND_DURATION": 30.0,
    "ALT_BOND_DURATION_MODE": "fixed",  # "fixed", "fixed_reset", "liability_matching", "cashflow_matching"
    "ALT_BOND_DURATION_RESET_INTERVAL": 5,
    "ALT_BOND_CREDIT_SPREAD": 0.015,       # 150 Basispunkte
    "ALT_BOND_DEFAULT_PROBABILITY": 0.013,  # 1.3% p.a.
    "ALT_BOND_LOSS_GIVEN_DEFAULT": 0.35,    # 35% LGD
    "ALT_BOND_DEFAULT_EXPOSURE": 0.02,      # 2%
    
    # Liability Parameters
    "TECHNICAL_RATE_DURATION": 15.0,
    "LIABILITY_DISCOUNT_SPREAD": 0.0050,
    "TECHNICAL_RATE_FLOOR": 0.005,
    
    # Financing Parameters
    "GENERAL_RESERVE_RATE": 0.060,
    "ADMIN_FEE_PER_PERSON": 150.0,
    
    # Mean Reversion for Equities
    "EQUITY_MEAN_REVERSION_ENABLED": True,
    "EQUITY_LONG_TERM_ANNUAL_RETURN": 0.05,
    "EQUITY_MEAN_REVERSION_STRENGTH": 0.10,
    "EQUITY_MEAN_REVERSION_THRESHOLD": -0.30,
    
    # Interest Rate Cap (Zinsabsicherung)
    "INTEREST_RATE_CAP_ENABLED": False,
    "INTEREST_RATE_CAP_STRIKE": 0.0075,  # 75 Basispunkte über aktuellem Zinsniveau
    "INTEREST_RATE_CAP_PREMIUM": 0.002,  # 0.2% des Notionals (20 BP)
    "INTEREST_RATE_CAP_DURATION": 1,     # Laufzeit in Jahren

    # Special Pension Payout (Sonder-Rente)
    "SPECIAL_PENSION_ENABLED": False,
    "SPECIAL_PENSION_THRESHOLD": 1.15,    # Deckungsgrad-Schwelle (115%)
    "SPECIAL_PENSION_TARGET": 1.145,      # Ziel-Deckungsgrad nach Auszahlung (114.5%)

    # Sammelstiftung-Modus (Collective Foundation Mode)
    "SAMMELSTIFTUNG_ENABLED": False,
    "SAMMELSTIFTUNG_INTERVAL": 5,         # Alle X Jahre kommt ein neuer Bestand hinzu
}


def generate_config_file(params: dict) -> str:
    """Generate config_alm.py content from parameters."""
    
    def to_float(val, default):
        """Convert value to float, handling None and strings."""
        if val is None:
            return default
        try:
            return float(val)
        except (ValueError, TypeError):
            return default
    
    def to_int(val, default):
        """Convert value to int."""
        if val is None:
            return default
        try:
            return int(val)
        except (ValueError, TypeError):
            return default
    
    # Build arrays from individual values
    mu = [
        to_float(params.get("MU_InterestRate"), 0.0025),
        to_float(params.get("MU_GovBonds"), 0.000),
        to_float(params.get("MU_CorpBonds"), 0.000),
        to_float(params.get("MU_Equities"), 0.055),
        to_float(params.get("MU_RealEstate"), 0.025),
        to_float(params.get("MU_Alternatives"), 0.035),
    ]
    
    sigma = [
        to_float(params.get("SIGMA_InterestRate"), 0.0061),
        to_float(params.get("SIGMA_GovBonds"), 0.030),
        to_float(params.get("SIGMA_CorpBonds"), 0.040),
        to_float(params.get("SIGMA_Equities"), 0.150),
        to_float(params.get("SIGMA_RealEstate"), 0.040),
        to_float(params.get("SIGMA_Alternatives"), 0.100),
    ]
    
    # DOF: None means normal distribution
    dof_raw = [
        params.get("DOF_InterestRate"),
        params.get("DOF_GovBonds"),
        params.get("DOF_CorpBonds"),
        params.get("DOF_Equities"),
        params.get("DOF_RealEstate"),
        params.get("DOF_Alternatives"),
    ]
    dof = []
    for i, v in enumerate(dof_raw):
        if v is None or v == '' or v == 'None':
            dof.append(None)
        else:
            try:
                dof.append(int(float(v)))
            except:
                dof.append([None, None, None, 5, 6, 5][i])  # defaults
    
    weights = [
        to_float(params.get("WEIGHT_InterestRate"), 0.00),
        to_float(params.get("WEIGHT_GovBonds"), 0.65),
        to_float(params.get("WEIGHT_CorpBonds"), 0.23),
        to_float(params.get("WEIGHT_Equities"), 0.05),
        to_float(params.get("WEIGHT_RealEstate"), 0.07),
        to_float(params.get("WEIGHT_Alternatives"), 0.00),
    ]
    
    # Build correlation matrix - ensure all values are floats
    c01 = to_float(params.get("CORR_IntRate_GovBonds"), 0.90)
    c02 = to_float(params.get("CORR_IntRate_CorpBonds"), 0.80)
    c03 = to_float(params.get("CORR_IntRate_Equities"), 0.20)
    c04 = to_float(params.get("CORR_IntRate_RealEstate"), 0.40)
    c05 = to_float(params.get("CORR_IntRate_Alternatives"), 0.30)
    c12 = to_float(params.get("CORR_GovBonds_CorpBonds"), 0.85)
    c13 = to_float(params.get("CORR_GovBonds_Equities"), 0.30)
    c14 = to_float(params.get("CORR_GovBonds_RealEstate"), 0.20)
    c15 = to_float(params.get("CORR_GovBonds_Alternatives"), 0.40)
    c23 = to_float(params.get("CORR_CorpBonds_Equities"), 0.45)
    c24 = to_float(params.get("CORR_CorpBonds_RealEstate"), 0.30)
    c25 = to_float(params.get("CORR_CorpBonds_Alternatives"), 0.50)
    c34 = to_float(params.get("CORR_Equities_RealEstate"), 0.50)
    c35 = to_float(params.get("CORR_Equities_Alternatives"), 0.60)
    c45 = to_float(params.get("CORR_RealEstate_Alternatives"), 0.30)
    
    corr = [
        [1.00, c01, c02, c03, c04, c05],
        [c01, 1.00, c12, c13, c14, c15],
        [c02, c12, 1.00, c23, c24, c25],
        [c03, c13, c23, 1.00, c34, c35],
        [c04, c14, c24, c34, 1.00, c45],
        [c05, c15, c25, c35, c45, 1.00],
    ]
    
    config_content = f'''# ==============================================================================
# 0. ALLGEMEINE SIMULATIONS-PARAMETER
# ==============================================================================

N_PATHS = {to_int(params.get("N_PATHS"), 500)}            # Anzahl der Monte-Carlo-Pfade
T_HORIZON = {to_int(params.get("T_HORIZON"), 40)}            # Simulationshorizont in Jahren

# --- DATEN-PFADE ---
SURVIVAL_TABLE_PATH = 'data/sterbetabelle_LPP.csv'
POPULATION_PATH = 'data/rentnerbestand_initial.csv'

# ==============================================================================
# 1. KAPITALMARKT-PARAMETER
# ==============================================================================

ASSET_CLASSES = ['InterestRate', 'GovBonds', 'CorpBonds', 'Equities', 'RealEstate', 'Alternatives']
N_ASSETS = len(ASSET_CLASSES)

MU =      {mu}
SIGMA =   {sigma}

DEGREES_OF_FREEDOM = {dof}

WEIGHTS = {weights}

CORR_MATRIX = [
    [1.00, {c01}, {c02}, {c03}, {c04}, {c05}],
    [{c01}, 1.00, {c12}, {c13}, {c14}, {c15}],
    [{c02}, {c12}, 1.00, {c23}, {c24}, {c25}],
    [{c03}, {c13}, {c23}, 1.00, {c34}, {c35}],
    [{c04}, {c14}, {c24}, {c34}, 1.00, {c45}],
    [{c05}, {c15}, {c25}, {c35}, {c45}, 1.00],
]

# ==============================================================================
# 2. ZINSSTRUKTUR & DURATION-PARAMETER
# ==============================================================================

YIELD_CURVE_SLOPE = {to_float(params.get("YIELD_CURVE_SLOPE"), 0.0005)}
BASIS_RATE_FLOOR = {to_float(params.get("BASIS_RATE_FLOOR"), -0.01)}

# --- STAATSANLEIHEN (Government Bonds) ---
INITIAL_GOV_BOND_DURATION = {to_float(params.get("INITIAL_GOV_BOND_DURATION"), 18.0)}
GOV_BOND_DURATION_MODE = "{params.get("GOV_BOND_DURATION_MODE", "fixed")}"
GOV_BOND_DURATION_RESET_INTERVAL = {to_int(params.get("GOV_BOND_DURATION_RESET_INTERVAL"), 5)}

# --- CORPORATE BONDS ---
INITIAL_CORP_BOND_DURATION = {to_float(params.get("INITIAL_CORP_BOND_DURATION"), 8.0)}
CORP_BOND_DURATION_MODE = "{params.get("CORP_BOND_DURATION_MODE", "fixed_reset")}"
CORP_BOND_DURATION_RESET_INTERVAL = {to_int(params.get("CORP_BOND_DURATION_RESET_INTERVAL"), 4)}

CORP_BOND_CREDIT_SPREAD = {to_float(params.get("CORP_BOND_CREDIT_SPREAD"), 0.005)}
CORP_BOND_DEFAULT_PROBABILITY = {to_float(params.get("CORP_BOND_DEFAULT_PROBABILITY"), 0.003)}
CORP_BOND_LOSS_GIVEN_DEFAULT = {to_float(params.get("CORP_BOND_LOSS_GIVEN_DEFAULT"), 0.40)}
CORP_BOND_DEFAULT_EXPOSURE = {to_float(params.get("CORP_BOND_DEFAULT_EXPOSURE"), 0.02)}

# --- PRIVATE IG CORE INFRASTRUCTURE DEBT CHF (Alternatives) ---
# Art. 53 BVV2 Abs.1 lit. d_bis und d_ter: max. 25% Allokation
INITIAL_ALT_BOND_DURATION = {to_float(params.get("INITIAL_ALT_BOND_DURATION"), 30.0)}
ALT_BOND_DURATION_MODE = "{params.get("ALT_BOND_DURATION_MODE", "fixed")}"
ALT_BOND_DURATION_RESET_INTERVAL = {to_int(params.get("ALT_BOND_DURATION_RESET_INTERVAL"), 5)}

ALT_BOND_CREDIT_SPREAD = {to_float(params.get("ALT_BOND_CREDIT_SPREAD"), 0.015)}
ALT_BOND_DEFAULT_PROBABILITY = {to_float(params.get("ALT_BOND_DEFAULT_PROBABILITY"), 0.013)}
ALT_BOND_LOSS_GIVEN_DEFAULT = {to_float(params.get("ALT_BOND_LOSS_GIVEN_DEFAULT"), 0.35)}
ALT_BOND_DEFAULT_EXPOSURE = {to_float(params.get("ALT_BOND_DEFAULT_EXPOSURE"), 0.02)}

INITIAL_BOND_DURATION = INITIAL_GOV_BOND_DURATION
DURATION_RESET_INTERVAL = GOV_BOND_DURATION_RESET_INTERVAL

TECHNICAL_RATE_DURATION = {to_float(params.get("TECHNICAL_RATE_DURATION"), 15.0)}
LIABILITY_DISCOUNT_SPREAD = {to_float(params.get("LIABILITY_DISCOUNT_SPREAD"), 0.0050)}
TECHNICAL_RATE_FLOOR = {to_float(params.get("TECHNICAL_RATE_FLOOR"), 0.005)}

# ==============================================================================
# 3. FINANZIERUNGSPARAMETER
# ==============================================================================

GENERAL_RESERVE_RATE = {to_float(params.get("GENERAL_RESERVE_RATE"), 0.060)}
ADMIN_FEE_PER_PERSON = {to_float(params.get("ADMIN_FEE_PER_PERSON"), 150.0)}

# ==============================================================================
# 5. MEAN REVERSION FÜR AKTIEN
# ==============================================================================

EQUITY_MEAN_REVERSION_ENABLED = {bool(params.get("EQUITY_MEAN_REVERSION_ENABLED", True))}
EQUITY_LONG_TERM_ANNUAL_RETURN = {to_float(params.get("EQUITY_LONG_TERM_ANNUAL_RETURN"), 0.05)}
EQUITY_MEAN_REVERSION_STRENGTH = {to_float(params.get("EQUITY_MEAN_REVERSION_STRENGTH"), 0.10)}
EQUITY_MEAN_REVERSION_THRESHOLD = {to_float(params.get("EQUITY_MEAN_REVERSION_THRESHOLD"), -0.30)}

# ==============================================================================
# 6. INTEREST RATE CAP (Zinsabsicherung)
# ==============================================================================

# Der Interest Rate Cap schützt gegen steigende Zinsen (Zinsschock)
# Notional = Bond-Allokation (Gov + Corp) * Anfangsvermögen
# Strike = Basiszins bei t=0 + INTEREST_RATE_CAP_STRIKE
# Bei r1_t > Strike erhält die PK eine Auszahlung = (r1_t - Strike) * Notional

INTEREST_RATE_CAP_ENABLED = {bool(params.get("INTEREST_RATE_CAP_ENABLED", False))}
INTEREST_RATE_CAP_STRIKE = {to_float(params.get("INTEREST_RATE_CAP_STRIKE"), 0.0075)}  # 75 BP über Basiszins
INTEREST_RATE_CAP_PREMIUM = {to_float(params.get("INTEREST_RATE_CAP_PREMIUM"), 0.002)}  # 0.2% = 20 BP des Notionals
INTEREST_RATE_CAP_DURATION = {to_int(params.get("INTEREST_RATE_CAP_DURATION"), 1)}  # Laufzeit in Jahren

# ==============================================================================
# 7. SPECIAL PENSION PAYOUT (Sonder-Rente)
# ==============================================================================

# Bei Deckungsgrad > SPECIAL_PENSION_THRESHOLD wird eine Sonder-Rente ausgezahlt
# Die Höhe wird so gewählt, dass der Deckungsgrad auf ca. SPECIAL_PENSION_TARGET sinkt
# Dadurch steigt der Cashflow entsprechend

SPECIAL_PENSION_ENABLED = {bool(params.get("SPECIAL_PENSION_ENABLED", False))}
SPECIAL_PENSION_THRESHOLD = {to_float(params.get("SPECIAL_PENSION_THRESHOLD"), 1.15)}  # Schwelle: 115%
SPECIAL_PENSION_TARGET = {to_float(params.get("SPECIAL_PENSION_TARGET"), 1.145)}  # Ziel: 114.5%

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

SAMMELSTIFTUNG_ENABLED = {bool(params.get("SAMMELSTIFTUNG_ENABLED", False))}
SAMMELSTIFTUNG_INTERVAL = {to_int(params.get("SAMMELSTIFTUNG_INTERVAL"), 5)}  # Alle X Jahre
'''
    return config_content


@app.get("/", response_class=HTMLResponse)
async def index(request: Request):
    """Serve main page."""
    return templates.TemplateResponse("index.html", {
        "request": request,
        "config": DEFAULT_CONFIG
    })


@app.get("/api/config")
async def get_config():
    """Get current configuration."""
    return JSONResponse(DEFAULT_CONFIG)


@app.get("/api/status")
async def get_status():
    """Get optimization status."""
    global optimization_running
    return JSONResponse({
        "running": optimization_running,
    })


@app.get("/api/files")
async def list_files():
    """List all files in data directory."""
    files = []
    if DATA_DIR.exists():
        for f in DATA_DIR.iterdir():
            if f.is_file():
                stat = f.stat()
                files.append({
                    "name": f.name,
                    "size": stat.st_size,
                    "modified": datetime.fromtimestamp(stat.st_mtime).isoformat(),
                })
    return JSONResponse(sorted(files, key=lambda x: x["modified"], reverse=True))


@app.get("/api/download/{filename}")
async def download_file(filename: str):
    """Download a file from data directory."""
    file_path = DATA_DIR / filename
    if file_path.exists() and file_path.is_file():
        return FileResponse(
            path=str(file_path),
            filename=filename,
            media_type="application/octet-stream"
        )
    return JSONResponse({"error": "File not found"}, status_code=404)


@app.post("/api/upload-population")
async def upload_population(file: UploadFile = File(...)):
    """Upload a custom population file."""
    try:
        dest = DATA_DIR / "rentnerbestand_initial.csv"
        with open(dest, "wb") as f:
            content = await file.read()
            f.write(content)
        return JSONResponse({"success": True, "filename": file.filename})
    except Exception as e:
        return JSONResponse({"error": str(e)}, status_code=500)


@app.post("/api/stop")
async def stop_optimization():
    """Stop the current optimization."""
    global current_process, optimization_running
    if current_process and optimization_running:
        current_process.terminate()
        optimization_running = False
        await broadcast_message({"type": "status", "message": "Optimierung abgebrochen", "running": False})
        return JSONResponse({"success": True})
    return JSONResponse({"success": False, "message": "No optimization running"})


@app.post("/api/generate-population")
async def generate_population(
    n_population: int = Form(50),
    age_mean: float = Form(69),
    age_std: float = Form(8),
    min_age: int = Form(65),
    max_age: int = Form(100),
    pct_female: float = Form(0.55),
    pct_married: float = Form(0.50),
    pct_single: float = Form(0.40),
    pct_widowed: float = Form(0.10),
    pension_mean: float = Form(30000),
    pension_std: float = Form(10000),
    pension_min: float = Form(10000),
    pension_max: float = Form(80000),
):
    """Generate a new population file with given parameters."""
    import numpy as np
    import pandas as pd
    from datetime import datetime
    import shutil
    
    try:
        # Archive existing file if it exists
        existing_file = DATA_DIR / "rentnerbestand_initial.csv"
        if existing_file.exists():
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            archive_name = f"rentnerbestand_initial_archived_{timestamp}.csv"
            archive_path = DATA_DIR / archive_name
            shutil.move(str(existing_file), str(archive_path))
        
        # Generate new population
        np.random.seed(None)  # Use random seed for variety
        
        CURRENT_YEAR = datetime.now().year
        
        # Ages
        ages = np.round(np.clip(
            np.random.normal(age_mean, age_std, n_population), 
            min_age, max_age
        )).astype(int)
        
        # Gender
        genders = np.random.choice(['F', 'M'], size=n_population, p=[pct_female, 1 - pct_female])
        
        # Marital status - normalize probabilities
        total_pct = pct_married + pct_single + pct_widowed
        p_married = pct_married / total_pct
        p_single = pct_single / total_pct
        p_widowed = pct_widowed / total_pct
        marital_statuses = np.random.choice(
            ['Married', 'Single', 'Widowed'], 
            size=n_population, 
            p=[p_married, p_single, p_widowed]
        )
        
        # Pensions
        pensions = np.round(np.clip(
            np.random.normal(pension_mean, pension_std, n_population), 
            pension_min, pension_max
        ), -2)
        
        # Birth years
        birth_years = CURRENT_YEAR - ages
        
        # Create DataFrame
        population_df = pd.DataFrame({
            'ID': np.arange(1, n_population + 1),
            'Age': ages,
            'Gender': genders,
            'MaritalStatus': marital_statuses,
            'InitialPension': pensions,
            'BirthYear': birth_years,
        })
        
        # Save
        population_df.to_csv(existing_file, index=False)
        
        # Calculate summary stats
        summary = {
            "success": True,
            "message": f"Rentnerbestand mit {n_population} Personen generiert",
            "stats": {
                "n_total": int(n_population),
                "avg_age": float(ages.mean()),
                "pct_female": float((genders == 'F').sum() / n_population * 100),
                "pct_married": float((marital_statuses == 'Married').sum() / n_population * 100),
                "avg_pension": float(pensions.mean()),
                "total_pension": float(pensions.sum()),
            }
        }
        
        await broadcast_message({
            "type": "log", 
            "message": f"✅ Neuer Rentnerbestand: {n_population} Personen, Ø Alter {ages.mean():.1f}, Ø Rente {pensions.mean():,.0f} CHF"
        })
        
        return JSONResponse(summary)
        
    except Exception as e:
        return JSONResponse({"success": False, "message": str(e)}, status_code=500)


@app.get("/api/chart-data/{filename}")
async def get_chart_data(filename: str):
    """Get chart data from JSON file."""
    file_path = DATA_DIR / filename
    if file_path.exists() and file_path.suffix == '.json':
        with open(file_path, 'r') as f:
            return JSONResponse(json.load(f))
    return JSONResponse({"error": "File not found"}, status_code=404)


@app.get("/api/analysis-files")
async def list_analysis_files():
    """List all mc_all_paths_*.csv files for analysis."""
    files = []
    if DATA_DIR.exists():
        for f in DATA_DIR.glob("mc_all_paths_*.csv"):
            if f.is_file():
                stat = f.stat()
                files.append({
                    "name": f.name,
                    "size": stat.st_size,
                    "modified": datetime.fromtimestamp(stat.st_mtime).isoformat(),
                })
    return JSONResponse(sorted(files, key=lambda x: x["modified"], reverse=True))


@app.get("/api/analysis/{filename}")
async def get_analysis_data(filename: str, year: int = 40):
    """Get analysis data for box plot from CSV file."""
    import pandas as pd
    import numpy as np
    
    file_path = DATA_DIR / filename
    if not file_path.exists() or not filename.startswith("mc_all_paths_"):
        return JSONResponse({"error": "File not found"}, status_code=404)
    
    try:
        # Read CSV with semicolon separator
        df = pd.read_csv(file_path, sep=';')
        
        # Get max year available
        max_year = int(df['year'].max())
        year = min(year, max_year)
        
        # Filter for selected year
        df_year = df[df['year'] == year].copy()
        
        # Calculate V_t - W_t (surplus/deficit)
        df_year['surplus'] = df_year['V_t'] - df_year['W_t']
        
        # Get path data for box plot
        paths_data = []
        for _, row in df_year.iterrows():
            paths_data.append({
                "path_nr": int(row['path_nr']),
                "surplus": float(row['surplus']),
                "V_t": float(row['V_t']),
                "W_t": float(row['W_t']),
                "deckungsgrad": float(row['deckungsgrad'])
            })
        
        # Calculate box plot statistics
        surplus_values = df_year['surplus'].values
        q1 = float(np.percentile(surplus_values, 25))
        q3 = float(np.percentile(surplus_values, 75))
        median = float(np.median(surplus_values))
        iqr = q3 - q1
        whisker_low = float(max(surplus_values.min(), q1 - 1.5 * iqr))
        whisker_high = float(min(surplus_values.max(), q3 + 1.5 * iqr))
        
        # Get simulation parameters from first row
        first_row = df.iloc[0]
        params = {
            "n_paths": int(first_row.get('n_paths', 0)),
            "t_horizon": int(first_row.get('t_horizon', 0)),
            "weight_gov_bonds": float(first_row.get('weight_gov_bonds', 0)),
            "weight_corp_bonds": float(first_row.get('weight_corp_bonds', 0)),
            "weight_equities": float(first_row.get('weight_equities', 0)),
            "weight_real_estate": float(first_row.get('weight_real_estate', 0)),
            "weight_alternatives": float(first_row.get('weight_alternatives', 0)),
            "mu_equities": float(first_row.get('mu_equities', 0)),
            "mu_real_estate": float(first_row.get('mu_real_estate', 0)),
            "mu_alternatives": float(first_row.get('mu_alternatives', 0)),
            "sigma_equities": float(first_row.get('sigma_equities', 0)),
            "sigma_real_estate": float(first_row.get('sigma_real_estate', 0)),
            "sigma_alternatives": float(first_row.get('sigma_alternatives', 0)),
            "gov_bond_duration_mode": str(first_row.get('gov_bond_duration_mode', '')),
            "corp_bond_duration_mode": str(first_row.get('corp_bond_duration_mode', '')),
            "initial_gov_bond_duration": float(first_row.get('initial_gov_bond_duration', 0)),
            "initial_corp_bond_duration": float(first_row.get('initial_corp_bond_duration', 0)),
            "gov_bond_duration_reset_enabled": bool(first_row.get('gov_bond_duration_reset_enabled', False)),
            "gov_bond_duration_reset_interval": int(first_row.get('gov_bond_duration_reset_interval', 0)),
            "corp_bond_duration_reset_enabled": bool(first_row.get('corp_bond_duration_reset_enabled', False)),
            "corp_bond_duration_reset_interval": int(first_row.get('corp_bond_duration_reset_interval', 0)),
            "technical_rate_duration": float(first_row.get('technical_rate_duration', 0)),
            "liability_discount_spread": float(first_row.get('liability_discount_spread', 0)),
            "technical_rate_floor": float(first_row.get('technical_rate_floor', 0)),
            "general_reserve_rate": float(first_row.get('general_reserve_rate', 0)),
            "bestand_n_total": int(first_row.get('bestand_n_total', 0)),
            "bestand_avg_age": float(first_row.get('bestand_avg_age', 0)),
            "bestand_W0": float(first_row.get('bestand_W0', 0)),
        }
        
        # Calculate risk metrics for selected year
        n_paths = len(df_year)
        n_default = int((df_year['W_t'] > df_year['V_t']).sum())  # W_t > V_t means default
        n_underfunding = int((df_year['deckungsgrad'] < 1.0).sum())  # DG < 100%
        prob_default = n_default / n_paths if n_paths > 0 else 0
        prob_underfunding = n_underfunding / n_paths if n_paths > 0 else 0
        
        # Average deficit for defaulted paths
        defaulted_paths = df_year[df_year['W_t'] > df_year['V_t']]
        avg_deficit = float(defaulted_paths['surplus'].mean()) if len(defaulted_paths) > 0 else 0
        
        # Deckungsgrad statistics
        dg_values = df_year['deckungsgrad'].values
        
        risk_metrics = {
            "n_paths": n_paths,
            "n_default": n_default,
            "n_underfunding": n_underfunding,
            "prob_default": prob_default,
            "prob_underfunding": prob_underfunding,
            "avg_deficit_at_default": avg_deficit,
            "dg_mean": float(np.mean(dg_values)),
            "dg_median": float(np.median(dg_values)),
            "dg_p5": float(np.percentile(dg_values, 5)),
            "dg_p95": float(np.percentile(dg_values, 95)),
        }
        
        return JSONResponse({
            "year": year,
            "max_year": max_year,
            "paths": paths_data,
            "boxplot": {
                "q1": q1,
                "median": median,
                "q3": q3,
                "whisker_low": whisker_low,
                "whisker_high": whisker_high,
                "min": float(surplus_values.min()),
                "max": float(surplus_values.max()),
                "mean": float(surplus_values.mean()),
            },
            "params": params,
            "risk_metrics": risk_metrics
        })
        
    except Exception as e:
        return JSONResponse({"error": str(e)}, status_code=500)


@app.get("/api/analysis/{filename}/path/{path_nr}")
async def get_path_analysis(filename: str, path_nr: int):
    """Get detailed path analysis for a specific path."""
    import pandas as pd
    import numpy as np
    
    file_path = DATA_DIR / filename
    if not file_path.exists() or not filename.startswith("mc_all_paths_"):
        return JSONResponse({"error": "File not found"}, status_code=404)
    
    try:
        # Read CSV
        df = pd.read_csv(file_path, sep=';')
        
        # Get specific path
        df_path = df[df['path_nr'] == path_nr].sort_values('year')
        
        if len(df_path) == 0:
            return JSONResponse({"error": f"Path {path_nr} not found"}, status_code=404)
        
        # Get all paths for statistics calculation
        years = sorted(df['year'].unique())
        n_paths = df['path_nr'].nunique()
        
        # Calculate statistics for all metrics across all paths
        def calc_stats(col):
            stats = {"p5": [], "median": [], "p95": [], "mean": []}
            for y in years:
                df_y = df[df['year'] == y]
                vals = df_y[col].values
                stats["p5"].append(float(np.percentile(vals, 5)))
                stats["median"].append(float(np.median(vals)))
                stats["p95"].append(float(np.percentile(vals, 95)))
                stats["mean"].append(float(np.mean(vals)))
            return stats
        
        # Path data
        path_data = {
            "years": [int(y) for y in df_path['year'].values],
            "portfolio_return": [float(v) for v in df_path['portfolio_return'].values],
            "r1_t": [float(v) for v in df_path['r1_t'].values],
            "num_widows": [int(v) for v in df_path['num_widows'].values],
            "V_t": [float(v) for v in df_path['V_t'].values],
            "W_t": [float(v) for v in df_path['W_t'].values],
            "deckungsgrad": [float(v) for v in df_path['deckungsgrad'].values],
            "num_pensioners": [int(v) for v in df_path['num_pensioners'].values],
        }
        
        # Calculate statistics for comparison
        stats = {
            "portfolio_return": calc_stats('portfolio_return'),
            "r1_t": calc_stats('r1_t'),
            "num_widows": calc_stats('num_widows'),
            "V_t": calc_stats('V_t'),
            "W_t": calc_stats('W_t'),
            "deckungsgrad": calc_stats('deckungsgrad'),
            "num_pensioners": calc_stats('num_pensioners'),
        }
        
        # Get simulation parameters
        first_row = df.iloc[0]
        params = {
            "n_paths": int(first_row.get('n_paths', 0)),
            "t_horizon": int(first_row.get('t_horizon', 0)),
            "weight_gov_bonds": float(first_row.get('weight_gov_bonds', 0)),
            "weight_corp_bonds": float(first_row.get('weight_corp_bonds', 0)),
            "weight_equities": float(first_row.get('weight_equities', 0)),
            "weight_real_estate": float(first_row.get('weight_real_estate', 0)),
            "weight_alternatives": float(first_row.get('weight_alternatives', 0)),
            "mu_interest_rate": float(first_row.get('mu_interest_rate', 0)),
            "mu_equities": float(first_row.get('mu_equities', 0)),
            "mu_real_estate": float(first_row.get('mu_real_estate', 0)),
            "mu_alternatives": float(first_row.get('mu_alternatives', 0)),
            "sigma_interest_rate": float(first_row.get('sigma_interest_rate', 0)),
            "sigma_equities": float(first_row.get('sigma_equities', 0)),
            "sigma_real_estate": float(first_row.get('sigma_real_estate', 0)),
            "sigma_alternatives": float(first_row.get('sigma_alternatives', 0)),
            "gov_bond_duration_mode": str(first_row.get('gov_bond_duration_mode', '')),
            "corp_bond_duration_mode": str(first_row.get('corp_bond_duration_mode', '')),
            "initial_gov_bond_duration": float(first_row.get('initial_gov_bond_duration', 0)),
            "initial_corp_bond_duration": float(first_row.get('initial_corp_bond_duration', 0)),
            "technical_rate_floor": float(first_row.get('technical_rate_floor', 0)),
            "technical_rate_duration": float(first_row.get('technical_rate_duration', 0)),
            "liability_discount_spread": float(first_row.get('liability_discount_spread', 0)),
            "general_reserve_rate": float(first_row.get('general_reserve_rate', 0)),
            "admin_fee_per_person": float(first_row.get('admin_fee_per_person', 0)),
            "bestand_n_total": int(first_row.get('bestand_n_total', 0)),
            "bestand_avg_age": float(first_row.get('bestand_avg_age', 0)),
            "bestand_n_m": int(first_row.get('bestand_n_m', 0)),
            "bestand_n_f": int(first_row.get('bestand_n_f', 0)),
            "bestand_share_married": float(first_row.get('bestand_share_married', 0)),
            "bestand_W0": float(first_row.get('bestand_W0', 0)),
            "bestand_pension_mean": float(first_row.get('bestand_pension_mean', 0)),
            "bestand_pension_median": float(first_row.get('bestand_pension_median', 0)),
            "equity_mean_reversion_enabled": bool(first_row.get('equity_mean_reversion_enabled', False)),
            "equity_long_term_annual_return": float(first_row.get('equity_long_term_annual_return', 0)),
        }
        
        # Calculate summary metrics for this path
        path_total_cashflow = float(df_path['cashflow_total'].sum())
        path_avg_return = float(df_path['portfolio_return'].mean())
        
        # Calculate capital income (portfolio_return * V_t for each year, then sum)
        # Use V_t from previous year (shifted) for the return calculation
        v_t_values = df_path['V_t'].values
        returns = df_path['portfolio_return'].values
        # Capital income = return_t * V_{t-1}, approximate with V_t * return_t
        path_total_capital_income = float(np.sum(returns * v_t_values))
        
        # Calculate the same metrics for all paths to get statistics
        all_paths = df['path_nr'].unique()
        all_total_cashflows = []
        all_avg_returns = []
        all_total_capital_incomes = []
        
        for p in all_paths:
            df_p = df[df['path_nr'] == p].sort_values('year')
            all_total_cashflows.append(df_p['cashflow_total'].sum())
            all_avg_returns.append(df_p['portfolio_return'].mean())
            v_vals = df_p['V_t'].values
            r_vals = df_p['portfolio_return'].values
            all_total_capital_incomes.append(np.sum(r_vals * v_vals))
        
        summary_metrics = {
            "path": {
                "total_cashflow": path_total_cashflow,
                "avg_return": path_avg_return,
                "total_capital_income": path_total_capital_income,
            },
            "all_paths": {
                "total_cashflow": {
                    "p5": float(np.percentile(all_total_cashflows, 5)),
                    "median": float(np.median(all_total_cashflows)),
                    "p95": float(np.percentile(all_total_cashflows, 95)),
                    "mean": float(np.mean(all_total_cashflows)),
                },
                "avg_return": {
                    "p5": float(np.percentile(all_avg_returns, 5)),
                    "median": float(np.median(all_avg_returns)),
                    "p95": float(np.percentile(all_avg_returns, 95)),
                    "mean": float(np.mean(all_avg_returns)),
                },
                "total_capital_income": {
                    "p5": float(np.percentile(all_total_capital_incomes, 5)),
                    "median": float(np.median(all_total_capital_incomes)),
                    "p95": float(np.percentile(all_total_capital_incomes, 95)),
                    "mean": float(np.mean(all_total_capital_incomes)),
                },
            }
        }
        
        return JSONResponse({
            "path_nr": path_nr,
            "years": [int(y) for y in years],
            "path_data": path_data,
            "statistics": stats,
            "params": params,
            "summary_metrics": summary_metrics
        })
        
    except Exception as e:
        return JSONResponse({"error": str(e)}, status_code=500)


@app.get("/api/latest-chart")
async def get_latest_chart():
    """Get the most recent chart data file."""
    json_files = list(DATA_DIR.glob("mc_paths_chart_*.json"))
    if not json_files:
        return JSONResponse({"error": "No chart data available"}, status_code=404)
    
    latest = max(json_files, key=lambda f: f.stat().st_mtime)
    with open(latest, 'r') as f:
        data = json.load(f)
        data['filename'] = latest.name
        return JSONResponse(data)


@app.websocket("/ws")
async def websocket_endpoint(websocket: WebSocket):
    """WebSocket endpoint for real-time updates."""
    await websocket.accept()
    active_connections.append(websocket)
    try:
        while True:
            data = await websocket.receive_text()
            msg = json.loads(data)
            
            if msg.get("action") == "start_optimization":
                await run_optimization(msg.get("config", {}), msg.get("method", "bayesian"), 
                                      msg.get("iterations", 50))
            elif msg.get("action") == "start_simulation":
                await run_simulation(msg.get("config", {}))
    except WebSocketDisconnect:
        active_connections.remove(websocket)


async def broadcast_message(message: dict):
    """Broadcast message to all connected clients."""
    disconnected = []
    for connection in active_connections:
        try:
            await connection.send_json(message)
        except:
            disconnected.append(connection)
    for conn in disconnected:
        if conn in active_connections:
            active_connections.remove(conn)


async def run_optimization(config: dict, method: str, iterations: int):
    """Run the optimization process."""
    global current_process, optimization_running
    
    if optimization_running:
        await broadcast_message({"type": "error", "message": "Optimierung läuft bereits"})
        return
    
    optimization_running = True
    await broadcast_message({"type": "status", "message": "Starte Optimierung...", "running": True})
    
    try:
        # Generate config file
        config_content = generate_config_file(config)
        config_path = DATA_DIR.parent / "config_alm.py"
        with open(config_path, "w") as f:
            f.write(config_content)
        
        await broadcast_message({"type": "log", "message": f"Config generiert: {method} mit {iterations} Iterationen"})
        
        # Prepare command
        optimize_script = DATA_DIR.parent / "optimize_alm_beta.py"
        cmd = [sys.executable, str(optimize_script), "--method", method, "--iterations", str(iterations)]
        
        # Run optimization
        current_process = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            bufsize=1,
            cwd=str(DATA_DIR.parent)
        )
        
        # Stream output
        while True:
            line = current_process.stdout.readline()
            if not line and current_process.poll() is not None:
                break
            if line:
                await broadcast_message({"type": "log", "message": line.strip()})
                await asyncio.sleep(0.01)  # Allow other tasks
        
        return_code = current_process.poll()
        
        if return_code == 0:
            await broadcast_message({"type": "status", "message": "Optimierung erfolgreich abgeschlossen!", "running": False, "success": True})
        else:
            await broadcast_message({"type": "status", "message": f"Optimierung beendet (Code: {return_code})", "running": False, "success": False})
            
    except Exception as e:
        await broadcast_message({"type": "error", "message": str(e)})
    finally:
        optimization_running = False
        current_process = None
        await broadcast_message({"type": "status", "running": False})


async def run_simulation(config: dict):
    """Run Monte Carlo simulation and return chart data."""
    global optimization_running
    
    if optimization_running:
        await broadcast_message({"type": "error", "message": "Eine Berechnung läuft bereits"})
        return
    
    optimization_running = True
    await broadcast_message({"type": "status", "message": "Starte Monte-Carlo-Simulation...", "running": True, "mode": "simulation"})
    
    try:
        # Generate config file
        config_content = generate_config_file(config)
        config_path = DATA_DIR.parent / "config_alm.py"
        with open(config_path, "w") as f:
            f.write(config_content)
        
        n_paths = config.get("N_PATHS", 500)
        t_horizon = config.get("T_HORIZON", 40)
        
        await broadcast_message({"type": "log", "message": f"Config generiert: {n_paths} Pfade, {t_horizon} Jahre Horizont"})
        
        # Run simulation script
        simulation_script = DATA_DIR.parent / "main_alm_simulation.py"
        cmd = [sys.executable, str(simulation_script)]
        
        current_process = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            bufsize=1,
            cwd=str(DATA_DIR.parent)
        )
        
        # Stream output
        while True:
            line = current_process.stdout.readline()
            if not line and current_process.poll() is not None:
                break
            if line:
                await broadcast_message({"type": "log", "message": line.strip()})
                await asyncio.sleep(0.01)
        
        return_code = current_process.poll()
        
        if return_code == 0:
            # Find the latest chart JSON file
            json_files = list(DATA_DIR.glob("mc_paths_chart_*.json"))
            if json_files:
                latest = max(json_files, key=lambda f: f.stat().st_mtime)
                with open(latest, 'r') as f:
                    chart_data = json.load(f)
                
                await broadcast_message({
                    "type": "simulation_complete", 
                    "message": "Simulation erfolgreich abgeschlossen!",
                    "chart_data": chart_data,
                    "filename": latest.name
                })
            else:
                await broadcast_message({"type": "status", "message": "Simulation abgeschlossen, aber keine Chart-Daten gefunden", "running": False})
        else:
            await broadcast_message({"type": "status", "message": f"Simulation beendet mit Fehler (Code: {return_code})", "running": False, "success": False})
            
    except Exception as e:
        await broadcast_message({"type": "error", "message": str(e)})
    finally:
        optimization_running = False
        await broadcast_message({"type": "status", "running": False})


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
