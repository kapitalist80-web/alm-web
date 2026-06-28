###############################################################################
# strategy_analysis.R
#
# Speichereffiziente Auswertung von ALM-Monte-Carlo-Ergebnissen ueber viele
# Anlagestrategien. Jedes Strategie-CSV (~500 MC-Pfade x t_horizon Jahre,
# 90+ Spalten) wird EINZELN geladen, zu wenigen Kennzahlen verdichtet und
# anschliessend sofort wieder aus dem RAM entfernt. Es liegt daher nie mehr als
# ein File gleichzeitig im Speicher -> auch >1200 Files auf 32 GB unproblematisch.
#
# Datenformat (pro Strategie-CSV, eine Zeile je Pfad-Jahr):
#   path_nr;year;deckungsgrad;V_t;W_t;portfolio_return; ... ;weight_*; ...
#   - V_t = Verpflichtungen (Barwert, Passivseite, LDI-Referenz)
#   - W_t = Vermoegen (Aktivseite)
#   - deckungsgrad = W_t / V_t (Deckungsgrad)
#   Trennzeichen ";", Dezimalpunkt "." (Swiss-Export)
#
# Input : alle *.csv in  data/sensitivity/
# Output: data/sensitivity/output/  (Tabellen + Plots)
###############################################################################

suppressPackageStartupMessages({
  library(data.table)   # schnelles, speichersparsames fread / Aggregation
})

## ---------------------------------------------------------------------------
## 0) Konfiguration
## ---------------------------------------------------------------------------
INPUT_DIR  <- "data/sensitivity"
OUTPUT_DIR <- file.path(INPUT_DIR, "output")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Risikofreier Jahres-Return fuer Sharpe / Information-Ratio.
# Konservativ aus den Daten ableitbar; hier als fixer Parameter gesetzt.
RISK_FREE_ANNUAL <- 0.00

# Funding-Schwelle (Unterdeckung) und kritische Schwelle
FUNDING_THRESHOLD <- 1.00   # Deckungsgrad < 1.00 => Unterdeckung
CRITICAL_THRESHOLD <- 0.90  # Deckungsgrad < 0.90 => kritische Unterdeckung

# data.table nur so viele Threads wie noetig (Speicher/CPU-Balance)
setDTthreads(0L)  # 0 = alle verfuegbaren Kerne

## ---------------------------------------------------------------------------
## Population (FIX in diesem Lauf) – nur zur Dokumentation, KEINE
## Auswertungsdimension. Wird unveraendert in alle Ausgaben uebernommen.
## ---------------------------------------------------------------------------
POPULATION_COLS <- c(
  "bestand_n_total","bestand_avg_age","bestand_n_m","bestand_n_f",
  "bestand_share_m","bestand_share_f","bestand_n_married",
  "bestand_share_married","bestand_share_single",
  "bestand_total_initial_pension","bestand_pension_median",
  "bestand_pension_mean","bestand_W0","bestand_spouse_age_diff_mean",
  "bestand_spouse_pension_rate"
)

# Spalten, die die Strategie identifizieren (konstant innerhalb eines Files)
STRATEGY_COLS <- c(
  "weight_gov_bonds","weight_corp_bonds","weight_equities",
  "weight_real_estate","weight_alternatives",
  "initial_gov_bond_duration","gov_bond_duration_mode",
  "initial_corp_bond_duration","corp_bond_duration_mode",
  "initial_alt_bond_duration","alt_bond_duration_mode",
  "interest_rate_cap_enabled","interest_rate_cap_strike",
  "equity_mean_reversion_enabled","special_pension_enabled",
  "liability_discount_spread","technical_rate_duration",
  "t_horizon","n_paths"
)

# Nur die Spalten einlesen, die wir wirklich brauchen -> spart RAM & Zeit.
KEEP_COLS <- unique(c(
  "path_nr","year","deckungsgrad","V_t","W_t","portfolio_return",
  STRATEGY_COLS, POPULATION_COLS
))

## ---------------------------------------------------------------------------
## 1) Hilfsfunktion: Auswertung EINES Strategie-Files
## ---------------------------------------------------------------------------
analyse_one_file <- function(path) {

  # Header lesen, um KEEP_COLS auf tatsaechlich vorhandene Spalten zu reduzieren
  hdr  <- names(fread(path, sep = ";", nrows = 0L, showProgress = FALSE))
  cols <- intersect(KEEP_COLS, hdr)

  dt <- fread(
    path, sep = ";", dec = ".", select = cols,
    showProgress = FALSE
  )
  if (nrow(dt) == 0L) return(NULL)

  setorder(dt, path_nr, year)

  ## --- Strategie- & Populations-Metadaten (1. Zeile reicht, da konstant) ----
  meta_cols <- intersect(c(STRATEGY_COLS, POPULATION_COLS), names(dt))
  meta <- dt[1L, ..meta_cols]

  ## --- Pfad-Endwerte (letztes Jahr je Pfad) --------------------------------
  last_year <- dt[, max(year)]
  end_dt <- dt[year == last_year,
               .(dg_end = deckungsgrad, W_end = W_t, V_end = V_t),
               by = path_nr]

  ## --- Jaehrliche Portfolio-Returns je Pfad (fuer Risiko/Performance) -------
  # portfolio_return ist bereits die Jahresrendite der Aktivseite.
  ret <- dt[, .(path_nr, year, r = portfolio_return,
                dg = deckungsgrad, W = W_t, V = V_t)]

  ## --- LDI: Surplus-Return (Aktiv- minus Passiv-Bewegung) -------------------
  # Funded-Status-Sicht: relative Aenderung des Deckungsgrads neutralisiert
  # Zinseffekte, die V_t und W_t gleichgerichtet treffen.
  #   surplus_return_t = dg_t / dg_{t-1} - 1
  # Zusaetzlich: Veraenderung Vermoegen vs. Veraenderung Verpflichtungen.
  ret[, dg_prev := shift(dg), by = path_nr]
  ret[, surplus_return := dg / dg_prev - 1]
  ret[, W_prev := shift(W), by = path_nr]
  ret[, V_prev := shift(V), by = path_nr]
  ret[, asset_growth := W / W_prev - 1]
  ret[, liab_growth  := V / V_prev - 1]

  # Pro Pfad: Mittel & SD der Jahres-Returns
  per_path <- ret[!is.na(r), .(
      mean_r        = mean(r),
      sd_r          = sd(r),
      mean_surplus  = mean(surplus_return,  na.rm = TRUE),
      sd_surplus    = sd(surplus_return,    na.rm = TRUE),
      sd_liab       = sd(liab_growth,       na.rm = TRUE)
    ), by = path_nr]

  ## --- Aggregation ueber alle Pfade ----------------------------------------
  # Klassische Risiko-/Performance-Kennzahlen auf Basis der Jahres-Returns
  all_r       <- ret[!is.na(r), r]
  all_surplus <- ret[!is.na(surplus_return), surplus_return]

  mean_ret    <- mean(all_r)
  vol         <- sd(all_r)                              # Volatilitaet (annual.)
  downside    <- all_r[all_r < RISK_FREE_ANNUAL]
  downside_dev <- if (length(downside) > 1)
                    sqrt(mean((downside - RISK_FREE_ANNUAL)^2)) else NA_real_
  sharpe      <- if (vol > 0) (mean_ret - RISK_FREE_ANNUAL) / vol else NA_real_
  sortino     <- if (!is.na(downside_dev) && downside_dev > 0)
                    (mean_ret - RISK_FREE_ANNUAL) / downside_dev else NA_real_

  # Information Ratio: Surplus-Return-Sicht (Aktiv ggue. Passiv = Benchmark).
  # Aktivrendite = surplus_return; Tracking-Error = SD(surplus_return).
  te          <- sd(all_surplus)
  active_ret  <- mean(all_surplus)
  info_ratio  <- if (te > 0) active_ret / te else NA_real_

  # Value-at-Risk / Expected Shortfall auf Jahres-Returns (5 %)
  var_05      <- quantile(all_r, 0.05, names = FALSE)
  es_05       <- mean(all_r[all_r <= var_05])

  # Max Drawdown des Deckungsgrads (pfadweise, dann Mittel & Worst)
  ret[, dg_run_max := cummax(dg), by = path_nr]
  ret[, dd := dg / dg_run_max - 1]
  dd_path <- ret[, .(max_dd = min(dd, na.rm = TRUE)), by = path_nr]

  ## --- Funding-/Unterdeckungs-Risiko ---------------------------------------
  # Wahrscheinlichkeit Unterdeckung am Ende und mindestens einmal im Horizont
  n_paths_eff <- end_dt[, .N]
  p_under_end <- end_dt[, mean(dg_end < FUNDING_THRESHOLD)]
  p_crit_end  <- end_dt[, mean(dg_end < CRITICAL_THRESHOLD)]
  ever_under  <- ret[, .(ever = any(dg < FUNDING_THRESHOLD)), by = path_nr]
  p_under_ever <- ever_under[, mean(ever)]
  # Erwarteter Fehlbetrag bei Unterdeckung (1 - DG, nur Unterdeckungsfaelle)
  shortfall    <- end_dt[dg_end < FUNDING_THRESHOLD, FUNDING_THRESHOLD - dg_end]
  exp_shortfall_cond <- if (length(shortfall) > 0) mean(shortfall) else 0

  ## --- Ergebniszeile zusammenstellen ---------------------------------------
  res <- data.table(
    file               = basename(path),

    # Performance
    mean_return        = mean_ret,
    median_dg_end      = end_dt[, median(dg_end)],
    mean_dg_end        = end_dt[, mean(dg_end)],
    p10_dg_end         = end_dt[, quantile(dg_end, 0.10, names = FALSE)],
    p90_dg_end         = end_dt[, quantile(dg_end, 0.90, names = FALSE)],

    # Risiko (klassisch)
    volatility         = vol,
    downside_dev       = downside_dev,
    sharpe_ratio       = sharpe,
    sortino_ratio      = sortino,
    var_5pct           = var_05,
    es_5pct            = es_05,
    mean_max_drawdown  = dd_path[, mean(max_dd)],
    worst_max_drawdown = dd_path[, min(max_dd)],

    # LDI / Surplus-Sicht
    surplus_return     = active_ret,
    tracking_error     = te,
    information_ratio  = info_ratio,
    mean_liab_vol      = per_path[, mean(sd_liab, na.rm = TRUE)],

    # Unterdeckungs-Risiko
    p_underfunded_end  = p_under_end,
    p_critical_end     = p_crit_end,
    p_underfunded_ever = p_under_ever,
    exp_shortfall_cond = exp_shortfall_cond,

    n_paths            = n_paths_eff
  )

  out <- cbind(res, meta)

  # Speicher sofort freigeben
  rm(dt, ret, per_path, end_dt, dd_path, ever_under); gc(verbose = FALSE)
  out
}

## ---------------------------------------------------------------------------
## 2) Streaming ueber alle Files
## ---------------------------------------------------------------------------
files <- list.files(INPUT_DIR, pattern = "\\.csv$", full.names = TRUE)
files <- files[!grepl("/output/", files, fixed = TRUE)]
stopifnot(length(files) > 0)

cat(sprintf("Gefundene Strategie-Files: %d\n", length(files)))

results <- vector("list", length(files))
for (i in seq_along(files)) {
  res <- tryCatch(analyse_one_file(files[i]),
                  error = function(e) {
                    warning(sprintf("Fehler bei %s: %s",
                                    basename(files[i]), conditionMessage(e)))
                    NULL
                  })
  results[[i]] <- res
  if (i %% 50 == 0 || i == length(files))
    cat(sprintf("  ... %d / %d verarbeitet\n", i, length(files)))
}

summary_dt <- rbindlist(results, use.names = TRUE, fill = TRUE)
rm(results); gc(verbose = FALSE)

## ---------------------------------------------------------------------------
## 3) Rankings & Ausgaben
## ---------------------------------------------------------------------------
# Hauptkriterium: geringstes Unterdeckungsrisiko, dann hoeherer End-Deckungsgrad
setorder(summary_dt, p_underfunded_end, -median_dg_end)
summary_dt[, rank_underfunding := .I]

# Sekundaer-Rankings
summary_dt[, rank_sharpe    := frank(-sharpe_ratio,      ties.method = "min")]
summary_dt[, rank_info      := frank(-information_ratio, ties.method = "min")]
summary_dt[, rank_drawdown  := frank(-mean_max_drawdown, ties.method = "min")]

# Gesamttabelle (ALLE Strategien, ungefiltert)
fwrite(summary_dt, file.path(OUTPUT_DIR, "strategy_summary.csv"),
       sep = ";", dec = ".")

## --- Eligible-Filter: keine Strategie mit P(jemals unterdeckt) > 5% in Tops --
MAX_UNDER_EVER <- 0.05
eligible <- summary_dt[p_underfunded_ever <= MAX_UNDER_EVER]
cat(sprintf("\nStrategien mit p_underfunded_ever <= %.0f%%: %d von %d\n",
            100 * MAX_UNDER_EVER, nrow(eligible), nrow(summary_dt)))

if (nrow(eligible) == 0L)
  warning("Keine Strategie erfuellt das Eligibility-Kriterium (p_underfunded_ever <= 5%).")

# Top-20 nach Unterdeckungsrisiko (Kernfrage des Auftrags), nur eligible
top_underfunding <- head(eligible[order(p_underfunded_end, -median_dg_end)], 20L)
fwrite(top_underfunding,
       file.path(OUTPUT_DIR, "top20_lowest_underfunding_risk.csv"),
       sep = ";", dec = ".")

# Top-20 nach Information Ratio (LDI-Effizienz), nur eligible
top_info <- head(eligible[order(-information_ratio)], 20L)
fwrite(top_info, file.path(OUTPUT_DIR, "top20_information_ratio.csv"),
       sep = ";", dec = ".")

## ---------------------------------------------------------------------------
## 4) Plots (Base R, keine Zusatzpakete)
## ---------------------------------------------------------------------------
png(file.path(OUTPUT_DIR, "risk_return_scatter.png"),
    width = 1000, height = 750, res = 110)
plot(summary_dt$volatility, summary_dt$mean_return,
     col = ifelse(summary_dt$p_underfunded_end < 0.05, "darkgreen", "grey50"),
     pch = 19, cex = 0.6,
     xlab = "Volatilitaet (Aktivseite)", ylab = "Mittlere Jahresrendite",
     main = "Risk-Return je Anlagestrategie\n(gruen: P(Unterdeckung Ende) < 5%)")
grid()
dev.off()

png(file.path(OUTPUT_DIR, "underfunding_vs_return.png"),
    width = 1000, height = 750, res = 110)
plot(summary_dt$p_underfunded_end, summary_dt$median_dg_end,
     pch = 19, cex = 0.6, col = "steelblue",
     xlab = "P(Deckungsgrad < 100% am Ende)",
     ylab = "Median Deckungsgrad am Ende",
     main = "Unterdeckungsrisiko vs. End-Deckungsgrad")
abline(h = FUNDING_THRESHOLD, lty = 2, col = "red")
grid()
dev.off()

png(file.path(OUTPUT_DIR, "ldi_surplus_efficiency.png"),
    width = 1000, height = 750, res = 110)
plot(summary_dt$tracking_error, summary_dt$surplus_return,
     pch = 19, cex = 0.6, col = "darkorange",
     xlab = "Tracking Error (SD Surplus-Return)",
     ylab = "Mittlerer Surplus-Return (DG-Wachstum)",
     main = "LDI-Sicht: Surplus-Return vs. Surplus-Risiko")
grid()
dev.off()

## ---------------------------------------------------------------------------
## 4b) Asset-Allocation-Plots (Rueckschluss Allokation -> Ergebnis)
## ---------------------------------------------------------------------------
WEIGHT_COLS <- c("weight_gov_bonds","weight_corp_bonds","weight_equities",
                 "weight_real_estate","weight_alternatives")
WEIGHT_LAB  <- c("Staatsanl.","Unternehm.anl.","Aktien","Immobilien","Alternative")
weight_have <- WEIGHT_COLS %in% names(summary_dt)

if (all(weight_have)) {

  # Aggregierte Bond-/Growth-Quoten als zusaetzliche Lesehilfe
  summary_dt[, w_bonds  := weight_gov_bonds + weight_corp_bonds]
  summary_dt[, w_growth := weight_equities + weight_real_estate + weight_alternatives]

  # (1) Jedes Asset-Gewicht vs. Unterdeckungsrisiko (Panel)
  png(file.path(OUTPUT_DIR, "alloc_vs_underfunding.png"),
      width = 1200, height = 800, res = 110)
  op <- par(mfrow = c(2, 3), mar = c(4, 4, 2.5, 1))
  for (k in seq_along(WEIGHT_COLS)) {
    plot(summary_dt[[WEIGHT_COLS[k]]], summary_dt$p_underfunded_ever,
         pch = 19, cex = 0.5, col = "grey40",
         xlab = paste0("Gewicht ", WEIGHT_LAB[k]),
         ylab = "P(jemals unterdeckt)",
         main = WEIGHT_LAB[k])
    abline(h = MAX_UNDER_EVER, lty = 2, col = "red")
    grid()
  }
  plot.new()
  legend("center", bty = "n",
         legend = c("rote Linie =", "Eligibility 5%"))
  par(op)
  dev.off()

  # (2) Jedes Asset-Gewicht vs. Rendite (Panel)
  png(file.path(OUTPUT_DIR, "alloc_vs_return.png"),
      width = 1200, height = 800, res = 110)
  op <- par(mfrow = c(2, 3), mar = c(4, 4, 2.5, 1))
  for (k in seq_along(WEIGHT_COLS)) {
    plot(summary_dt[[WEIGHT_COLS[k]]], summary_dt$mean_return,
         pch = 19, cex = 0.5,
         col = ifelse(summary_dt$p_underfunded_ever <= MAX_UNDER_EVER,
                      "darkgreen", "grey60"),
         xlab = paste0("Gewicht ", WEIGHT_LAB[k]),
         ylab = "Mittlere Jahresrendite",
         main = WEIGHT_LAB[k])
    grid()
  }
  plot.new()
  legend("center", bty = "n", pch = 19, col = c("darkgreen","grey60"),
         legend = c("eligible (<=5%)", "nicht eligible"))
  par(op)
  dev.off()

  # (3) Growth-Quote vs. Risiko, eingefaerbt nach Eligibility
  png(file.path(OUTPUT_DIR, "alloc_growthquote_risk.png"),
      width = 1000, height = 750, res = 110)
  plot(summary_dt$w_growth, summary_dt$volatility,
       pch = 19, cex = 0.6,
       col = ifelse(summary_dt$p_underfunded_ever <= MAX_UNDER_EVER,
                    "darkgreen", "grey60"),
       xlab = "Growth-Quote (Aktien + Immobilien + Alternative)",
       ylab = "Volatilitaet (Aktivseite)",
       main = "Allokation: Growth-Quote vs. Risiko")
  legend("topleft", bty = "n", pch = 19, col = c("darkgreen","grey60"),
         legend = c("eligible (<=5%)", "nicht eligible"))
  grid()
  dev.off()

  # (4) Heatmap-artig: mittlere Kennzahl je (Aktien-, Bond-)Gewichtsraster
  #     Aggregiert ueber gerundete Gewichte -> zeigt Allokations-Sweetspots.
  hm <- copy(summary_dt)
  hm[, eq_bin   := round(weight_equities,    2)]
  hm[, bond_bin := round(w_bonds,            2)]
  grid_dt <- hm[, .(p_under = mean(p_underfunded_ever),
                    ret     = mean(mean_return),
                    n       = .N),
                by = .(eq_bin, bond_bin)]
  fwrite(grid_dt, file.path(OUTPUT_DIR, "alloc_grid_summary.csv"),
         sep = ";", dec = ".")

  png(file.path(OUTPUT_DIR, "alloc_grid_underfunding.png"),
      width = 1000, height = 800, res = 110)
  # Farbskala: gruen (gering) -> rot (hoch)
  pal <- colorRampPalette(c("darkgreen","gold","red"))(100)
  z   <- grid_dt$p_under
  idx <- as.integer(cut(z, breaks = 100, include.lowest = TRUE))
  plot(grid_dt$eq_bin, grid_dt$bond_bin, pch = 15, cex = 2.2,
       col = pal[idx],
       xlab = "Aktien-Gewicht", ylab = "Anleihen-Gewicht (Staat+Untern.)",
       main = "Allokations-Raster: P(jemals unterdeckt)")
  grid()
  legend("topright", bty = "n", pch = 15, col = pal[c(1, 50, 100)],
         legend = c("gering", "mittel", "hoch"))
  dev.off()

  rm(hm, grid_dt); gc(verbose = FALSE)

} else {
  warning("Gewichtsspalten fehlen – Asset-Allocation-Plots uebersprungen.")
}

## ---------------------------------------------------------------------------
## 5) Konsolen-Report
## ---------------------------------------------------------------------------
cat("\n==================== AUSWERTUNG ABGESCHLOSSEN ====================\n")
cat(sprintf("Strategien ausgewertet : %d\n", nrow(summary_dt)))
cat(sprintf("Ausgabeverzeichnis     : %s\n", normalizePath(OUTPUT_DIR)))
cat("\nFixe Population (Dokumentation, keine Auswertungsdimension):\n")
pop_present <- intersect(POPULATION_COLS, names(summary_dt))
print(t(summary_dt[1L, ..pop_present]))

cat("\n--- Top 5: geringstes Unterdeckungsrisiko (nur eligible) ---\n")
print(top_underfunding[1:min(5L, nrow(top_underfunding)),
                       .(file, p_underfunded_end, p_underfunded_ever,
                         median_dg_end, sharpe_ratio, information_ratio)])
cat("\n==================================================================\n")
