###############################################################################
# sammelstiftung_analysis.R
#
# Auswertung der Sensitivitaets-Szenarien zum SAMMELSTIFTUNGS-MODUS (Dimension D).
# Analog zu strategy_analysis.R: jedes Szenario-CSV (~n_paths MC-Pfade x
# t_horizon Jahre, 90+ Spalten) wird EINZELN geladen, zu wenigen Kennzahlen
# verdichtet und sofort wieder aus dem RAM entfernt -> nie mehr als ein File
# gleichzeitig im Speicher.
#
# Fokus der Auswertung:
#   Wahrscheinlichkeit von Unterdeckung (Deckungsgrad < 1.00) und
#   Default / kritischer Unterdeckung (Deckungsgrad < 0.95)
#   in Abhaengigkeit der drei Sammelstiftungs-Szenarien:
#
#     S0  Basis                        : SAMMELSTIFTUNG_ENABLED = FALSE, SPECIAL_PENSION_ENABLED = FALSE
#     S1  Sammelstiftung ohne Sonderr. : SAMMELSTIFTUNG_ENABLED = TRUE , SPECIAL_PENSION_ENABLED = FALSE
#     S2  Sammelstiftung mit Sonderr.  : SAMMELSTIFTUNG_ENABLED = TRUE , SPECIAL_PENSION_ENABLED = TRUE
#
#   (Die Kombination SAMMELSTIFTUNG=FALSE + SPECIAL_PENSION=TRUE existiert nicht.)
#
# Jede Kennzahl wird sowohl als "ever" (mind. einmal im Horizont) als auch als
# "end" (am Ende des Horizonts) ausgewiesen.
#
# Datenformat (pro Szenario-CSV, eine Zeile je Pfad-Jahr):
#   path_nr;year;deckungsgrad;V_t;W_t; ... ;sammelstiftung_enabled;
#   special_pension_enabled;sammelstiftung_interval;special_pension_threshold;
#   special_pension_target; ...
#   Trennzeichen ";", Dezimalpunkt "." (Swiss-Export)
#
# Input : alle *.csv in  data/sammelstiftung/
# Output: data/sammelstiftung/output/  (Tabellen + Plots)
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

## ---------------------------------------------------------------------------
## 0) Konfiguration
## ---------------------------------------------------------------------------
# Verzeichnis mit den Szenario-CSVs aus sensitivity_analysis.py.
# Default des Python-Skripts ist data/sensitivity; bei einem dedizierten
# Sammelstiftungs-Lauf z.B. via --output_dir data/sammelstiftung getrennt.
INPUT_DIR  <- "data/sensitivity"
OUTPUT_DIR <- file.path(INPUT_DIR, "output_sammelstiftung")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Schwellen
UNDERFUNDED_THRESHOLD <- 1.00   # Deckungsgrad < 1.00  => Unterdeckung
CRITICAL_THRESHOLD    <- 0.95   # Deckungsgrad < 0.95  => Default / kritisch

# Deckungsgrad-Cap fuer LAGEMASSE (Median/Mean/Quantile) und DG-Plots.
# Im Modell wird der DG bei (W_t -> 0, V_t > 0) auf 200% gesetzt; einzelne
# Pfade koennen so extreme Werte (>>2) annehmen und Median/Plots verzerren.
# Die WAHRSCHEINLICHKEITS-Kennzahlen (P unter Schwelle) sind davon NICHT
# betroffen und werden ungecappt aus dem Roh-DG berechnet.
DG_CAP <- 2.0

setDTthreads(0L)  # alle Kerne

# Szenario-identifizierende Spalten (konstant innerhalb eines Files)
SCENARIO_COLS <- c(
  "sammelstiftung_enabled", "special_pension_enabled",
  "sammelstiftung_interval", "special_pension_threshold",
  "special_pension_target"
)

# Populations-/Doku-Spalten (fix im Lauf, nur Dokumentation)
POPULATION_COLS <- c(
  "bestand_n_total","bestand_avg_age","bestand_share_m","bestand_share_f",
  "bestand_share_married","bestand_pension_mean","bestand_W0",
  "bestand_spouse_pension_rate"
)

# Asset-Gewichte (konstant je File = eine Allokation)
WEIGHT_COLS <- c(
  "weight_gov_bonds","weight_corp_bonds","weight_equities",
  "weight_real_estate","weight_alternatives"
)

KEEP_COLS <- unique(c(
  "path_nr","year","deckungsgrad","V_t","W_t","special_pension_payout",
  SCENARIO_COLS, WEIGHT_COLS, POPULATION_COLS
))

## ---------------------------------------------------------------------------
## Hilfsfunktion: Szenario-Label aus den beiden Flags ableiten
## ---------------------------------------------------------------------------
to_bool <- function(x) {
  # robust gegen "True"/"False", "1"/"0", TRUE/FALSE, 1/0
  if (is.logical(x)) return(x)
  xc <- tolower(trimws(as.character(x)))
  xc %in% c("true","1","t","yes","ja")
}

scenario_label <- function(samm, spec) {
  if (!samm)            return("S0 Basis (keine Sammelstiftung)")
  if (samm && !spec)    return("S1 Sammelstiftung ohne Sonderrente")
  "S2 Sammelstiftung mit Sonderrente"
}
scenario_code <- function(samm, spec) {
  if (!samm)         return("S0")
  if (samm && !spec) return("S1")
  "S2"
}

## ---------------------------------------------------------------------------
## 1) Auswertung EINES Szenario-Files
## ---------------------------------------------------------------------------
analyse_one_file <- function(path) {

  hdr  <- names(fread(path, sep = ";", nrows = 0L, showProgress = FALSE))
  # Pfad-Exporte muessen path_nr UND year enthalten; sonst ueberspringen
  # (z.B. versehentlich mitgelesene Aggregat-CSVs).
  if (!all(c("path_nr","year","deckungsgrad") %in% hdr)) return(NULL)
  cols <- intersect(KEEP_COLS, hdr)

  dt <- fread(path, sep = ";", dec = ".", select = cols, showProgress = FALSE)
  if (nrow(dt) == 0L) return(NULL)
  setorder(dt, path_nr, year)

  ## --- Szenario- & Populations-Metadaten (1. Zeile reicht, konstant) -------
  samm <- to_bool(dt[["sammelstiftung_enabled"]][1L])
  spec <- to_bool(dt[["special_pension_enabled"]][1L])

  # Nur Spalten uebernehmen, die NICHT bereits in res stehen
  # (Szenario-Flags sind schon in res -> sonst Doppelspalten beim cbind)
  meta_cols <- intersect(c(WEIGHT_COLS, POPULATION_COLS), names(dt))
  meta <- dt[1L, ..meta_cols]

  ## --- Endwerte je Pfad (letztes Jahr) -------------------------------------
  last_year <- dt[, max(year)]
  end_dt <- dt[year == last_year, .(dg_end = deckungsgrad), by = path_nr]
  # gecappte Variante nur fuer Lagemasse/Plots
  end_dt[, dg_end_cap := pmin(dg_end, DG_CAP)]

  ## --- Pfadweise: jemals unter Schwelle ------------------------------------
  ever_dt <- dt[, .(
      under_ever = any(deckungsgrad < UNDERFUNDED_THRESHOLD),
      crit_ever  = any(deckungsgrad < CRITICAL_THRESHOLD),
      dg_min     = min(deckungsgrad)
    ), by = path_nr]

  n_paths_eff <- end_dt[, .N]

  ## --- Sonderrenten-Auszahlungen (nur relevant in S2) ----------------------
  has_spp <- "special_pension_payout" %in% names(dt)
  spp_total_mean <- if (has_spp)
    dt[, .(s = sum(special_pension_payout, na.rm = TRUE)), by = path_nr][, mean(s)] else NA_real_
  p_spp_ever <- if (has_spp)
    dt[, .(any_spp = any(special_pension_payout > 0, na.rm = TRUE)), by = path_nr][, mean(any_spp)] else NA_real_

  ## --- Kennzahlen ----------------------------------------------------------
  res <- data.table(
    file        = basename(path),
    scenario    = scenario_code(samm, spec),
    scenario_lab = scenario_label(samm, spec),
    sammelstiftung_enabled = samm,
    special_pension_enabled = spec,

    # Unterdeckung (DG < 1.00) -- am ROH-Deckungsgrad
    p_underfunded_end  = end_dt[, mean(dg_end < UNDERFUNDED_THRESHOLD)],
    p_underfunded_ever = ever_dt[, mean(under_ever)],

    # Default / kritisch (DG < 0.95) -- am ROH-Deckungsgrad
    p_critical_end     = end_dt[, mean(dg_end < CRITICAL_THRESHOLD)],
    p_critical_ever    = ever_dt[, mean(crit_ever)],

    # Deckungsgrad-Lage -- GECAPPT (robust gegen W_t->0 Ausreisser)
    median_dg_end      = end_dt[, median(dg_end_cap)],
    mean_dg_end        = end_dt[, mean(dg_end_cap)],
    p10_dg_end         = end_dt[, quantile(dg_end_cap, 0.10, names = FALSE)],
    p90_dg_end         = end_dt[, quantile(dg_end_cap, 0.90, names = FALSE)],
    min_dg_path_mean   = ever_dt[, mean(pmin(dg_min, DG_CAP))],
    worst_dg_min       = ever_dt[, min(dg_min)],

    # Sonderrente (Diagnostik)
    spp_total_mean     = spp_total_mean,
    p_special_pension_ever = p_spp_ever,

    n_paths            = n_paths_eff
  )

  out <- cbind(res, meta)
  rm(dt, end_dt, ever_dt); gc(verbose = FALSE)
  out
}

## ---------------------------------------------------------------------------
## 2) Streaming ueber alle Files
## ---------------------------------------------------------------------------
files <- list.files(INPUT_DIR, pattern = "\\.csv$", full.names = TRUE)
files <- files[!grepl("/output", files, fixed = TRUE)]
# Aggregat-/Uebersichts-CSVs ohne Pfaddaten ausschliessen
# (sensitivity_summary.csv, *_summary.csv, *_aggregate.csv etc.)
files <- files[!grepl("summary|aggregate|delta", basename(files), ignore.case = TRUE)]
# Nur die eigentlichen Szenario-Pfad-Exporte behalten, falls Namensmuster passt
scenario_files <- files[grepl("^sensitivity_scenario_", basename(files))]
if (length(scenario_files) > 0) files <- scenario_files
stopifnot(length(files) > 0)

cat(sprintf("Gefundene Szenario-Files: %d\n", length(files)))

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

# Szenario als geordneter Faktor (S0 < S1 < S2)
summary_dt[, scenario := factor(scenario, levels = c("S0","S1","S2"))]
summary_dt[, scenario_lab := factor(
  scenario_lab,
  levels = c("S0 Basis (keine Sammelstiftung)",
             "S1 Sammelstiftung ohne Sonderrente",
             "S2 Sammelstiftung mit Sonderrente"))]

# Gesamttabelle (alle Files)
fwrite(summary_dt, file.path(OUTPUT_DIR, "sammelstiftung_summary.csv"),
       sep = ";", dec = ".")

## ---------------------------------------------------------------------------
## 3) Aggregation je Szenario (Kernfrage)
## ---------------------------------------------------------------------------
# Mittel ueber alle Files je Szenario; gewichtet nach n_paths fuer
# pfad-konsistente Wahrscheinlichkeiten.
agg <- summary_dt[, .(
    n_files            = .N,
    n_paths_total      = sum(n_paths),

    p_underfunded_end  = weighted.mean(p_underfunded_end,  n_paths),
    p_underfunded_ever = weighted.mean(p_underfunded_ever, n_paths),
    p_critical_end     = weighted.mean(p_critical_end,     n_paths),
    p_critical_ever    = weighted.mean(p_critical_ever,    n_paths),

    median_dg_end      = median(median_dg_end),
    mean_dg_end        = weighted.mean(mean_dg_end, n_paths),
    p10_dg_end         = mean(p10_dg_end),
    p90_dg_end         = mean(p90_dg_end),
    worst_dg_min       = min(worst_dg_min),

    p_special_pension_ever = weighted.mean(p_special_pension_ever, n_paths),
    spp_total_mean     = weighted.mean(spp_total_mean, n_paths)
  ), by = .(scenario, scenario_lab)]

setorder(agg, scenario)
fwrite(agg, file.path(OUTPUT_DIR, "sammelstiftung_aggregate.csv"),
       sep = ";", dec = ".")

cat("\n==================== AGGREGAT JE SZENARIO ====================\n")
print(agg[, .(scenario_lab, n_files, n_paths_total,
              p_underfunded_end, p_underfunded_ever,
              p_critical_end, p_critical_ever, median_dg_end)])

## ---------------------------------------------------------------------------
## 4) Plots (ggplot2)
## ---------------------------------------------------------------------------
THEME <- theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold"),
        legend.position = "bottom")

scen_cols <- c("S0" = "#4575b4", "S1" = "#fdae61", "S2" = "#d73027")

## --- (1) Balkendiagramm: P(Unterdeckung/Default) je Szenario, ever + end ---
plot_dt <- melt(
  agg,
  id.vars = c("scenario","scenario_lab"),
  measure.vars = c("p_underfunded_end","p_underfunded_ever",
                   "p_critical_end","p_critical_ever"),
  variable.name = "metric", value.name = "p"
)
metric_lab <- c(
  p_underfunded_end  = "Unterdeckung (DG<1.00), Ende",
  p_underfunded_ever = "Unterdeckung (DG<1.00), jemals",
  p_critical_end     = "Default (DG<0.95), Ende",
  p_critical_ever    = "Default (DG<0.95), jemals"
)
plot_dt[, metric := factor(metric_lab[as.character(metric)],
                           levels = metric_lab)]

p1 <- ggplot(plot_dt, aes(x = metric, y = p, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", 100 * p)),
            position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = scen_cols, name = NULL,
                    labels = levels(agg$scenario_lab)) +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", 100 * x),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Unterdeckungs- und Default-Wahrscheinlichkeit je Sammelstiftungs-Szenario",
       x = NULL, y = "Wahrscheinlichkeit") +
  THEME +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave(file.path(OUTPUT_DIR, "p_underfunding_by_scenario.png"),
       p1, width = 11, height = 6.5, dpi = 120)

## --- (2) Verteilung End-Deckungsgrad je Szenario --------------------------
# Robust gegen wenige Files je Szenario: Jitter-Punkte (jedes File ein Punkt)
# plus Median-Crossbar. Boxplot allein bliebe bei 1 File je Szenario leer.
p2 <- ggplot(summary_dt, aes(x = scenario, y = median_dg_end, colour = scenario)) +
  geom_violin(aes(fill = scenario), alpha = 0.15, colour = NA,
              scale = "width", trim = FALSE) +
  geom_jitter(width = 0.12, height = 0, alpha = 0.6, size = 1.6) +
  stat_summary(fun = median, geom = "crossbar", width = 0.5,
               colour = "black", linewidth = 0.4) +
  geom_hline(yintercept = UNDERFUNDED_THRESHOLD, linetype = 2, colour = "grey30") +
  geom_hline(yintercept = CRITICAL_THRESHOLD,    linetype = 3, colour = "red") +
  scale_colour_manual(values = scen_cols, guide = "none") +
  scale_fill_manual(values = scen_cols, guide = "none") +
  labs(title = "Verteilung des Median-End-Deckungsgrads je Szenario",
       subtitle = "jeder Punkt = ein Allokations-File  |  gestrichelt: Unterdeckung 1.00  |  gepunktet rot: Default 0.95",
       x = NULL, y = "Median Deckungsgrad am Ende") +
  THEME

ggsave(file.path(OUTPUT_DIR, "dg_end_distribution_by_scenario.png"),
       p2, width = 9, height = 6, dpi = 120)

## --- (3) Default-ever vs. Median-DG, eingefaerbt nach Szenario -------------
p3 <- ggplot(summary_dt, aes(x = p_critical_ever, y = median_dg_end,
                             colour = scenario)) +
  geom_point(alpha = 0.6, size = 1.6) +
  geom_vline(xintercept = 0.05, linetype = 2, colour = "grey50") +
  scale_colour_manual(values = scen_cols, name = NULL,
                      labels = levels(summary_dt$scenario_lab)) +
  scale_x_continuous(labels = function(x) sprintf("%.0f%%", 100 * x)) +
  labs(title = "Default-Risiko (jemals DG<0.95) vs. End-Deckungsgrad",
       x = "P(jemals Default, DG<0.95)", y = "Median Deckungsgrad am Ende") +
  THEME

ggsave(file.path(OUTPUT_DIR, "default_ever_vs_dg_end.png"),
       p3, width = 9, height = 6, dpi = 120)

## --- (3b) Scatter-Set: P(jemals unterdeckt) vs. Asset-Gewicht -------------
# Ein Punkt je Allokations-File, Farbe = Szenario (S0/S1/S2).
# Panel je Asset-Klasse (analog zum bereitgestellten Referenz-Layout).
weight_labels <- c(
  weight_gov_bonds    = "Staatsanl.",
  weight_corp_bonds   = "Unternehm.anl.",
  weight_equities     = "Aktien",
  weight_real_estate  = "Immobilien",
  weight_alternatives = "Alternative"
)

present_weights <- intersect(WEIGHT_COLS, names(summary_dt))
if (length(present_weights) > 0) {

  scatter_dt <- melt(
    summary_dt,
    id.vars = c("scenario","scenario_lab",
                "p_underfunded_ever","p_critical_ever","median_dg_end"),
    measure.vars = present_weights,
    variable.name = "asset", value.name = "weight"
  )
  scatter_dt[, asset_lab := factor(weight_labels[as.character(asset)],
                                   levels = weight_labels[present_weights])]

  ## (3b-i) y = P(jemals unterdeckt, DG<1.00) -------------------------------
  p_scatter_uf <- ggplot(scatter_dt,
                         aes(x = weight, y = p_underfunded_ever, colour = scenario)) +
    geom_point(alpha = 0.55, size = 1.4) +
    geom_hline(yintercept = 0.05, linetype = 2, colour = "grey40") +
    facet_wrap(~ asset_lab, scales = "free_x") +
    scale_colour_manual(values = scen_cols, name = NULL,
                        labels = levels(summary_dt$scenario_lab)) +
    scale_x_continuous(labels = function(x) sprintf("%.2f", x)) +
    labs(title = "Unterdeckungs-Wahrscheinlichkeit vs. Asset-Gewicht je Szenario",
         x = NULL, y = "P(jemals unterdeckt, DG<1.00)",
         caption = "gestrichelt: Eligibility 5%") +
    THEME

  ggsave(file.path(OUTPUT_DIR, "alloc_vs_underfunding_by_scenario.png"),
         p_scatter_uf, width = 12, height = 7.5, dpi = 120)

  ## (3b-ii) y = P(jemals Default, DG<0.95) --------------------------------
  p_scatter_def <- ggplot(scatter_dt,
                          aes(x = weight, y = p_critical_ever, colour = scenario)) +
    geom_point(alpha = 0.55, size = 1.4) +
    geom_hline(yintercept = 0.05, linetype = 2, colour = "grey40") +
    facet_wrap(~ asset_lab, scales = "free_x") +
    scale_colour_manual(values = scen_cols, name = NULL,
                        labels = levels(summary_dt$scenario_lab)) +
    scale_x_continuous(labels = function(x) sprintf("%.2f", x)) +
    labs(title = "Default-Wahrscheinlichkeit vs. Asset-Gewicht je Szenario",
         x = NULL, y = "P(jemals Default, DG<0.95)",
         caption = "gestrichelt: Eligibility 5%") +
    THEME

  ggsave(file.path(OUTPUT_DIR, "alloc_vs_default_by_scenario.png"),
         p_scatter_def, width = 12, height = 7.5, dpi = 120)
}


## --- (4) Differenz-Sicht: Effekt Sammelstiftung & Sonderrente --------------
# Delta gegenueber Basis S0 (gleiche Population/Strategie vorausgesetzt; hier
# als Aggregat-Differenz interpretiert).
base_vals <- agg[scenario == "S0",
                 .(p_underfunded_ever, p_critical_ever, median_dg_end)]
if (nrow(base_vals) == 1L) {
  delta <- agg[, .(
      scenario, scenario_lab,
      d_p_underfunded_ever = p_underfunded_ever - base_vals$p_underfunded_ever,
      d_p_critical_ever    = p_critical_ever    - base_vals$p_critical_ever,
      d_median_dg_end      = median_dg_end      - base_vals$median_dg_end
  )]
  fwrite(delta, file.path(OUTPUT_DIR, "sammelstiftung_delta_vs_base.csv"),
         sep = ";", dec = ".")

  cat("\n--- Effekt gegenueber Basis S0 (Aggregat-Differenz) ---\n")
  print(delta)
}

## ---------------------------------------------------------------------------
## 5) Konsolen-Report
## ---------------------------------------------------------------------------
cat("\n==================== AUSWERTUNG ABGESCHLOSSEN ====================\n")
cat(sprintf("Szenario-Files ausgewertet : %d\n", nrow(summary_dt)))
cat(sprintf("Ausgabeverzeichnis         : %s\n", normalizePath(OUTPUT_DIR)))
cat("\nFixe Population (Dokumentation):\n")
pop_present <- intersect(POPULATION_COLS, names(summary_dt))
print(t(summary_dt[1L, ..pop_present]))
cat("\n=================================================================\n")
