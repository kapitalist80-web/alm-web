# ==============================================================================
# cashflow_fan_chart.R
# ------------------------------------------------------------------------------
# Einleitungs-Chart für die Sensitivitätsanalyse:
# Entwicklung des absoluten Cashflows (Median + P5/P95-Band) über 40 Jahre
# aus einer einzigen Simulation mit homogenem Bestand (500–1000 Pfade).
#
# Verwendung:
#   source("cashflow_fan_chart.R")
#   cashflow_fan_chart("data/mc_all_paths_20250601_120000.csv")
#   cashflow_fan_chart()   # öffnet Datei-Dialog
# ==============================================================================

cashflow_fan_chart <- function(
    csv_path      = NULL,        # Pfad zur CSV; NULL öffnet Datei-Dialog
    title_extra   = NULL,        # Optionaler Zusatz im Titel (z.B. Allokation)
    chf_unit      = "Mio. CHF",  # "CHF" oder "Mio. CHF" oder "Tsd. CHF"
    color_median  = "#1a6faf",   # Farbe der Medianlinie
    color_band    = "#1a6faf",   # Farbe des P5-P95-Bands
    save_plot     = FALSE,       # Plot als PNG speichern?
    save_path     = "cashflow_fan_chart.png",
    width_in      = 10,
    height_in     = 5.5
) {
  suppressPackageStartupMessages({
    library(data.table)
    library(ggplot2)
    library(scales)
  })

  # ---- 1. Datei wählen / laden ----------------------------------------------
  if (is.null(csv_path)) {
    if (interactive()) {
      csv_path <- file.choose()
    } else {
      stop("csv_path muss angegeben werden (kein interaktiver Modus).")
    }
  }

  cat(sprintf("Lade: %s\n", basename(csv_path)))
  t0 <- Sys.time()

  dt <- fread(
    csv_path,
    sep          = ";",
    dec          = ".",
    select       = c("path_nr", "year", "cashflow_rent"),
    stringsAsFactors = FALSE,
    showProgress = FALSE
  )

  dt[, cashflow_rent := as.numeric(cashflow_rent)]
  dt[, year          := as.integer(year)]
  dt[, path_nr       := as.integer(path_nr)]

  n_paths <- uniqueN(dt$path_nr)
  n_years <- uniqueN(dt$year)
  cat(sprintf("  %d Pfade × %d Jahre = %s Zeilen  (%.1f Sek.)\n",
              n_paths, n_years,
              format(nrow(dt), big.mark = "'"),
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))

  # ---- 2. Skalierungsfaktor --------------------------------------------------
  scale_factor <- dplyr::case_when(
    chf_unit == "Mio. CHF" ~ 1e-6,
    chf_unit == "Tsd. CHF" ~ 1e-3,
    TRUE                   ~ 1
  )

  # ---- 3. Quantile pro Jahr --------------------------------------------------
  agg <- dt[, .(
    p05    = quantile(cashflow_rent, 0.05, na.rm = TRUE),
    p25    = quantile(cashflow_rent, 0.25, na.rm = TRUE),
    median = median(cashflow_rent,         na.rm = TRUE),
    p75    = quantile(cashflow_rent, 0.75, na.rm = TRUE),
    p95    = quantile(cashflow_rent, 0.95, na.rm = TRUE),
    mean   = mean(cashflow_rent,           na.rm = TRUE)
  ), by = year][order(year)]

  agg[, `:=`(
    p05    = p05    * scale_factor,
    p25    = p25    * scale_factor,
    median = median * scale_factor,
    p75    = p75    * scale_factor,
    p95    = p95    * scale_factor,
    mean   = mean   * scale_factor
  )]

  # Endwerte für Beschriftung rechts
  last <- agg[year == max(year)]

  # ---- 4. Metadaten aus CSV lesen (Populationsdetails) ----------------------
  pop_cols <- c(
    "bestand_n_total",             # Anzahl Rentner
    "bestand_avg_age",             # Durchschnittsalter
    "bestand_pension_mean",        # Durchschnittliche Jahresrente
    "bestand_share_married",       # Anteil Verheiratete
    "bestand_share_f",             # Frauenanteil
    "pop_share_female",            # Frauenanteil (alternativ)
    "bestand_spouse_pension_rate", # Ehegattenrente in % der Rente
    "bestand_spouse_age_diff_mean" # Altersunterschied Ehegatte
  )

  avail_header <- names(fread(csv_path, sep = ";", nrows = 0L, showProgress = FALSE))
  dt_meta <- fread(csv_path, sep = ";", dec = ".", nrows = 1L,
                   select       = intersect(pop_cols, avail_header),
                   showProgress = FALSE)

  # Hilfsfunktion: Wert aus Meta lesen (NA-sicher)
  .m <- function(col) {
    if (col %in% names(dt_meta) && !is.na(dt_meta[[col]])) dt_meta[[col]] else NULL
  }

  # Frauenanteil: bestand_share_f hat Vorrang, Fallback pop_share_female
  share_f <- if (!is.null(.m("bestand_share_f"))) .m("bestand_share_f")               else .m("pop_share_female")

  # Zeile 1: Bestandseckdaten
  row1 <- character(0)
  if (!is.null(.m("bestand_n_total")))
    row1 <- c(row1, sprintf("n = %d Rentner", as.integer(.m("bestand_n_total"))))
  if (!is.null(.m("bestand_avg_age")))
    row1 <- c(row1, sprintf("Ø Alter %.1f J.", .m("bestand_avg_age")))
  if (!is.null(.m("bestand_pension_mean")))
    row1 <- c(row1, sprintf("Ø Rente CHF %s/J.",
                            format(round(.m("bestand_pension_mean")), big.mark = "'")))

  # Zeile 2: Demographische Zusammensetzung
  row2 <- character(0)
  if (!is.null(.m("bestand_share_married")))
    row2 <- c(row2, sprintf("Verheiratet %.0f%%", .m("bestand_share_married") * 100))
  if (!is.null(share_f))
    row2 <- c(row2, sprintf("Frauen %.0f%%", share_f * 100))
  if (!is.null(.m("bestand_spouse_pension_rate")))
    row2 <- c(row2, sprintf("Ehegattenrente %.0f%% der Rente",
                            .m("bestand_spouse_pension_rate") * 100))
  if (!is.null(.m("bestand_spouse_age_diff_mean"))) {
    diff_val <- .m("bestand_spouse_age_diff_mean")
    row2 <- c(row2, sprintf("Altersunterschied Ehegatte %+.1f J.", diff_val))
  }

  # Zusammensetzen: zwei Zeilen mit Newline trennen
  subtitle_line1 <- paste(row1, collapse = "  ·  ")
  subtitle_line2 <- paste(row2, collapse = "  ·  ")
  subtitle <- if (nchar(subtitle_line2) > 0)
    paste(subtitle_line1, subtitle_line2, sep = "
")
  else
    subtitle_line1

  if (!is.null(title_extra)) subtitle <- paste0(title_extra, "
", subtitle)

  main_title <- sprintf(
    "Cashflow-Entwicklung über %d Simulationsjahre  (%d Monte-Carlo-Pfade)",
    n_years, n_paths
  )

  # ---- 5. Plot ---------------------------------------------------------------
  p <- ggplot(agg, aes(x = year)) +

    # P5-P95 äusseres Band
    geom_ribbon(aes(ymin = p05, ymax = p95),
                fill = color_band, alpha = 0.12) +

    # P25-P75 inneres Band
    geom_ribbon(aes(ymin = p25, ymax = p75),
                fill = color_band, alpha = 0.22) +

    # P5 / P95 gestrichelt
    geom_line(aes(y = p05), color = color_band,
              linetype = "dashed", linewidth = 0.55, alpha = 0.7) +
    geom_line(aes(y = p95), color = color_band,
              linetype = "dashed", linewidth = 0.55, alpha = 0.7) +

    # P25 / P75 gepunktet
    geom_line(aes(y = p25), color = color_band,
              linetype = "dotted", linewidth = 0.45, alpha = 0.6) +
    geom_line(aes(y = p75), color = color_band,
              linetype = "dotted", linewidth = 0.45, alpha = 0.6) +

    # Medianlinie
    geom_line(aes(y = median), color = color_median, linewidth = 1.1) +
    geom_point(data = agg[year %% 5 == 0 | year == 1],
               aes(y = median),
               color = color_median, size = 1.8, alpha = 0.85) +

    # Nulllinie
    geom_hline(yintercept = 0, color = "grey60", linewidth = 0.35) +

    # Achsen
    scale_x_continuous(
      breaks = c(1, seq(5, n_years, by = 5)),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(
      labels = function(x) {
        ifelse(x < 0,
               paste0("−", format(abs(x), big.mark = "'", nsmall = 1)),
               format(x, big.mark = "'", nsmall = 1))
      },
      expand = expansion(mult = c(0.05, 0.05))
    ) +

    labs(
      title    = main_title,
      subtitle = subtitle,
      x        = "Simulationsjahr",
      y        = sprintf("Cashflow (%s)", chf_unit),
      caption  = paste0(
        "Band: P5–P95 (hell) und P25–P75 (dunkel)  |  Linie: Median  |  ",
        "Quelle: ", basename(csv_path)
      )
    ) +

    theme_minimal(base_size = 11) +
    theme(
      plot.title        = element_text(face = "bold", size = 13),
      plot.subtitle     = element_text(color = "grey40", size = 9.5),
      plot.caption      = element_text(color = "grey55", size = 7.5,
                                       hjust = 0),
      axis.title        = element_text(size = 9.5),
      axis.text         = element_text(size = 9),
      panel.grid.minor  = element_blank(),
      panel.grid.major.x = element_line(color = "grey92"),
      panel.grid.major.y = element_line(color = "grey88"),
      legend.position   = "none",
      plot.margin       = margin(t = 10, r = 2, b = 8, l = 2)
    )

  # In RStudio: Plot-Grösse auf volle Breite setzen
  old_opts <- options(
    repr.plot.width  = 14,
    repr.plot.height = 5.5
  )
  on.exit(options(old_opts), add = TRUE)

  print(p)

  # ---- 6. Speichern (optional) -----------------------------------------------
  if (save_plot) {
    ggsave(save_path, plot = p, width = width_in, height = height_in,
           dpi = 300, bg = "white")
    cat(sprintf("Gespeichert: %s\n", save_path))
  }

  # ---- 7. Konsolen-Zusammenfassung -------------------------------------------
  cat(sprintf("\n── Cashflow-Zusammenfassung ──────────────────────────────\n"))
  cat(sprintf("  Jahr 1:   Median %s  [P5: %s, P95: %s]\n",
      .fmt_cf(agg$median[1], chf_unit),
      .fmt_cf(agg$p05[1],    chf_unit),
      .fmt_cf(agg$p95[1],    chf_unit)))
  cat(sprintf("  Jahr %d:  Median %s  [P5: %s, P95: %s]\n",
      n_years,
      .fmt_cf(last$median, chf_unit),
      .fmt_cf(last$p05,    chf_unit),
      .fmt_cf(last$p95,    chf_unit)))
  veraend <- (last$median - agg$median[1]) / abs(agg$median[1]) * 100
  cat(sprintf("  Veränderung Median J1→J%d: %+.1f%%\n", n_years, veraend))
  cat(sprintf("  Bandbreite J%d (P95-P5):   %s  (%.0f%% des Medians)\n",
      n_years,
      .fmt_cf(last$p95 - last$p05, chf_unit),
      (last$p95 - last$p05) / abs(last$median) * 100))
  cat("──────────────────────────────────────────────────────────\n\n")

  invisible(list(data = agg, plot = p))
}


# ---- Hilfsfunktion: formatierter Cashflow-Wert ------------------------------
.fmt_cf <- function(x, unit = "Mio. CHF") {
  if (is.na(x) || !is.finite(x)) return("—")
  sign_str <- if (x < 0) "−" else ""
  x <- abs(x)
  if (unit == "Mio. CHF") {
    sprintf("%s%.2f Mio.", sign_str, x)
  } else if (unit == "Tsd. CHF") {
    sprintf("%s%.0f Tsd.", sign_str, x)
  } else {
    paste0(sign_str, format(round(x), big.mark = "'"))
  }
}


# ==============================================================================
# Direkt ausführbar: Datei-Dialog wenn kein Argument angegeben
# ==============================================================================
if (sys.nframe() == 0) {
  # Script direkt ausgeführt (nicht via source()) → sofort starten
  cashflow_fan_chart()
}
