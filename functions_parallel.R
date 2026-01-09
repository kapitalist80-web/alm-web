# ============================================================================
# ALM-Analyse für Pensionskassen: Zentrale Funktionsdatei (PARALLELISIERT)
# ============================================================================
# Projekt: Diplomarbeit - Monte Carlo Simulation für ALM
# Autor: Michel Bossong
# Datum: 2025-01-15
# Beschreibung: Optimierte Version mit Parallelverarbeitung für große Datensätze
#              (1.5M+ Zeilen, 80+ Variablen)
# ============================================================================
# Abhängigkeiten: tidyverse, ggplot2, data.table, parallel, doParallel
# ============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(scales)
library(data.table)
library(parallel)
library(doParallel)

# ===== GLOBALE PARALLELISIERUNGS-EINSTELLUNGEN =====

#' Initialisiert Parallelverarbeitung
#' @param n_cores Anzahl Kerne (NULL = alle minus 1)
#' @return Anzahl verwendeter Kerne
init_parallel <- function(n_cores = NULL) {
  if (is.null(n_cores)) {
    n_cores <- max(1, detectCores() - 1)
  }
  
  # data.table Threading aktivieren
  setDTthreads(n_cores)
  
  # doParallel registrieren (für foreach)
  registerDoParallel(cores = n_cores)
  
  cat("Parallelverarbeitung aktiviert mit", n_cores, "Kernen\n")
  cat("- data.table threads:", getDTthreads(), "\n")
  cat("- doParallel workers:", getDoParWorkers(), "\n")
  
  return(n_cores)
}

#' Beendet Parallelverarbeitung
stop_parallel <- function() {
  stopImplicitCluster()
  setDTthreads(1)
  cat("Parallelverarbeitung beendet\n")
}

# ===== 1. DATENIMPORT (PARALLELISIERT) =====

#' Lädt alle CSV-Dateien parallel aus dem Datenordner
#' @param data_dir Pfad zum Datenordner mit CSV-Dateien
#' @param n_cores Anzahl Kerne für paralleles Laden (NULL = auto)
#' @param verbose Fortschritt anzeigen
#' @return Data frame mit allen Monte-Carlo Simulationspfaden
load_mc_data <- function(data_dir = "/home/michelbossong/alm-web/data/", 
                         n_cores = NULL,
                         verbose = TRUE) {
  
  library(data.table)
  library(parallel)
  
  # Alle CSV-Dateien im Ordner finden
  csv_files <- list.files(data_dir, pattern = "^mc.*\\.csv$", full.names = TRUE)
  
  if (length(csv_files) == 0) {
    stop("Keine CSV-Dateien im Ordner gefunden: ", data_dir)
  }
  
  if (verbose) cat("Gefunden:", length(csv_files), "CSV-Dateien\n")
  
  # Parallelisierung initialisieren
  if (is.null(n_cores)) {
    n_cores <- max(1, detectCores() - 1)
  }
  
  if (verbose) cat("Lade mit", n_cores, "Kernen parallel...\n")
  
  start_time <- Sys.time()
  
  # Cluster erstellen
  cl <- makeCluster(n_cores)
  
  # Libraries auf allen Workern laden
  clusterEvalQ(cl, {
    library(data.table)
  })
  
  # Parallel laden mit Fortschrittsanzeige
  if (verbose) {
    df_list <- pbapply::pblapply(csv_files, function(file) {
      dt <- fread(file, sep = ";", dec = ".", stringsAsFactors = FALSE, 
                  showProgress = FALSE)
      dt$source_file <- basename(file)
      return(dt)
    }, cl = cl)
  } else {
    df_list <- parLapply(cl, csv_files, function(file) {
      dt <- fread(file, sep = ";", dec = ".", stringsAsFactors = FALSE,
                  showProgress = FALSE)
      dt$source_file <- basename(file)
      return(dt)
    })
  }
  
  stopCluster(cl)
  
  if (verbose) {
    load_time <- difftime(Sys.time(), start_time, units = "secs")
    cat(sprintf("Laden abgeschlossen in %.1f Sekunden\n", as.numeric(load_time)))
  }
  
  # Kombinieren mit data.table (schnell)
  if (verbose) cat("Kombiniere Dateien...\n")
  dt <- rbindlist(df_list, fill = TRUE)
  rm(df_list)  # Speicher freigeben
  gc()
  
  # Eindeutige path_nr über alle Files
  if (verbose) cat("Erstelle eindeutige Path-IDs...\n")
  dt[, path_id := paste(source_file, path_nr, sep = "_")]
  dt[, unique_path_nr := .GRP, by = path_id]
  
  # Original path_nr behalten
  setnames(dt, "path_nr", "path_nr_original")
  setnames(dt, "unique_path_nr", "path_nr")
  dt[, path_id := NULL]
  
  # Numerische Spalten konvertieren (parallelisiert mit data.table)
  numeric_cols <- c(
    "deckungsgrad", "V_t", "W_t", "portfolio_return",
    "gov_bonds_return", "corp_bonds_return", "equities_return",
    "realestate_return", "alternatives_return",
    "r1_t", "i_gov_bonds_t", "i_corp_bonds_t", "i_tech_t",
    "cashflow_rent", "cashflow_admin_fee", "cashflow_total",
    "liability_duration", "num_pensioners", "num_widows",
    "weight_gov_bonds", "weight_corp_bonds", "weight_equities",
    "weight_real_estate", "weight_alternatives",
    "initial_gov_bond_duration", "initial_corp_bond_duration",
    "bestand_n_total", "bestand_avg_age", "bestand_share_f",
    "bestand_share_married", "bestand_total_initial_pension",
    "bestand_pension_median", "bestand_pension_mean", "bestand_pension_std",
    "gov_bonds_duration_effect", "corp_bonds_duration_effect"
  )
  
  if (verbose) cat("Konvertiere numerische Spalten...\n")
  for (col in numeric_cols) {
    if (col %in% names(dt)) {
      dt[, (col) := as.numeric(get(col))]
    }
  }
  
  dt[, year := as.integer(year)]
  
  if (verbose) {
    total_time <- difftime(Sys.time(), start_time, units = "secs")
    cat("\n=== ZUSAMMENFASSUNG ===\n")
    cat(sprintf("Geladene Zeilen:        %s\n", format(nrow(dt), big.mark = "'")))
    cat(sprintf("Anzahl Variablen:       %d\n", ncol(dt)))
    cat(sprintf("Anzahl CSV-Dateien:     %d\n", length(csv_files)))
    cat(sprintf("Eindeutige Pfade:       %s\n", format(uniqueN(dt$path_nr), big.mark = "'")))
    cat(sprintf("Speichergröße:          %.1f MB\n", as.numeric(object.size(dt)) / 1024^2))
    cat(sprintf("Gesamtzeit:             %.1f Sekunden\n", as.numeric(total_time)))
    cat(sprintf("Durchsatz:              %.0f Zeilen/Sek\n", nrow(dt) / as.numeric(total_time)))
  }
  
  return(as.data.frame(dt))
}


# ===== 2. RISIKO- UND STRESS-ANALYSE (OPTIMIERT) =====

#' Analysiert Default-Wahrscheinlichkeit bei Zinsschock (data.table optimiert)
#' @param df DataFrame mit Simulationsdaten
#' @param shock_threshold Zinsschwelle (z.B. 0.01 für 1%)
#' @param shock_direction "above" oder "below"
#' @param shock_year Jahr für den Zinsschock
#' @param default_threshold Schwellenwert für Default
#' @return Liste mit Analyseergebnissen und Plots
analyze_default_by_interest_shock <- function(df, 
                                              shock_threshold = 0.01,
                                              shock_direction = "above",
                                              shock_year = 1,
                                              default_threshold = 0.8) {
  
  # Als data.table für Performance
  dt <- as.data.table(df)
  
  # Zinsschock-Pfade identifizieren (vectorized)
  shock_paths <- dt[year == shock_year, .(
    path_nr,
    r1_t_year1 = r1_t,
    is_shock = if (shock_direction == "above") r1_t >= shock_threshold else r1_t <= shock_threshold
  )]
  
  # Default-Status pro Pfad (parallelisiert durch data.table)
  default_status <- dt[, .(
    min_deckungsgrad = min(deckungsgrad, na.rm = TRUE),
    is_default = any(deckungsgrad < default_threshold),
    default_year = fifelse(any(deckungsgrad < default_threshold),
                          min(year[deckungsgrad < default_threshold]), 
                          NA_real_)
  ), by = path_nr]
  
  # Kombinieren
  analysis_data <- shock_paths[default_status, on = "path_nr"]
  analysis_data[, shock_category := fifelse(
    is_shock,
    paste0("Zinsschock (r1_t ", 
           ifelse(shock_direction == "above", "≥", "≤"), 
           " ", sprintf("%.1f%%", shock_threshold * 100), ")"),
    "Kein Schock"
  )]
  
  # Statistiken berechnen
  summary_stats <- analysis_data[, .(
    n_paths = .N,
    n_defaults = sum(is_default),
    default_rate = mean(is_default) * 100,
    mean_r1t = mean(r1_t_year1),
    median_r1t = median(r1_t_year1)
  ), by = shock_category]
  
  # Gesamtstatistik
  total_paths <- nrow(analysis_data)
  total_defaults <- sum(analysis_data$is_default)
  total_default_rate <- total_defaults / total_paths * 100
  
  shock_data <- analysis_data[is_shock == TRUE]
  no_shock_data <- analysis_data[is_shock == FALSE]
  
  shock_default_rate <- mean(shock_data$is_default) * 100
  no_shock_default_rate <- mean(no_shock_data$is_default) * 100
  
  # Relatives Risiko
  relative_risk <- shock_default_rate / no_shock_default_rate
  absolute_diff <- shock_default_rate - no_shock_default_rate
  
  # Output
  cat("=== Analyse: Default bei Zinsschock ===\n")
  cat(sprintf("Zinsschock-Definition: r1_t %s %.2f%% in Jahr %d\n", 
              ifelse(shock_direction == "above", "≥", "≤"),
              shock_threshold * 100, shock_year))
  cat(sprintf("Default-Definition: Deckungsgrad < %.0f%%\n\n", default_threshold * 100))
  
  cat("--- Übersicht ---\n")
  print(summary_stats)
  
  cat("\n--- Risikoanalyse ---\n")
  cat(sprintf("Gesamt-Default-Rate:        %6.2f%% (%d/%d)\n", 
              total_default_rate, total_defaults, total_paths))
  cat(sprintf("Default-Rate bei Schock:    %6.2f%% (%d/%d)\n", 
              shock_default_rate, sum(shock_data$is_default), nrow(shock_data)))
  cat(sprintf("Default-Rate ohne Schock:   %6.2f%% (%d/%d)\n", 
              no_shock_default_rate, sum(no_shock_data$is_default), nrow(no_shock_data)))
  cat(sprintf("\nRelatives Risiko (RR):      %6.2fx\n", relative_risk))
  cat(sprintf("Absolute Differenz:         %+6.2f Prozentpunkte\n", absolute_diff))
  
  # Chi-Quadrat Test
  contingency_table <- table(analysis_data$is_shock, analysis_data$is_default)
  if (all(dim(contingency_table) == c(2, 2))) {
    chi_test <- chisq.test(contingency_table)
    cat(sprintf("\nChi²-Test: χ² = %.2f, p = %.6f %s\n", 
                chi_test$statistic, chi_test$p.value,
                ifelse(chi_test$p.value < 0.001, "***", 
                       ifelse(chi_test$p.value < 0.01, "**",
                              ifelse(chi_test$p.value < 0.05, "*", "n.s.")))))
  }
  
  # Plot erstellen (ggplot2 nutzt intern data.frame)
  plot_data <- as.data.frame(analysis_data)
  
  p1 <- ggplot(plot_data, aes(x = shock_category, fill = is_default)) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "firebrick"),
                      labels = c("Kein Default", "Default"),
                      name = "") +
    labs(title = "Default-Rate nach Zinsschock",
         subtitle = sprintf("Schock: r1_t %s %.2f%% in Jahr %d", 
                           ifelse(shock_direction == "above", "≥", "≤"),
                           shock_threshold * 100, shock_year),
         x = NULL, y = "Anteil") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p1)
  
  return(list(
    summary = as.data.frame(summary_stats),
    analysis_data = as.data.frame(analysis_data),
    statistics = list(
      total_default_rate = total_default_rate,
      shock_default_rate = shock_default_rate,
      no_shock_default_rate = no_shock_default_rate,
      relative_risk = relative_risk,
      absolute_diff = absolute_diff
    ),
    plot = p1
  ))
}


# ===== 3. PORTFOLIO-VOLATILITÄTS-ANALYSE (OPTIMIERT) =====

#' Analysiert Portfolio-Return-Volatilität (data.table optimiert)
#' @param df Dataframe mit MC-Simulationen
#' @param years_to_analyze Welche Jahre einbeziehen
#' @param duration_matching Duration-Effekte neutralisieren
#' @return Liste mit Statistiken und Plots
analyze_return_volatility <- function(df, 
                                      years_to_analyze = 1:20,
                                      duration_matching = TRUE) {
  
  # Als data.table für Performance
  dt <- as.data.table(df)
  
  # Portfolio-Return berechnen
  if (duration_matching) {
    dt[, portfolio_return_adj := portfolio_return - 
         gov_bonds_duration_effect - corp_bonds_duration_effect]
    return_col <- "portfolio_return_adj"
    title_suffix <- " (Duration-Matched)"
    cat("Duration-Matching aktiv: Duration-Effekte abgezogen\n")
  } else {
    dt[, portfolio_return_adj := portfolio_return]
    return_col <- "portfolio_return_adj"
    title_suffix <- ""
  }
  
  # Strategie-Identifier erstellen
  dt[, alloc_label := sprintf("Gov%.0f/Corp%.0f/Eq%.0f/RE%.0f/Alt%.0f",
                              weight_gov_bonds * 100,
                              weight_corp_bonds * 100,
                              weight_equities * 100,
                              weight_real_estate * 100,
                              weight_alternatives * 100)]
  
  dt[, duration_label := sprintf("Gov:%s(%.0f)/Corp:%s(%.0f)",
                                gov_bond_duration_mode,
                                initial_gov_bond_duration,
                                corp_bond_duration_mode,
                                initial_corp_bond_duration)]
  
  dt[, strategy := paste(alloc_label, duration_label, sep = " | ")]
  
  # Volatilität pro Strategie (parallelisiert durch data.table)
  stats <- dt[year %in% years_to_analyze, .(
    n_paths = uniqueN(path_nr),
    mean_return = mean(portfolio_return_adj, na.rm = TRUE),
    sd_return = sd(portfolio_return_adj, na.rm = TRUE),
    mean_return_raw = mean(portfolio_return, na.rm = TRUE),
    sd_return_raw = sd(portfolio_return, na.rm = TRUE),
    mean_duration_effect = mean(gov_bonds_duration_effect + corp_bonds_duration_effect, na.rm = TRUE),
    sd_duration_effect = sd(gov_bonds_duration_effect + corp_bonds_duration_effect, na.rm = TRUE),
    var_95 = quantile(portfolio_return_adj, 0.05, na.rm = TRUE),
    sharpe = mean(portfolio_return_adj, na.rm = TRUE) / sd(portfolio_return_adj, na.rm = TRUE)
  ), by = .(strategy, alloc_label, duration_label,
            weight_gov_bonds, weight_corp_bonds, weight_equities,
            weight_real_estate, weight_alternatives,
            initial_gov_bond_duration, gov_bond_duration_mode,
            initial_corp_bond_duration, corp_bond_duration_mode)]
  
  # CVaR separat berechnen (komplexere Aggregation)
  cvar_data <- dt[year %in% years_to_analyze, {
    q05 <- quantile(portfolio_return_adj, 0.05, na.rm = TRUE)
    .(cvar_95 = mean(portfolio_return_adj[portfolio_return_adj <= q05], na.rm = TRUE))
  }, by = strategy]
  
  stats <- stats[cvar_data, on = "strategy"]
  setorder(stats, sd_return)
  
  # Konvertiere zu data.frame für ggplot2
  stats_df <- as.data.frame(stats)
  
  # --- PLOTS ---
  
  # Plot 1: Efficient Frontier
  p1 <- ggplot(stats_df, aes(x = sd_return, y = mean_return)) +
    geom_point(aes(color = weight_equities, size = weight_gov_bonds), alpha = 0.7) +
    geom_text(aes(label = alloc_label), size = 2.5, vjust = -1, check_overlap = TRUE) +
    scale_color_viridis_c(name = "Equity", labels = percent) +
    scale_size_continuous(name = "Gov Bonds", range = c(2, 8)) +
    scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(title = paste0("Return vs. Volatilität", title_suffix), 
         x = "Volatilität (Std.Dev.)", y = "Mean Return") +
    theme_minimal()
  
  # Plot 2: Volatilität-Ranking
  p2 <- stats_df %>%
    head(20) %>%
    mutate(alloc_label = reorder(alloc_label, sd_return)) %>%
    ggplot(aes(x = alloc_label, y = sd_return, fill = sharpe)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", sd_return * 100)), hjust = -0.1, size = 3) +
    scale_fill_viridis_c(name = "Sharpe", option = "plasma") +
    scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
    coord_flip() +
    labs(title = paste0("Tiefste Volatilität (Top 20)", title_suffix), 
         x = NULL, y = "Volatilität") +
    theme_minimal()
  
  # Plot 3: Duration-Effekt
  p3 <- ggplot(stats_df, aes(x = initial_gov_bond_duration, y = sd_return)) +
    geom_point(aes(color = alloc_label, shape = gov_bond_duration_mode), size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "gray40", alpha = 0.2) +
    scale_y_continuous(labels = percent) +
    labs(title = paste0("Duration-Effekt auf Volatilität", title_suffix),
         x = "Gov-Bond Duration (Jahre)", y = "Volatilität",
         color = "Allocation", shape = "Duration Mode") +
    theme_minimal() +
    theme(legend.position = "bottom", legend.box = "vertical")
  
  # Plot 4: Heatmap
  p4 <- stats_df %>%
    ggplot(aes(x = weight_equities, y = weight_gov_bonds, fill = sd_return)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.1f%%", sd_return * 100)), color = "white", size = 3) +
    scale_fill_viridis_c(name = "Volatilität", labels = percent, option = "magma", direction = -1) +
    scale_x_continuous(labels = percent) +
    scale_y_continuous(labels = percent) +
    labs(title = paste0("Volatilität: Equity vs. Gov-Bonds", title_suffix), 
         x = "Equity", y = "Gov Bonds") +
    theme_minimal()
  
  # Plot 5: Vergleich Raw vs Duration-Matched
  p5 <- NULL
  if (duration_matching) {
    p5 <- stats_df %>%
      select(alloc_label, sd_return, sd_return_raw) %>%
      tidyr::pivot_longer(cols = c(sd_return, sd_return_raw), 
                          names_to = "type", values_to = "volatility") %>%
      mutate(type = ifelse(type == "sd_return", "Duration-Matched", "Raw")) %>%
      ggplot(aes(x = reorder(alloc_label, volatility), y = volatility, fill = type)) +
      geom_col(position = "dodge", width = 0.7) +
      scale_fill_manual(values = c("Duration-Matched" = "steelblue", "Raw" = "coral")) +
      scale_y_continuous(labels = percent) +
      coord_flip() +
      labs(title = "Volatilität: Raw vs. Duration-Matched",
           subtitle = "Effekt der Duration-Neutralisierung",
           x = NULL, y = "Volatilität", fill = "") +
      theme_minimal() +
      theme(legend.position = "bottom")
  }
  
  # Ausgabe
  cat("\n=== VOLATILITÄTS-RANKING", title_suffix, "===\n")
  print(stats_df %>% 
          select(alloc_label, duration_label, sd_return, mean_return, sharpe) %>% 
          head(10))
  
  if (duration_matching) {
    cat("\n=== DURATION-EFFEKT STATISTIK ===\n")
    cat(sprintf("Mittlerer Duration-Effekt: %.2f%%\n", mean(stats_df$mean_duration_effect) * 100))
    cat(sprintf("Std.Dev. Duration-Effekt:  %.2f%%\n", mean(stats_df$sd_duration_effect) * 100))
    cat(sprintf("Volatilitäts-Reduktion:    %.2f%% (Durchschnitt)\n", 
                mean((stats_df$sd_return_raw - stats_df$sd_return) / stats_df$sd_return_raw) * 100))
  }
  
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  if (!is.null(p5)) print(p5)
  
  return(list(
    summary = stats_df, 
    plots = list(frontier = p1, ranking = p2, duration = p3, heatmap = p4, comparison = p5),
    duration_matching = duration_matching
  ))
}


# ===== 4. BATCH-ANALYSEN (PARALLELE AUSFÜHRUNG) =====

#' Führt mehrere Analysen parallel aus
#' @param df Dataframe mit MC-Simulationen
#' @param analyses Liste mit Analyse-Funktionen und Parametern
#' @return Liste mit allen Ergebnissen
run_parallel_analyses <- function(df, analyses) {
  #' @examples
  #' analyses <- list(
  #'   list(func = analyze_default_by_interest_shock, 
  #'        params = list(shock_threshold = 0.01, shock_direction = "above")),
  #'   list(func = analyze_default_by_interest_shock,
  #'        params = list(shock_threshold = 0.02, shock_direction = "above"))
  #' )
  #' results <- run_parallel_analyses(df, analyses)
  
  library(foreach)
  
  results <- foreach(i = 1:length(analyses), .packages = c("data.table", "ggplot2", "dplyr")) %dopar% {
    analysis <- analyses[[i]]
    func <- analysis$func
    params <- c(list(df = df), analysis$params)
    do.call(func, params)
  }
  
  names(results) <- paste0("analysis_", 1:length(analyses))
  return(results)
}


# ===== 5. HILFSFUNKTIONEN =====

#' Gibt Speichernutzung und Performance-Statistiken aus
#' @param df Dataframe
show_performance_stats <- function(df) {
  cat("\n=== PERFORMANCE STATISTIK ===\n")
  cat(sprintf("Zeilen:              %s\n", format(nrow(df), big.mark = "'")))
  cat(sprintf("Spalten:             %d\n", ncol(df)))
  cat(sprintf("Speicher (MB):       %.1f\n", as.numeric(object.size(df)) / 1024^2))
  cat(sprintf("data.table threads:  %d\n", getDTthreads()))
  cat(sprintf("doParallel workers:  %d\n", getDoParWorkers()))
  
  if (is.data.table(df)) {
    cat("Format:              data.table (optimiert)\n")
  } else {
    cat("Format:              data.frame\n")
    cat("Tipp:                Konvertiere zu data.table mit as.data.table() für bessere Performance\n")
  }
}

#' Benchmarkt eine Funktion
#' @param expr Ausdruck zum Benchmarken
#' @param times Anzahl Wiederholungen
benchmark_function <- function(expr, times = 3) {
  library(microbenchmark)
  result <- microbenchmark(expr, times = times, unit = "s")
  print(result)
  return(result)
}


# ============================================================================
# ENDE DER PARALLELISIERTEN FUNKTIONSDATEI
# ============================================================================
# Verwendungsbeispiel:
#
# # 1. Parallelverarbeitung initialisieren
# init_parallel(n_cores = 6)  # oder NULL für auto-detect
#
# # 2. Daten laden (parallelisiert)
# df <- load_mc_data("/home/michelbossong/alm-web/data/", verbose = TRUE)
#
# # 3. Analysen durchführen (nutzen automatisch data.table threading)
# results1 <- analyze_default_by_interest_shock(df, shock_threshold = 0.01)
# results2 <- analyze_return_volatility(df, duration_matching = TRUE)
#
# # 4. Oder mehrere Analysen parallel
# analyses <- list(
#   list(func = analyze_default_by_interest_shock, 
#        params = list(shock_threshold = 0.01)),
#   list(func = analyze_default_by_interest_shock,
#        params = list(shock_threshold = 0.02))
# )
# all_results <- run_parallel_analyses(df, analyses)
#
# # 5. Aufräumen
# stop_parallel()
# ============================================================================
