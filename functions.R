# ============================================================================
# ALM-Analyse für Pensionskassen: Zentrale Funktionsdatei
# ============================================================================
# Projekt: Diplomarbeit - Monte Carlo Simulation für ALM
# Autor: Michel Bossong
# Datum: 2025-01-15
# Beschreibung: Alle Analysefunktionen für Pfadanalyse, Risikoanalyse und
#              Visualisierung der Monte-Carlo-Simulationsergebnisse
# ============================================================================
# Abhängigkeiten: tidyverse, ggplot2, data.table, gridExtra, scales
# ============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(scales)
library(data.table)

# ===== 1. DATENIMPORT =====
# Laden aller Monte-Carlo Simulationsergebnisse aus dem data-Ordner

# Korrigierte load_mc_data Funktion
# - dec = "." statt ","
# - Eindeutige path_nr über alle Files hinweg

load_mc_data <- function(data_dir = "/home/michelbossong/alm-web/data/") {
  #' Lädt alle CSV-Dateien aus dem Datenordner
  #'
  #' @param data_dir Pfad zum Datenordner mit CSV-Dateien
  #' @return Data frame mit allen Monte-Carlo Simulationspfaden
  
  library(data.table)
  
  # Alle CSV-Dateien im Ordner finden
  csv_files <- list.files(data_dir, pattern = "^mc.*\\.csv$", full.names = TRUE)
  
  if (length(csv_files) == 0) {
    stop("Keine CSV-Dateien im Ordner gefunden: ", data_dir)
  }
  
  cat("Lade", length(csv_files), "Dateien...\n")
  
  # Alle Dateien laden und kombinieren
  df_list <- lapply(csv_files, function(file) {
    dt <- fread(file, sep = ";", dec = ".", stringsAsFactors = FALSE)
    dt$source_file <- basename(file)
    return(dt)
  })
  
  df <- rbindlist(df_list, fill = TRUE)
  
  # Eindeutige path_nr über alle Files: kombiniere source_file + original path_nr
  df[, path_id := paste(source_file, path_nr, sep = "_")]
  df[, unique_path_nr := as.integer(factor(path_id))]
  
  # Original path_nr behalten als path_nr_original, neue als path_nr
  setnames(df, "path_nr", "path_nr_original")
  setnames(df, "unique_path_nr", "path_nr")
  df[, path_id := NULL]
  
  # Sicherstellen dass numerische Spalten numerisch sind
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
    "bestand_pension_median", "bestand_pension_mean", "bestand_pension_std"
  )
  
  for (col in numeric_cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.numeric(df[[col]])
    }
  }
  
  df$year <- as.integer(df$year)
  
  cat("Geladen:", format(nrow(df), big.mark = "'"), "Zeilen,", 
      length(csv_files), "Dateien,",
      uniqueN(df$path_nr), "eindeutige Pfade\n")
  
  return(as.data.frame(df))
}



# ===== 2. RISIKO- UND STRESS-ANALYSE =====

#' Analysiert Default-Wahrscheinlichkeit bei Zinsschock
#'
#' @param df DataFrame mit Simulationsdaten
#' @param shock_threshold Zinsschwelle (z.B. 0.01 für 1%)
#' @param shock_direction "above" (Zins >= threshold) oder "below" (Zins <= threshold)
#' @param shock_year Jahr für den Zinsschock (Standard: 1)
#' @param default_threshold Schwellenwert für Default (Standard: 0.8 = 80%)
#' @return Liste mit Analyseergebnissen und Plots
#'
analyze_default_by_interest_shock <- function(df, 
                                              shock_threshold = 0.01,
                                              shock_direction = "above",
                                              shock_year = 1,
                                              default_threshold = 0.8) {
  
  # Zinsschock-Pfade identifizieren
  shock_paths <- df %>%
    filter(year == shock_year) %>%
    mutate(
      is_shock = if (shock_direction == "above") r1_t >= shock_threshold else r1_t <= shock_threshold
    ) %>%
    select(path_nr, r1_t_year1 = r1_t, is_shock)
  
  # Default-Status pro Pfad
  default_status <- df %>%
    group_by(path_nr) %>%
    summarise(
      min_deckungsgrad = min(deckungsgrad, na.rm = TRUE),
      is_default = any(deckungsgrad < default_threshold),
      default_year = ifelse(any(deckungsgrad < default_threshold),
                            min(year[deckungsgrad < default_threshold]), NA),
      .groups = "drop"
    )
  
  # Kombinieren
  analysis_data <- shock_paths %>%
    left_join(default_status, by = "path_nr") %>%
    mutate(
      shock_category = ifelse(is_shock, 
                              paste0("Zinsschock (r1_t ", 
                                     ifelse(shock_direction == "above", "≥", "≤"), 
                                     " ", percent(shock_threshold, accuracy = 0.1), ")"),
                              "Kein Schock")
    )
  
  # Statistiken berechnen
  summary_stats <- analysis_data %>%
    group_by(shock_category) %>%
    summarise(
      n_paths = n(),
      n_defaults = sum(is_default),
      default_rate = mean(is_default) * 100,
      mean_r1t = mean(r1_t_year1),
      median_r1t = median(r1_t_year1),
      .groups = "drop"
    )
  
  # Gesamtstatistik
  total_paths <- nrow(analysis_data)
  total_defaults <- sum(analysis_data$is_default)
  total_default_rate <- total_defaults / total_paths * 100
  
  shock_data <- analysis_data %>% filter(is_shock)
  no_shock_data <- analysis_data %>% filter(!is_shock)
  
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
  print(summary_stats, n = Inf)
  
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
                ifelse(chi_test$p.value < 0.05, "***", "")))
  }
  
  # Farbschema
  colors <- c("Kein Schock" = "#2E86AB", setNames("#E94F37", summary_stats$shock_category[summary_stats$shock_category != "Kein Schock"]))
  
  # Plot 1: Default-Rate Vergleich
  p_bar <- ggplot(summary_stats, aes(x = shock_category, y = default_rate, fill = shock_category)) +
    geom_col(alpha = 0.8, width = 0.6) +
    geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", default_rate, n_paths)), 
              vjust = -0.3, size = 4) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), 
                       expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "Default-Wahrscheinlichkeit bei Zinsschock",
      subtitle = sprintf("Relatives Risiko: %.2fx | Absolute Differenz: %+.1f PP", 
                         relative_risk, absolute_diff),
      x = "", y = "Default-Rate (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none", 
          axis.text.x = element_text(size = 10))
  
  # Plot 2: r1_t Verteilung nach Default-Status
  p_density <- ggplot(analysis_data, aes(x = r1_t_year1, fill = is_default)) +
    geom_density(alpha = 0.5) +
    geom_vline(xintercept = shock_threshold, linetype = "dashed", color = "red", linewidth = 1) +
    scale_fill_manual(values = c("FALSE" = "#2E86AB", "TRUE" = "#E94F37"),
                      labels = c("Kein Default", "Default")) +
    scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(
      title = sprintf("r1_t Verteilung (Jahr %d) nach Default-Status", shock_year),
      subtitle = sprintf("Rote Linie: Zinsschock-Schwelle (%.1f%%)", shock_threshold * 100),
      x = "r1_t", y = "Dichte", fill = ""
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  # Plot 3: Default-Rate über Zins-Quantile
  analysis_data <- analysis_data %>%
    mutate(r1t_quantile = cut(r1_t_year1, 
                              breaks = quantile(r1_t_year1, probs = seq(0, 1, 0.1)),
                              include.lowest = TRUE,
                              labels = paste0("Q", 1:10)))
  
  quantile_stats <- analysis_data %>%
    group_by(r1t_quantile) %>%
    summarise(
      default_rate = mean(is_default) * 100,
      mean_r1t = mean(r1_t_year1),
      n = n(),
      .groups = "drop"
    ) %>%
    filter(!is.na(r1t_quantile))
  
  p_quantile <- ggplot(quantile_stats, aes(x = r1t_quantile, y = default_rate)) +
    geom_col(fill = "#2E86AB", alpha = 0.8) +
    geom_text(aes(label = sprintf("%.1f%%", default_rate)), vjust = -0.3, size = 3) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "Default-Rate nach r1_t Dezilen (Jahr 1)",
      subtitle = "Q1 = tiefste Zinsen, Q10 = höchste Zinsen",
      x = "r1_t Dezil", y = "Default-Rate (%)"
    ) +
    theme_minimal(base_size = 12)
  
  list(
    summary = summary_stats,
    relative_risk = relative_risk,
    absolute_diff = absolute_diff,
    shock_default_rate = shock_default_rate,
    no_shock_default_rate = no_shock_default_rate,
    bar_plot = p_bar,
    density_plot = p_density,
    quantile_plot = p_quantile,
    data = analysis_data
  )
}

# ==============================================================================
# Aufruf
# ==============================================================================

# Beispiel: Zinsschock wenn r1_t >= 1%
# results <- analyze_default_by_interest_shock(df, shock_threshold = 0.01, shock_direction = "above")
# print(results$bar_plot)
# print(results$quantile_plot)

# Beispiel: Negativzins-Schock wenn r1_t <= -0.5%
# results <- analyze_default_by_interest_shock(df, shock_threshold = -0.005, shock_direction = "below")


#' Analysiert r1_t Verteilung: Default-Pfade vs. Alle Pfade
#'
#' @param df DataFrame mit Simulationsdaten (bereits geladen)
#' @param default_threshold Schwellenwert für Default (Standard: 0.8 = 80%)
#' @param years_to_analyze Anzahl Jahre für die Analyse (Standard: 3)
#' @return Liste mit Analyseergebnissen und Plots
#'
analyze_r1t_default_comparison <- function(df, 
                                           default_threshold = 0.8,
                                           years_to_analyze = 3) {
  
  cat("Anzahl Zeilen:", nrow(df), "\n")
  cat("Anzahl Pfade:", n_distinct(df$path_nr), "\n")
  cat("Jahre pro Pfad:", max(df$year), "\n\n")
  
  # Default-Pfade identifizieren
  default_paths <- df %>%
    group_by(path_nr) %>%
    summarise(
      min_deckungsgrad = min(deckungsgrad, na.rm = TRUE),
      is_default = any(deckungsgrad < default_threshold),
      .groups = "drop"
    )
  
  n_default <- sum(default_paths$is_default)
  n_total <- nrow(default_paths)
  default_rate <- n_default / n_total * 100
  
  cat(sprintf("Default-Pfade: %d von %d (%.2f%%)\n", n_default, n_total, default_rate))
  
  if (n_default == 0) {
    cat("\n⚠ Keine Default-Pfade gefunden!\n")
    return(NULL)
  }
  
  # Daten filtern und kategorisieren
  data_all <- df %>%
    filter(year <= years_to_analyze) %>%
    mutate(category = "Alle Pfade", year_label = paste0("Jahr ", year))
  
  data_default <- df %>%
    filter(year <= years_to_analyze) %>%
    inner_join(default_paths %>% filter(is_default) %>% select(path_nr), by = "path_nr") %>%
    mutate(category = "Default-Pfade", year_label = paste0("Jahr ", year))
  
  data_combined <- bind_rows(data_all, data_default)
  
  # Statistiken
  stats_summary <- data_combined %>%
    group_by(category, year) %>%
    summarise(
      n = n(),
      mean = mean(r1_t, na.rm = TRUE),
      median = median(r1_t, na.rm = TRUE),
      sd = sd(r1_t, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\n=== Statistiken r1_t ===\n")
  print(stats_summary, n = Inf)
  
  # Farbschema
  colors <- c("Alle Pfade" = "#2E86AB", "Default-Pfade" = "#E94F37")
  
  # Density-Plot
  p_density <- ggplot(data_combined, aes(x = r1_t, fill = category, color = category)) +
    geom_density(alpha = 0.3, linewidth = 1) +
    facet_wrap(~year_label, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(
      title = "Verteilung r1_t: Default-Pfade vs. Alle Pfade",
      subtitle = sprintf("Default-Rate: %.2f%% (%d von %d Pfaden)", default_rate, n_default, n_total),
      x = "r1_t", y = "Dichte", fill = "Kategorie", color = "Kategorie"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
  
  # Box-Plot
  p_boxplot <- ggplot(data_combined, aes(x = category, y = r1_t, fill = category)) +
    geom_boxplot(alpha = 0.7) +
    facet_wrap(~year_label, ncol = 3) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(title = "r1_t Box-Plot Vergleich", x = "", y = "r1_t") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none", axis.text.x = element_text(angle = 15, hjust = 1))
  
  # KS-Tests
  cat("\n=== Kolmogorov-Smirnov Tests ===\n")
  for (y in 1:years_to_analyze) {
    r1t_all <- data_all %>% filter(year == y) %>% pull(r1_t)
    r1t_def <- data_default %>% filter(year == y) %>% pull(r1_t)
    if (length(r1t_def) > 1) {
      ks <- ks.test(r1t_all, r1t_def)
      cat(sprintf("Jahr %d: D=%.4f, p=%.4f %s\n", y, ks$statistic, ks$p.value,
                  ifelse(ks$p.value < 0.05, "***", "")))
    }
  }
  
  list(
    stats = stats_summary,
    density = p_density,
    boxplot = p_boxplot,
    n_default = n_default,
    default_rate = default_rate
  )
}

# ==============================================================================
# Aufruf (df muss bereits geladen sein)
# ==============================================================================

# results <- analyze_r1t_default_comparison(df)
# print(results$density)
# print(results$boxplot)

# ===== 3. VISUALISIERUNGEN =====

plot_path_analysis <- function(data, path_number) {
    #' Erstellt Übersichts-Plot für einen einzelnen Pfad
    #'
    #' @param data Data frame mit Simulationspfaden
    #' @param path_number Pfad-Nummer zur Visualisierung
    #' @return ggplot Objekt
    
    path_data <- data %>% filter(path_nr == path_number)
    
    if (nrow(path_data) == 0) {
        stop("Pfad ", path_number, " nicht gefunden")
    }
    
    ggplot(path_data, aes(x = year)) +
        geom_line(aes(y = deckungsgrad, color = "Deckungsgrad"), linewidth = 1) +
        geom_point(aes(y = deckungsgrad, color = "Deckungsgrad"), size = 2) +
        geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", 
                   linewidth = 0.8, label = "Minimum (80%)") +
        geom_hline(yintercept = 1.0, linetype = "dotted", color = "gray50", 
                   linewidth = 0.6, label = "Sollwert (100%)") +
        scale_y_continuous(labels = percent_format(accuracy = 1)) +
        scale_x_continuous(breaks = seq(0, 40, 5)) +
        scale_color_manual(values = c("Deckungsgrad" = "#2E86AB")) +
        labs(
            title = paste("Pfadanalyse - Pfad", path_number),
            x = "Jahr", y = "Deckungsgrad",
            color = ""
        ) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "bottom")
}


plot_boxplot_difference <- function(data, year_num, show_points = TRUE, title = NULL) {
    #' Erstellt Boxplot für Jahresvergleich mit optionalen Datenpunkten
    #'
    #' @param data Data frame mit Simulationspfaden
    #' @param year_num Jahr für Analyse
    #' @param show_points TRUE/FALSE für Datenpunkte
    #' @param title Optionaler Titel
    #' @return ggplot Objekt
    
    year_data <- data %>%
        filter(year == year_num) %>%
        mutate(year = as.factor(year))
    
    p <- ggplot(year_data, aes(x = year, y = deckungsgrad)) +
        geom_boxplot(fill = "#2E86AB", alpha = 0.7, outlier.shape = NA) +
        geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", 
                   linewidth = 0.8) +
        geom_hline(yintercept = 1.0, linetype = "dotted", color = "gray50", 
                   linewidth = 0.6)
    
    if (show_points) {
        p <- p + geom_jitter(width = 0.1, alpha = 0.3, size = 1)
    }
    
    p <- p +
        scale_y_continuous(labels = percent_format(accuracy = 1)) +
        labs(
            title = if (is.null(title)) paste("Deckungsgrad Jahr", year_num) else title,
            x = "", y = "Deckungsgrad"
        ) +
        theme_minimal(base_size = 12)
    
    return(p)
}


plot_path_analysis_detailed <- function(data, path_number, total_paths) {
    #' Erstellt detaillierte Mehrfach-Plot-Analyse für einen Pfad
    #' mit Vergleich zu statistischen Benchmarks
    #'
    #' @param data Data frame mit Simulationspfaden
    #' @param path_number Pfad-Nummer zur detaillierten Visualisierung
    #' @param total_paths Gesamtzahl der Pfade (für Statistikberechnung)
    #' @return ggplot Objekt mit kombiniertem Layout
    
    # Filtere einzelnen Pfad und berechne Statistiken
    path_data <- data %>% filter(path_nr == path_number)
    
    if (nrow(path_data) == 0) {
        stop("Pfad ", path_number, " nicht gefunden")
    }
    
    # Berechne Statistiken über alle Pfade pro Jahr
    df_unique <- data[!duplicated(data[, c("path_nr", "year")]), ]
    
    # Statistiken für Witwen
    df_stats_widows <- df_unique[, .(
        median_widows = median(num_widows, na.rm = TRUE),
        p5_widows = quantile(num_widows, 0.05, na.rm = TRUE),
        p95_widows = quantile(num_widows, 0.95, na.rm = TRUE)
    ), by = year]
    
    # Statistiken für V_t
    df_stats_vt <- df_unique[, .(
        median_vt = median(V_t, na.rm = TRUE),
        p5_vt = quantile(V_t, 0.05, na.rm = TRUE),
        p95_vt = quantile(V_t, 0.95, na.rm = TRUE)
    ), by = year]
    
    # Statistiken für W_t
    df_stats_wt <- df_unique[, .(
        median_wt = median(W_t, na.rm = TRUE),
        p5_wt = quantile(W_t, 0.05, na.rm = TRUE),
        p95_wt = quantile(W_t, 0.95, na.rm = TRUE)
    ), by = year]
    
    # Statistiken für Deckungsgrad
    df_stats_coverage <- df_unique[, .(
        median_coverage = median(deckungsgrad, na.rm = TRUE),
        p5_coverage = quantile(deckungsgrad, 0.05, na.rm = TRUE),
        p95_coverage = quantile(deckungsgrad, 0.95, na.rm = TRUE)
    ), by = year]
    
    # Statistiken für num_pensioners
    df_stats_pensioners <- df_unique[, .(
        median_pensioners = median(num_pensioners, na.rm = TRUE),
        p5_pensioners = quantile(num_pensioners, 0.05, na.rm = TRUE),
        p95_pensioners = quantile(num_pensioners, 0.95, na.rm = TRUE)
    ), by = year]
    
    # Zusammenfassen
    df_path <- path_data
    df_path <- merge(df_path, df_stats_widows, by = "year")
    df_path <- merge(df_path, df_stats_vt, by = "year")
    df_path <- merge(df_path, df_stats_wt, by = "year")
    df_path <- merge(df_path, df_stats_coverage, by = "year")
    df_path <- merge(df_path, df_stats_pensioners, by = "year")
    
    # Debug: Zeige wie viele Pfade verwendet werden
    n_paths_filtered <- df_unique[, uniqueN(path_nr)]
    
    # ===== PLOT 1: Witwen, Portfolio Return und Zinssatz =====
    scale_factor_return <- max(df_path$num_widows, na.rm = TRUE) / 
        max(abs(df_path$portfolio_return), na.rm = TRUE)
    scale_factor_rate <- max(df_path$num_widows, na.rm = TRUE) / 
        max(abs(df_path$r1_t), na.rm = TRUE)
    
    plot1 <- ggplot(df_path, aes(x = year)) +
        geom_ribbon(aes(ymin = p5_widows, ymax = p95_widows, fill = "P5-P95 Witwen"),
                    alpha = 0.2, color = NA) +
        geom_line(aes(y = median_widows, color = "Median Witwen"),
                  linewidth = 0.8, linetype = "dashed") +
        geom_line(aes(y = num_widows, color = "Pfad Witwen"),
                  linewidth = 1) +
        geom_point(aes(y = num_widows, color = "Pfad Witwen"), size = 2) +
        geom_col(aes(y = portfolio_return * scale_factor_return,
                     fill = "Portfolio Return"), alpha = 0.5, width = 0.7) +
        geom_col(aes(y = r1_t * scale_factor_rate,
                     fill = "Zinssatz r1_t"), alpha = 0.5, width = 0.7) +
        scale_y_continuous(
            name = "Anzahl Witwen",
            sec.axis = sec_axis(~ . / scale_factor_return, name = "Portfolio Return / Zinssatz")
        ) +
        scale_x_continuous(name = "Jahr", breaks = seq(0, 40, 5)) +
        scale_color_manual(values = c("Pfad Witwen" = "blue", "Median Witwen" = "darkblue")) +
        scale_fill_manual(values = c("Portfolio Return" = "orange", "Zinssatz r1_t" = "lightgreen",
                                     "P5-P95 Witwen" = "blue")) +
        labs(title = paste("Analyse Pfad", path_number, "- Witwen & Returns")) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 12),
              legend.position = "bottom", legend.title = element_blank(),
              axis.title.y.right = element_text(color = "darkred"))
    
    # ===== PLOT 2: V_t, W_t (linke Achse) und Deckungsgrad (rechte Achse, 0-2) =====
    max_vw <- max(c(df_path$p95_vt, df_path$p95_wt), na.rm = TRUE)
    scale_factor_coverage <- max_vw / 2
    
    plot2 <- ggplot(df_path, aes(x = year)) +
        geom_ribbon(aes(ymin = p5_vt / scale_factor_coverage, 
                        ymax = p95_vt / scale_factor_coverage, 
                        fill = "P5-P95 V_t"),
                    alpha = 0.15, color = NA) +
        geom_line(aes(y = median_vt / scale_factor_coverage, color = "Median V_t"),
                  linewidth = 0.8, linetype = "dashed") +
        geom_line(aes(y = V_t / scale_factor_coverage, color = "Pfad V_t"), linewidth = 1) +
        geom_point(aes(y = V_t / scale_factor_coverage, color = "Pfad V_t"), size = 2) +
        
        geom_ribbon(aes(ymin = p5_wt / scale_factor_coverage, 
                        ymax = p95_wt / scale_factor_coverage, 
                        fill = "P5-P95 W_t"),
                    alpha = 0.15, color = NA) +
        geom_line(aes(y = median_wt / scale_factor_coverage, color = "Median W_t"),
                  linewidth = 0.8, linetype = "dashed") +
        geom_line(aes(y = W_t / scale_factor_coverage, color = "Pfad W_t"), linewidth = 1) +
        geom_point(aes(y = W_t / scale_factor_coverage, color = "Pfad W_t"), size = 2) +
        
        geom_ribbon(aes(ymin = p5_coverage, ymax = p95_coverage, 
                        fill = "P5-P95 Deckungsgrad"),
                    alpha = 0.15, color = NA) +
        geom_line(aes(y = median_coverage, color = "Median Deckungsgrad"),
                  linewidth = 0.8, linetype = "dashed") +
        geom_line(aes(y = deckungsgrad, color = "Pfad Deckungsgrad"),
                  linewidth = 1) +
        geom_point(aes(y = deckungsgrad, color = "Pfad Deckungsgrad"), size = 2) +
        
        geom_hline(yintercept = 1, linetype = "dotted", 
                   color = "gray50", linewidth = 0.5) +
        
        scale_y_continuous(
            name = "V_t (Vermögen) & W_t (Verbindlichkeit) / 1e6",
            limits = c(0, 2)
        ) +
        scale_x_continuous(name = "Jahr", breaks = seq(0, 40, 5)) +
        scale_color_manual(values = c("Pfad V_t" = "darkgreen", "Median V_t" = "green",
                                      "Pfad W_t" = "darkred", "Median W_t" = "red",
                                      "Pfad Deckungsgrad" = "purple", "Median Deckungsgrad" = "mediumpurple")) +
        scale_fill_manual(values = c("P5-P95 V_t" = "green", "P5-P95 W_t" = "red",
                                     "P5-P95 Deckungsgrad" = "purple")) +
        labs(title = "Vermögen (V_t), Verbindlichkeit (W_t) und Deckungsgrad (0-2)") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 12),
              legend.position = "bottom", legend.title = element_blank())
    
    # ===== PLOT 3: Anzahl Rentner =====
    plot3 <- ggplot(df_path, aes(x = year)) +
        geom_ribbon(aes(ymin = p5_pensioners, ymax = p95_pensioners, fill = "P5-P95 Rentner"),
                    alpha = 0.2, color = NA) +
        geom_line(aes(y = median_pensioners, color = "Median Rentner"),
                  linewidth = 0.8, linetype = "dashed") +
        geom_line(aes(y = num_pensioners, color = "Pfad Rentner"), linewidth = 1) +
        geom_point(aes(y = num_pensioners, color = "Pfad Rentner"), size = 2) +
        geom_area(aes(y = num_pensioners, fill = "Pfad Rentner"), alpha = 0.15) +
        scale_y_continuous(name = "Anzahl Rentner") +
        scale_x_continuous(name = "Jahr", breaks = seq(0, 40, 5)) +
        scale_color_manual(values = c("Pfad Rentner" = "purple", "Median Rentner" = "mediumpurple")) +
        scale_fill_manual(values = c("P5-P95 Rentner" = "purple", "Pfad Rentner" = "purple")) +
        labs(title = "Entwicklung Anzahl Rentner") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 12),
              legend.position = "bottom", legend.title = element_blank())
    
    # ===== PARAMETER-TABELLE =====
    params <- df_path[1, .(
        "Gov Bonds Weight" = weight_gov_bonds,
        "Gov Bond Duration" = initial_gov_bond_duration,
        "Equities Weight" = weight_equities,
        "Reserve Rate" = general_reserve_rate,
        "Bestand Total" = bestand_n_total,
        "Share Married" = bestand_share_married
    )]
    
    param_table <- gridExtra::tableGrob(
        data.frame(
            Parameter = c("Gov Bonds Weight", "Gov Bond Duration", "Equities Weight",
                          "Reserve Rate", "Bestand Total", "Share Married", 
                          "Pfade in Statistik"),
            Value = c(
                round(params$`Gov Bonds Weight`, 4),
                round(params$`Gov Bond Duration`, 1),
                round(params$`Equities Weight`, 4),
                round(params$`Reserve Rate`, 4),
                format(params$`Bestand Total`, big.mark=","),
                round(params$`Share Married`, 4),
                paste(n_paths_filtered, "/", total_paths)
            )
        ),
        rows = NULL,
        theme = gridExtra::ttheme_minimal(base_size = 9, padding = unit(c(2, 2), "mm"))
    )
    
    # ===== KOMBINIERE ALLE PLOTS =====
    final_plot <- gridExtra::grid.arrange(
        plot1,
        plot2,
        plot3,
        param_table,
        nrow = 4,
        heights = c(1.5, 1.5, 1.5, 0.8),
        top = grid::textGrob(paste("Detaillierte Analyse Pfad", path_number),
                             gp = grid::gpar(fontsize = 16, fontface = "bold"))
    )
    
    return(final_plot)
}

# ==============================================================================
# Analyse: Cashflow-Streuung in Abhängigkeit der Bestandseigenschaften
# ==============================================================================

library(scales)

analyze_cashflow_dispersion <- function(df, years_to_analyze = 1:40) {
  #' @param df Bereits geladener Dataframe mit allen MC-Simulationen
  #' @param years_to_analyze Welche Jahre einbeziehen (default: 1-40)

  df <- as.data.frame(df)
  years_to_analyze <- as.integer(years_to_analyze)

  # Bestand berechnen
  df <- df %>%
    filter(year %in% years_to_analyze) %>%
    mutate(bestand = num_pensioners + num_widows)
  
  # Bestandseigenschaften extrahieren (sind pro Simulation konstant)
  bestand_properties <- df %>%
    distinct(source_file, bestand_n_total, bestand_avg_age, bestand_share_f,
             bestand_share_married, bestand_total_initial_pension,
             bestand_pension_median, bestand_pension_mean, bestand_pension_std) %>%
    mutate(
      # Abgeleitete Kennzahlen
      pension_cv = bestand_pension_std / bestand_pension_mean,  # Heterogenität
      avg_pension = bestand_total_initial_pension / bestand_n_total
    )
  
  # Falls source_file nicht existiert, nach Bestandseigenschaften gruppieren
  if (!"source_file" %in% names(df)) {
    df <- df %>%
      mutate(source_file = paste(bestand_n_total, bestand_avg_age, bestand_share_f, sep = "_"))
  }
  
  # Statistiken pro Bestandskonfiguration und Jahr
  stats_by_config_year <- df %>%
    group_by(source_file, year, bestand_n_total, bestand_avg_age, bestand_share_f,
             bestand_share_married, bestand_pension_mean, bestand_pension_std) %>%
    summarise(
      n_paths = n_distinct(path_nr),
      mean_bestand = mean(bestand, na.rm = TRUE),
      mean_cashflow = mean(cashflow_rent, na.rm = TRUE),
      sd_cashflow = sd(cashflow_rent, na.rm = TRUE),
      cv_cashflow = sd_cashflow / abs(mean_cashflow),
      .groups = "drop"
    ) %>%
    mutate(pension_cv = bestand_pension_std / bestand_pension_mean)
  
  # Aggregiert pro Bestandskonfiguration (über alle Jahre)
  stats_by_config <- df %>%
    group_by(source_file, bestand_n_total, bestand_avg_age, bestand_share_f,
             bestand_share_married, bestand_pension_mean, bestand_pension_std) %>%
    summarise(
      n_paths = n_distinct(path_nr),
      mean_cashflow = mean(cashflow_rent, na.rm = TRUE),
      sd_cashflow = sd(cashflow_rent, na.rm = TRUE),
      cv_cashflow = sd_cashflow / abs(mean_cashflow),
      .groups = "drop"
    ) %>%
    mutate(pension_cv = bestand_pension_std / bestand_pension_mean)
  
  # ===========================================================================
  # PLOTS
  # ===========================================================================
  
  plots <- list()
  
  # --- PLOT 1: CV nach Bestandsgrösse ---
  plots$cv_by_size <- ggplot(stats_by_config_year, 
                             aes(x = bestand_n_total, y = cv_cashflow)) +
    geom_point(aes(color = factor(year)), alpha = 0.5, size = 1) +
    geom_smooth(method = "loess", color = "darkred", se = TRUE, alpha = 0.2) +
    scale_y_continuous(labels = percent) +
    scale_color_viridis_d(name = "Jahr", option = "turbo") +
    labs(title = "Cashflow-Variabilität nach Bestandsgrösse",
         subtitle = "Grössere Bestände = stabilere Cashflows?",
         x = "Bestandsgrösse (n total)", y = "CV (Std.Dev. / Mean)") +
    theme_minimal()
  
  # --- PLOT 2: CV nach Durchschnittsalter ---
  plots$cv_by_age <- ggplot(stats_by_config_year,
                            aes(x = bestand_avg_age, y = cv_cashflow)) +
    geom_point(aes(color = factor(year)), alpha = 0.5, size = 1) +
    geom_smooth(method = "loess", color = "darkred", se = TRUE, alpha = 0.2) +
    scale_y_continuous(labels = percent) +
    scale_color_viridis_d(name = "Jahr", option = "turbo") +
    labs(title = "Cashflow-Variabilität nach Durchschnittsalter",
         subtitle = "Ältere Bestände = höhere Sterblichkeitsvolatilität?",
         x = "Durchschnittsalter", y = "CV") +
    theme_minimal()
  
  # --- PLOT 3: CV nach Frauenanteil ---
  plots$cv_by_gender <- ggplot(stats_by_config_year,
                               aes(x = bestand_share_f, y = cv_cashflow)) +
    geom_point(aes(color = factor(year)), alpha = 0.5, size = 1) +
    geom_smooth(method = "loess", color = "darkred", se = TRUE, alpha = 0.2) +
    scale_x_continuous(labels = percent) +
    scale_y_continuous(labels = percent) +
    scale_color_viridis_d(name = "Jahr", option = "turbo") +
    labs(title = "Cashflow-Variabilität nach Frauenanteil",
         subtitle = "Höhere Lebenserwartung Frauen = längere Zahlungsströme",
         x = "Frauenanteil", y = "CV") +
    theme_minimal()
  
  # --- PLOT 4: CV nach Verheiratetenanteil ---
  plots$cv_by_married <- ggplot(stats_by_config_year,
                                aes(x = bestand_share_married, y = cv_cashflow)) +
    geom_point(aes(color = factor(year)), alpha = 0.5, size = 1) +
    geom_smooth(method = "loess", color = "darkred", se = TRUE, alpha = 0.2) +
    scale_x_continuous(labels = percent) +
    scale_y_continuous(labels = percent) +
    scale_color_viridis_d(name = "Jahr", option = "turbo") +
    labs(title = "Cashflow-Variabilität nach Verheiratetenanteil",
         subtitle = "Mehr Verheiratete = mehr Witwen-Cashflows",
         x = "Verheiratetenanteil", y = "CV") +
    theme_minimal()
  
  # --- PLOT 5: CV nach Pensions-Heterogenität ---
  plots$cv_by_pension_heterogeneity <- ggplot(stats_by_config_year,
                                              aes(x = pension_cv, y = cv_cashflow)) +
    geom_point(aes(color = factor(year)), alpha = 0.5, size = 1) +
    geom_smooth(method = "loess", color = "darkred", se = TRUE, alpha = 0.2) +
    scale_x_continuous(labels = percent) +
    scale_y_continuous(labels = percent) +
    scale_color_viridis_d(name = "Jahr", option = "turbo") +
    labs(title = "Cashflow-Variabilität nach Pensions-Heterogenität",
         subtitle = "Heterogenere Renten = höhere Cashflow-Streuung?",
         x = "Pensions-CV (Std / Mean)", y = "Cashflow-CV") +
    theme_minimal()
  
  # --- PLOT 6: CV nach mittlerer Pension ---
  plots$cv_by_pension_level <- ggplot(stats_by_config_year,
                                      aes(x = bestand_pension_mean, y = cv_cashflow)) +
    geom_point(aes(color = factor(year)), alpha = 0.5, size = 1) +
    geom_smooth(method = "loess", color = "darkred", se = TRUE, alpha = 0.2) +
    scale_x_continuous(labels = label_number(scale = 1e-3, suffix = "k")) +
    scale_y_continuous(labels = percent) +
    scale_color_viridis_d(name = "Jahr", option = "turbo") +
    labs(title = "Cashflow-Variabilität nach Pensionsniveau",
         x = "Mittlere Pension (CHF)", y = "CV") +
    theme_minimal()
  
  # --- PLOT 7: Heatmap Alter vs. Grösse ---
  if (n_distinct(stats_by_config$bestand_n_total) > 1 & 
      n_distinct(stats_by_config$bestand_avg_age) > 1) {
    plots$heatmap_age_size <- stats_by_config %>%
      mutate(
        age_bin = cut(bestand_avg_age, breaks = 5),
        size_bin = cut(bestand_n_total, breaks = 5)
      ) %>%
      group_by(age_bin, size_bin) %>%
      summarise(mean_cv = mean(cv_cashflow, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = size_bin, y = age_bin, fill = mean_cv)) +
      geom_tile(color = "white") +
      geom_text(aes(label = percent(mean_cv, accuracy = 0.1)), color = "white", size = 3) +
      scale_fill_viridis_c(name = "CV", labels = percent, option = "magma", direction = -1) +
      labs(title = "CV-Heatmap: Alter vs. Bestandsgrösse",
           x = "Bestandsgrösse", y = "Durchschnittsalter") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # --- PLOT 8: Facetten nach Bestandseigenschaften über Zeit ---
  if (n_distinct(stats_by_config_year$bestand_n_total) > 1 &
      n_distinct(stats_by_config_year$bestand_avg_age) > 1) {
    plots$cv_timeline_facet <- stats_by_config_year %>%
      mutate(
        size_cat = cut(bestand_n_total, breaks = 3, labels = c("Klein", "Mittel", "Gross")),
        age_cat = cut(bestand_avg_age, breaks = 3, labels = c("Jung", "Mittel", "Alt"))
      ) %>%
      group_by(year, size_cat, age_cat) %>%
      summarise(mean_cv = mean(cv_cashflow, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = year, y = mean_cv, color = size_cat)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      facet_wrap(~age_cat, labeller = labeller(age_cat = function(x) paste("Alter:", x))) +
      scale_y_continuous(labels = percent) +
      scale_color_brewer(name = "Bestandsgrösse", palette = "Set1") +
      labs(title = "CV-Entwicklung nach Alter und Grösse",
           x = "Jahr", y = "CV") +
      theme_minimal()
  }
  
  # --- PLOT 9: Multivariate Übersicht (Scatter Matrix Style) ---
  var_labels <- c(
    bestand_n_total = "Bestandsgrösse",
    bestand_avg_age = "Durchschnittsalter",
    bestand_share_f = "Frauenanteil",
    bestand_share_married = "Verheiratetenanteil",
    pension_cv = "Pensions-Heterogenität"
  )
  plots$scatter_overview <- stats_by_config %>%
    select(cv_cashflow, bestand_n_total, bestand_avg_age,
           bestand_share_f, bestand_share_married, pension_cv) %>%
    pivot_longer(-cv_cashflow, names_to = "variable", values_to = "value") %>%
    mutate(variable = var_labels[as.character(variable)]) %>%
    ggplot(aes(x = value, y = cv_cashflow)) +
    geom_point(alpha = 0.5, color = "steelblue") +
    geom_smooth(method = "lm", color = "darkred", se = TRUE, alpha = 0.2) +
    facet_wrap(~variable, scales = "free_x") +
    scale_y_continuous(labels = percent) +
    labs(title = "CV-Treiber: Übersicht aller Bestandseigenschaften",
         x = NULL, y = "Cashflow-CV") +
    theme_minimal()
  
  # --- PLOT 10: Korrelationsmatrix ---
  cor_data <- stats_by_config %>%
    select(cv_cashflow, bestand_n_total, bestand_avg_age, 
           bestand_share_f, bestand_share_married, 
           bestand_pension_mean, pension_cv) %>%
    cor(use = "complete.obs")
  
  cor_df <- as.data.frame(cor_data)
  cor_df$var1 <- rownames(cor_df)
  cor_long <- cor_df %>%
    pivot_longer(-var1, names_to = "var2", values_to = "correlation") %>%
    mutate(var1 = as.character(var1), var2 = as.character(var2))
  
  cor_labels <- c(
    cv_cashflow = "CV", bestand_n_total = "Grösse",
    bestand_avg_age = "Alter", bestand_share_f = "Frauen",
    bestand_share_married = "Verheiratet",
    bestand_pension_mean = "Pension Ø", pension_cv = "Pension CV"
  )
  plots$correlation <- cor_long %>%
    mutate(
      var1 = cor_labels[var1],
      var2 = cor_labels[var2]
    ) %>%
    ggplot(aes(x = var1, y = var2, fill = correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", correlation)), color = "white", size = 3) +
    scale_fill_gradient2(low = "steelblue", mid = "white", high = "darkred",
                         midpoint = 0, limits = c(-1, 1), name = "Korrelation") +
    labs(title = "Korrelationsmatrix: Bestandseigenschaften vs. CV",
         x = NULL, y = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # ===========================================================================
  # Ausgabe
  # ===========================================================================
  
  cat("\n=== KORRELATIONEN MIT CASHFLOW-CV ===\n")
  cv_cors <- cor_data["cv_cashflow", ] %>% sort(decreasing = TRUE)
  print(round(cv_cors, 3))
  
  cat("\n=== STATISTIKEN PRO BESTANDSKONFIGURATION ===\n")
  print(stats_by_config %>% 
          select(bestand_n_total, bestand_avg_age, bestand_share_f, 
                 bestand_share_married, pension_cv, cv_cashflow) %>%
          mutate(across(c(bestand_share_f, bestand_share_married, pension_cv, cv_cashflow), 
                        ~percent(., accuracy = 0.1))) %>%
          head(10))
  
  # Plots ausgeben
  for (p in plots) {
    if (!is.null(p)) print(p)
  }
  
  return(list(
    by_config_year = stats_by_config_year,
    by_config = stats_by_config,
    correlations = cor_data,
    plots = plots
  ))
}

# Verwendung:
# results <- analyze_cashflow_dispersion(df)
# results$correlations        # Korrelationsmatrix
# results$by_config           # Aggregierte Stats pro Bestandskonfiguration
# results$plots$correlation   # Einzelner Plot

# ==============================================================================
# Analyse: Portfolio-Return-Volatilität nach Asset Allocation & Duration
# Version mit Duration-Matching Option
# ==============================================================================

library(ggplot2)
library(dplyr)
library(scales)

analyze_return_volatility <- function(df, 
                                      years_to_analyze = 1:20,
                                      duration_matching = TRUE) {
  #' @param df Bereits geladener Dataframe mit allen MC-Simulationen
  #' @param years_to_analyze Welche Jahre einbeziehen (default: 1-20)
  #' @param duration_matching Wenn TRUE, werden Duration-Effekte neutralisiert
  
  # Portfolio-Return berechnen (mit oder ohne Duration-Matching)
  if (duration_matching) {
    df <- df %>%
      mutate(
        portfolio_return_adj = portfolio_return - gov_bonds_duration_effect - corp_bonds_duration_effect
      )
    return_col <- "portfolio_return_adj"
    title_suffix <- " (Duration-Matched)"
    cat("Duration-Matching aktiv: gov_bonds_duration_effect und corp_bonds_duration_effect abgezogen\n")
  } else {
    df <- df %>%
      mutate(portfolio_return_adj = portfolio_return)
    return_col <- "portfolio_return_adj"
    title_suffix <- ""
  }
  
  # Strategie-Identifier erstellen
  df <- df %>%
    mutate(
      alloc_label = sprintf("Gov%.0f/Corp%.0f/Eq%.0f/RE%.0f/Alt%.0f",
                            weight_gov_bonds * 100,
                            weight_corp_bonds * 100,
                            weight_equities * 100,
                            weight_real_estate * 100,
                            weight_alternatives * 100),
      
      duration_label = sprintf("Gov:%s(%.0f)/Corp:%s(%.0f)",
                               gov_bond_duration_mode,
                               initial_gov_bond_duration,
                               corp_bond_duration_mode,
                               initial_corp_bond_duration),
      
      strategy = paste(alloc_label, duration_label, sep = " | ")
    )
  
  # Volatilität pro Strategie berechnen
  stats <- df %>%
    filter(year %in% years_to_analyze) %>%
    group_by(strategy, alloc_label, duration_label,
             weight_gov_bonds, weight_corp_bonds, weight_equities,
             weight_real_estate, weight_alternatives,
             initial_gov_bond_duration, gov_bond_duration_mode,
             initial_corp_bond_duration, corp_bond_duration_mode) %>%
    summarise(
      n_paths = n_distinct(path_nr),
      mean_return = mean(portfolio_return_adj, na.rm = TRUE),
      sd_return = sd(portfolio_return_adj, na.rm = TRUE),
      mean_return_raw = mean(portfolio_return, na.rm = TRUE),
      sd_return_raw = sd(portfolio_return, na.rm = TRUE),
      mean_duration_effect = mean(gov_bonds_duration_effect + corp_bonds_duration_effect, na.rm = TRUE),
      sd_duration_effect = sd(gov_bonds_duration_effect + corp_bonds_duration_effect, na.rm = TRUE),
      var_95 = quantile(portfolio_return_adj, 0.05, na.rm = TRUE),
      cvar_95 = mean(portfolio_return_adj[portfolio_return_adj <= quantile(portfolio_return_adj, 0.05)], na.rm = TRUE),
      sharpe = mean_return / sd_return,
      .groups = "drop"
    ) %>%
    arrange(sd_return)
  
  # --- PLOT 1: Return vs. Volatilität (Efficient Frontier) ---
  p1 <- ggplot(stats, aes(x = sd_return, y = mean_return)) +
    geom_point(aes(color = weight_equities, size = weight_gov_bonds), alpha = 0.7) +
    geom_text(aes(label = alloc_label), size = 2.5, vjust = -1, check_overlap = TRUE) +
    scale_color_viridis_c(name = "Equity", labels = percent) +
    scale_size_continuous(name = "Gov Bonds", range = c(2, 8)) +
    scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(title = paste0("Return vs. Volatilität", title_suffix), 
         x = "Volatilität (Std.Dev.)", y = "Mean Return") +
    theme_minimal()
  
  # --- PLOT 2: Volatilität-Ranking ---
  p2 <- stats %>%
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
  
  # --- PLOT 3: Duration-Effekt auf Volatilität ---
  p3 <- ggplot(stats, aes(x = initial_gov_bond_duration, y = sd_return)) +
    geom_point(aes(color = alloc_label, shape = gov_bond_duration_mode), size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "gray40", alpha = 0.2) +
    scale_y_continuous(labels = percent) +
    labs(title = paste0("Duration-Effekt auf Volatilität", title_suffix),
         x = "Gov-Bond Duration (Jahre)", y = "Volatilität",
         color = "Allocation", shape = "Duration Mode") +
    theme_minimal() +
    theme(legend.position = "bottom", legend.box = "vertical")
  
  # --- PLOT 4: Heatmap ---
  p4 <- stats %>%
    ggplot(aes(x = weight_equities, y = weight_gov_bonds, fill = sd_return)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.1f%%", sd_return * 100)), color = "white", size = 3) +
    scale_fill_viridis_c(name = "Volatilität", labels = percent, option = "magma", direction = -1) +
    scale_x_continuous(labels = percent) +
    scale_y_continuous(labels = percent) +
    labs(title = paste0("Volatilität: Equity vs. Gov-Bonds", title_suffix), 
         x = "Equity", y = "Gov Bonds") +
    theme_minimal()
  
  # --- PLOT 5: Vergleich Raw vs. Duration-Matched (nur wenn duration_matching = TRUE) ---
  if (duration_matching) {
    p5 <- stats %>%
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
  } else {
    p5 <- NULL
  }
  
  # Ausgabe
  cat("\n=== VOLATILITÄTS-RANKING", title_suffix, "===\n")
  print(stats %>% 
          select(alloc_label, duration_label, sd_return, mean_return, sharpe) %>% 
          head(10))
  
  if (duration_matching) {
    cat("\n=== DURATION-EFFEKT STATISTIK ===\n")
    cat(sprintf("Mittlerer Duration-Effekt: %.2f%%\n", mean(stats$mean_duration_effect) * 100))
    cat(sprintf("Std.Dev. Duration-Effekt:  %.2f%%\n", mean(stats$sd_duration_effect) * 100))
    cat(sprintf("Volatilitäts-Reduktion:    %.2f%% (Durchschnitt)\n", 
                mean((stats$sd_return_raw - stats$sd_return) / stats$sd_return_raw) * 100))
  }
  
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  if (!is.null(p5)) print(p5)
  
  return(list(
    summary = stats, 
    plots = list(frontier = p1, ranking = p2, duration = p3, heatmap = p4, comparison = p5),
    duration_matching = duration_matching
  ))
}

# Verwendung:
# Mit Duration-Matching (Default):
# results <- analyze_return_volatility(df, years_to_analyze = 1:20, duration_matching = TRUE)
#
# Ohne Duration-Matching:
# results <- analyze_return_volatility(df, years_to_analyze = 1:20, duration_matching = FALSE)
#
# results$summary  # Tabelle mit beiden Volatilitäten (raw und adjusted)
# results$plots$comparison  # Vergleichsplot
# ============================================================================
# SENSITIVITÄTSANALYSE: Laden und Auswerten der sensitivity_analysis.py Daten
# ============================================================================
# Für die systematische Auswertung der Szenario-Simulationen aus
# sensitivity_analysis.py (Populations-Varianten x Allokationen x Duration x SS)
# ============================================================================

library(parallel)    # Built-in, keine Installation nötig

# ==============================================================================
# 1. DATENIMPORT: Sensitivity-Szenario-CSVs (parallelisiert)
# ==============================================================================

load_sensitivity_data <- function(data_dir = "data/sensitivity/",
                                  pattern = "^sensitivity_scenario_.*\\.csv$",
                                  n_cores = NULL,
                                  verbose = TRUE) {
  #' Lädt alle Szenario-CSVs aus dem Output von sensitivity_analysis.py
  #'
  #' @param data_dir   Pfad zum Verzeichnis mit den Szenario-CSVs

  #' @param pattern    Regex-Pattern für die Dateinamen
  #' @param n_cores    Anzahl CPU-Kerne (NULL = auto: alle - 1)
  #' @param verbose    Fortschrittsanzeige
  #' @return data.table mit allen Pfaden aller Szenarien

  library(data.table)
  library(parallel)

  csv_files <- list.files(data_dir, pattern = pattern, full.names = TRUE)

  if (length(csv_files) == 0) {
    stop("Keine Sensitivity-CSVs gefunden in: ", data_dir,
         "\n  Pattern: ", pattern)
  }

  if (is.null(n_cores)) n_cores <- max(1, detectCores() - 1)

  if (verbose) {
    cat(sprintf("=== SENSITIVITY DATA LOADER ===\n"))
    cat(sprintf("Verzeichnis:   %s\n", data_dir))
    cat(sprintf("Dateien:       %d\n", length(csv_files)))
    cat(sprintf("Kerne:         %d\n", n_cores))
  }

  start_time <- Sys.time()

  # Paralleles Laden
  cl <- makeCluster(n_cores)
  clusterEvalQ(cl, library(data.table))

  df_list <- parLapply(cl, csv_files, function(file) {
    dt <- fread(file, sep = ";", dec = ".", stringsAsFactors = FALSE,
                showProgress = FALSE)
    dt$source_file <- basename(file)
    return(dt)
  })

  stopCluster(cl)

  if (verbose) {
    load_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("Laden:         %.1f Sek.\n", load_time))
  }

  # Kombinieren
  dt <- rbindlist(df_list, fill = TRUE)
  rm(df_list); gc(verbose = FALSE)

  # Eindeutige path_nr innerhalb jedes Szenarios belassen (scenario_id + path_nr ist unique)
  # Zusätzlich globale unique ID
  dt[, global_path_id := paste(scenario_id, path_nr, sep = "_")]
  dt[, global_path_nr := .GRP, by = global_path_id]
  dt[, global_path_id := NULL]

  # Numerische Spalten sicherstellen
  num_cols <- c(
    "scenario_id", "year", "path_nr",
    "deckungsgrad", "V_t", "W_t", "portfolio_return",
    "gov_bonds_return", "corp_bonds_return", "equities_return",
    "realestate_return", "alternatives_return",
    "gov_bonds_coupon", "gov_bonds_duration_effect",
    "corp_bonds_coupon", "corp_bonds_duration_effect",
    "corp_bonds_default_loss",
    "alt_bonds_coupon", "alt_bonds_duration_effect", "alt_bonds_default_loss",
    "r1_t", "i_gov_bonds_t", "i_corp_bonds_t", "i_alt_bonds_t", "i_tech_t",
    "cashflow_rent", "cashflow_admin_fee", "cashflow_total",
    "special_pension_payout",
    "liability_duration", "gov_bond_duration", "corp_bond_duration", "alt_bond_duration",
    "num_pensioners", "num_widows",
    "interest_rate_cap_payout", "interest_rate_cap_active",
    # Config-Parameter
    "weight_gov_bonds", "weight_corp_bonds", "weight_equities",
    "weight_real_estate", "weight_alternatives",
    "initial_gov_bond_duration", "initial_corp_bond_duration",
    "mu_interest_rate", "mu_equities", "sigma_equities",
    "yield_curve_slope", "technical_rate_duration",
    "liability_discount_spread", "technical_rate_floor",
    "general_reserve_rate", "n_paths", "t_horizon",
    # Bestand
    "bestand_n_total", "bestand_avg_age",
    "bestand_n_m", "bestand_n_f", "bestand_share_m", "bestand_share_f",
    "bestand_n_married", "bestand_share_married", "bestand_share_single",
    "bestand_total_initial_pension",
    "bestand_pension_min", "bestand_pension_p10", "bestand_pension_p25",
    "bestand_pension_median", "bestand_pension_mean", "bestand_pension_std",
    "bestand_pension_p75", "bestand_pension_p90", "bestand_pension_max",
    "bestand_W0", "bestand_spouse_age_diff_mean", "bestand_spouse_age_diff_std",
    "bestand_spouse_pension_rate",
    # Pop-Parameter aus Szenario
    "pop_n_population", "pop_age_mean", "pop_pension_mean",
    "pop_share_married", "pop_spouse_pension_rate", "pop_spouse_age_diff",
    "pop_share_female"
  )

  for (col in num_cols) {
    if (col %in% names(dt)) {
      dt[, (col) := as.numeric(get(col))]
    }
  }
  dt[, year := as.integer(year)]
  dt[, scenario_id := as.integer(scenario_id)]

  if (verbose) {
    total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    n_scen <- uniqueN(dt$scenario_id)
    n_paths <- uniqueN(dt$global_path_nr)
    cat(sprintf("\n=== ZUSAMMENFASSUNG ===\n"))
    cat(sprintf("Zeilen:            %s\n", format(nrow(dt), big.mark = "'")))
    cat(sprintf("Szenarien:         %d\n", n_scen))
    cat(sprintf("Globale Pfade:     %s\n", format(n_paths, big.mark = "'")))
    cat(sprintf("Spalten:           %d\n", ncol(dt)))
    cat(sprintf("Speicher:          %.1f MB\n", as.numeric(object.size(dt)) / 1024^2))
    cat(sprintf("Gesamtzeit:        %.1f Sek.\n", total_time))
    cat(sprintf("Durchsatz:         %s Zeilen/Sek.\n",
                format(round(nrow(dt) / total_time), big.mark = "'")))
  }

  return(dt)
}
# ==============================================================================
# load_sensitivity_lean()
# ------------------------------------------------------------------------------
# Schlanke Alternative zu load_sensitivity_data() – lädt nur die Spalten die
# von cashflow_range_plot() und cashflow_hypothesis_test() benötigt werden.
#
# Speichervergleich (100 Mio Zeilen):
#   load_sensitivity_data()  →  90 Spalten  →  ~64 GB RAM  (+ ~220 GB Peak)
#   load_sensitivity_lean()  →  13 Spalten  →  ~7.5 GB RAM (+ ~25 GB Peak)
#
# Verwendung:
#   df <- load_sensitivity_lean()                        # Default
#   df <- load_sensitivity_lean(extra_cols = "deckungsgrad")  # + eigene Spalten
#   df <- load_sensitivity_lean(as_data_frame = TRUE)    # als data.frame statt data.table
# ==============================================================================

load_sensitivity_lean <- function(
    data_dir     = "data/sensitivity/",
    pattern      = "^sensitivity_scenario_.*\\.csv$",
    n_cores      = NULL,
    extra_cols   = NULL,    # character vector: zusätzliche Spalten einlesen
    as_data_frame = FALSE,  # TRUE: gibt data.frame zurück (für ggplot-Workflows)
    verbose      = TRUE
) {
  #' @param data_dir      Pfad zum Verzeichnis mit den Szenario-CSVs
  #' @param pattern       Regex-Pattern für die Dateinamen
  #' @param n_cores       Anzahl CPU-Kerne (NULL = auto: alle - 1)
  #' @param extra_cols    Zusätzliche Spalten neben den Pflicht-Spalten
  #' @param as_data_frame Rückgabe als data.frame statt data.table
  #' @param verbose       Fortschrittsanzeige
  #' @return data.table (oder data.frame) mit nur den benötigten Spalten

  library(data.table)
  library(parallel)

  # ---- Pflichtspalten --------------------------------------------------------
  # Identifikation + Zeitachse
  id_cols <- c("scenario_id", "path_nr", "year")

  # Zielvariable
  target_col <- "cashflow_rent"

  # 7 Bestandseigenschaften (für range_plot und hypothesis_test)
  property_cols <- c(
    "bestand_n_total",            # Bestandsgrösse
    "bestand_avg_age",            # Durchschnittsalter
    "bestand_share_married",      # Verheiratetenanteil
    "bestand_spouse_pension_rate",# Ehegattenrente-Rate
    "bestand_spouse_age_diff_mean",# Altersunterschied Ehegatte
    "pop_share_female",           # Frauenanteil (Population)
    "bestand_pension_mean"        # Rentenhöhe Ø
  )

  select_cols <- unique(c(id_cols, target_col, property_cols, extra_cols))

  # ---- Dateien finden --------------------------------------------------------
  csv_files <- list.files(data_dir, pattern = pattern, full.names = TRUE)
  if (length(csv_files) == 0) {
    stop("Keine Sensitivity-CSVs gefunden in: ", data_dir,
         "\n  Pattern: ", pattern)
  }

  if (is.null(n_cores)) n_cores <- max(1, detectCores() - 1)

  if (verbose) {
    cat("=== SENSITIVITY LEAN LOADER ===\n")
    cat(sprintf("Verzeichnis:   %s\n", data_dir))
    cat(sprintf("Dateien:       %d\n", length(csv_files)))
    cat(sprintf("Kerne:         %d\n", n_cores))
    cat(sprintf("Spalten:       %d  (statt 90)\n", length(select_cols)))
    cat(sprintf("  %s\n", paste(select_cols, collapse = ", ")))
  }

  start_time <- Sys.time()

  # ---- Spaltennamen aus erster Datei lesen (für Validierung) ----------------
  header <- fread(csv_files[1], sep = ";", nrows = 0L)
  available_cols <- names(header)

  missing_required <- setdiff(c(id_cols, target_col), available_cols)
  if (length(missing_required) > 0) {
    stop("Pflicht-Spalten fehlen in den CSVs: ",
         paste(missing_required, collapse = ", "))
  }

  missing_props <- setdiff(property_cols, available_cols)
  if (length(missing_props) > 0) {
    warning("Folgende Eigenschafts-Spalten fehlen und werden übersprungen:\n  ",
            paste(missing_props, collapse = ", "))
  }

  missing_extra <- setdiff(extra_cols, available_cols)
  if (length(missing_extra) > 0) {
    warning("Folgende extra_cols fehlen und werden übersprungen:\n  ",
            paste(missing_extra, collapse = ", "))
  }

  # Nur Spalten einlesen die tatsächlich vorhanden sind
  read_cols <- intersect(select_cols, available_cols)

  if (verbose) {
    cat(sprintf("Laden:         "))
  }

  # ---- Paralleles Laden (nur select_cols via fread's select-Argument) --------
  cl <- makeCluster(n_cores)
  clusterEvalQ(cl, library(data.table))
  clusterExport(cl, c("read_cols"), envir = environment())

  df_list <- parLapply(cl, csv_files, function(file) {
    dt <- fread(
      file,
      sep          = ";",
      dec          = ".",
      select       = read_cols,   # <-- nur nötige Spalten lesen (Kern des Speedups)
      stringsAsFactors = FALSE,
      showProgress = FALSE
    )
    dt[, source_file := basename(file)]
    return(dt)
  })

  stopCluster(cl)

  load_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (verbose) cat(sprintf("%.1f Sek.\n", load_time))

  # ---- Zusammenführen --------------------------------------------------------
  dt <- rbindlist(df_list, fill = TRUE)
  rm(df_list); gc(verbose = FALSE)

  # Globale Pfad-ID (scenario_id + path_nr → eindeutiger Pfad)
  dt[, .path_key := paste(scenario_id, path_nr, sep = "_")]
  dt[, global_path_nr := .GRP, by = .path_key]

  # ---- Typen sicherstellen ---------------------------------------------------
  numeric_cols <- c(
    target_col, property_cols,
    intersect(extra_cols, available_cols)
  )
  for (col in intersect(numeric_cols, names(dt))) {
    dt[, (col) := as.numeric(get(col))]
  }
  if ("year"        %in% names(dt)) dt[, year        := as.integer(year)]
  if ("scenario_id" %in% names(dt)) dt[, scenario_id := as.integer(scenario_id)]
  if ("path_nr"     %in% names(dt)) dt[, path_nr     := as.integer(path_nr)]

  # ---- Zusammenfassung -------------------------------------------------------
  if (verbose) {
    total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    mem_mb     <- as.numeric(object.size(dt)) / 1024^2

    cat("\n=== ZUSAMMENFASSUNG ===\n")
    cat(sprintf("Zeilen:            %s\n",   format(nrow(dt),                  big.mark = "'")))
    cat(sprintf("Szenarien:         %d\n",   uniqueN(dt$scenario_id)))
    cat(sprintf("Globale Pfade:     %s\n",   format(uniqueN(dt$global_path_nr), big.mark = "'")))
    cat(sprintf("Spalten geladen:   %d\n",   ncol(dt)))
    cat(sprintf("Speicher:          %.1f MB  (%.1f GB)\n", mem_mb, mem_mb / 1024))
    cat(sprintf("Gesamtzeit:        %.1f Sek.\n", total_time))
    cat(sprintf("Durchsatz:         %s Zeilen/Sek.\n",
                format(round(nrow(dt) / total_time), big.mark = "'")))

    # Verfügbarkeit der Eigenschafts-Spalten
    cat("\nEigenschafts-Spalten:\n")
    for (col in property_cols) {
      ok  <- col %in% names(dt)
      pct <- if (ok) 100 * mean(!is.na(dt[[col]])) else 0
      cat(sprintf("  %s %-35s  %s\n",
                  if (ok) "✓" else "✗", col,
                  if (ok) sprintf("%.1f%% non-NA", pct) else "FEHLT"))
    }
  }

  if (as_data_frame) return(as.data.frame(dt))
  return(dt)
}


# ==============================================================================
# Verwendungsbeispiele
# ==============================================================================
#
# # Standard-Import für range_plot und hypothesis_test:
# df <- load_sensitivity_lean()
#
# # Mit zusätzlicher Spalte (z.B. für Deckungsgrad-Analyse):
# df <- load_sensitivity_lean(extra_cols = c("deckungsgrad", "V_t"))
#
# # Als data.frame (für dplyr/ggplot ohne data.table):
# df <- load_sensitivity_lean(as_data_frame = TRUE)
#
# # Direkt weiterverwenden:
# cashflow_range_plot(df, metric = "CV")
# result <- cashflow_hypothesis_test(df)

load_sensitivity_summary <- function(data_dir = "data/sensitivity/",
                                     filename = "sensitivity_summary.csv") {
  #' Lädt die Übersichts-CSV mit einer Zeile pro Szenario
  #'
  #' @param data_dir   Verzeichnis
  #' @param filename   Dateiname der Summary-CSV
  #' @return data.table

  filepath <- file.path(data_dir, filename)
  if (!file.exists(filepath)) {
    stop("Summary-Datei nicht gefunden: ", filepath)
  }

  dt <- fread(filepath, sep = ";", dec = ".", stringsAsFactors = FALSE)
  cat(sprintf("Summary geladen: %d Szenarien, %d Spalten\n", nrow(dt), ncol(dt)))
  return(dt)
}


# ==============================================================================
# 2. SENSITIVITÄTSANALYSE: Risikometriken pro Szenario (parallelisiert)
# ==============================================================================

compute_scenario_risk_metrics <- function(dt,
                                          dg_threshold = 0.80,
                                          n_cores = NULL) {
  #' Berechnet Risikometriken pro Szenario (parallelisiert via data.table)
  #'
  #' @param dt           data.table aus load_sensitivity_data()
  #' @param dg_threshold Schwelle für Unterdeckung (Default: 0.80 = 80%)
  #' @param n_cores      Anzahl Kerne für data.table (NULL = auto)
  #' @return data.table mit einer Zeile pro Szenario

  if (!is.null(n_cores)) setDTthreads(n_cores)
  dt <- as.data.table(dt)

  cat("Berechne Risikometriken pro Szenario...\n")
  start_time <- Sys.time()

  # --- Pro Pfad: min DG, Default-Jahr, Terminal-DG ---
  path_stats <- dt[, .(
    min_dg         = min(deckungsgrad, na.rm = TRUE),
    terminal_dg    = deckungsgrad[which.max(year)],
    terminal_V     = V_t[which.max(year)],
    terminal_W     = W_t[which.max(year)],
    has_underfunding = any(deckungsgrad < dg_threshold),
    has_default    = any(V_t <= 0),
    first_uf_year  = fifelse(any(deckungsgrad < dg_threshold),
                             min(year[deckungsgrad < dg_threshold]),
                             NA_real_),
    cum_portfolio  = prod(1 + portfolio_return) - 1,
    mean_cashflow  = mean(cashflow_total, na.rm = TRUE),
    max_r1t        = max(r1_t, na.rm = TRUE),
    min_r1t        = min(r1_t, na.rm = TRUE)
  ), by = .(scenario_id, path_nr)]

  # --- Aggregation pro Szenario ---
  scenario_risk <- path_stats[, .(
    n_paths             = .N,
    prob_underfunding   = mean(has_underfunding),
    prob_default        = mean(has_default),
    n_underfunding      = sum(has_underfunding),
    n_default           = sum(has_default),
    # Terminal Deckungsgrad
    end_dg_mean         = mean(terminal_dg, na.rm = TRUE),
    end_dg_median       = median(terminal_dg, na.rm = TRUE),
    end_dg_p5           = quantile(terminal_dg, 0.05, na.rm = TRUE),
    end_dg_p25          = quantile(terminal_dg, 0.25, na.rm = TRUE),
    end_dg_p75          = quantile(terminal_dg, 0.75, na.rm = TRUE),
    end_dg_p95          = quantile(terminal_dg, 0.95, na.rm = TRUE),
    # Minimum Deckungsgrad
    min_dg_mean         = mean(min_dg, na.rm = TRUE),
    min_dg_p5           = quantile(min_dg, 0.05, na.rm = TRUE),
    # Shortfall
    mean_shortfall      = mean(pmax(0, dg_threshold - min_dg), na.rm = TRUE),
    cvar_dg             = mean(terminal_dg[terminal_dg <= quantile(terminal_dg, 0.05)],
                               na.rm = TRUE),
    # Rendite
    cum_return_mean     = mean(cum_portfolio, na.rm = TRUE),
    cum_return_p5       = quantile(cum_portfolio, 0.05, na.rm = TRUE),
    # Cashflow
    mean_cashflow       = mean(mean_cashflow, na.rm = TRUE),
    # Timing
    mean_uf_year        = mean(first_uf_year, na.rm = TRUE)
  ), by = scenario_id]

  # --- Szenario-Parameter joinen ---
  scenario_params <- unique(dt[, .(
    scenario_id,
    weight_gov_bonds, weight_corp_bonds, weight_equities,
    weight_real_estate, weight_alternatives,
    gov_bond_duration_mode, corp_bond_duration_mode,
    initial_gov_bond_duration, initial_corp_bond_duration,
    sammelstiftung_enabled, sammelstiftung_interval,
    pop_n_population, pop_age_mean, pop_pension_mean,
    pop_share_married, pop_spouse_pension_rate,
    bestand_n_total, bestand_avg_age, bestand_share_married,
    bestand_pension_mean, bestand_W0, bestand_spouse_pension_rate,
    general_reserve_rate
  )], by = "scenario_id")

  # Falls pop_-Spalten nicht existieren (ältere Daten), graceful degradation
  cols_to_join <- intersect(names(scenario_params), names(dt))
  scenario_params <- unique(dt[, ..cols_to_join])

  result <- scenario_risk[scenario_params, on = "scenario_id", nomatch = NULL]

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat(sprintf("Fertig: %d Szenarien in %.1f Sek.\n", nrow(result), elapsed))

  return(result)
}


# ==============================================================================
# 3. GRAFISCHE SENSITIVITÄTSANALYSEN
# ==============================================================================

# --------------------------------------------------------------------------
# 3a. Tornado-Diagramm: Welcher Parameter hat den grössten Einfluss?
# --------------------------------------------------------------------------

plot_sensitivity_tornado <- function(risk_dt,
                                     target_var = "prob_underfunding",
                                     target_label = "P(Unterdeckung)") {
  #' Tornado-Diagramm: Einfluss jedes variierten Parameters auf die Zielgrösse
  #'
  #' @param risk_dt      data.table aus compute_scenario_risk_metrics()
  #' @param target_var   Zielgrösse (Spaltenname)
  #' @param target_label Label für die Achse
  #' @return ggplot

  dt <- as.data.table(risk_dt)

  # Parameter die variiert wurden (nur numerische)
  param_cols <- c(
    "pop_n_population", "pop_age_mean", "pop_pension_mean",
    "pop_share_married", "pop_spouse_pension_rate",
    "weight_gov_bonds", "weight_corp_bonds", "weight_equities",
    "weight_real_estate", "weight_alternatives",
    "initial_gov_bond_duration", "initial_corp_bond_duration"
  )
  param_cols <- intersect(param_cols, names(dt))

  # Für jeden Parameter: Range des Targets bei Min vs. Max des Parameters
  tornado_data <- lapply(param_cols, function(p) {
    if (!is.numeric(dt[[p]]) || uniqueN(dt[[p]]) < 2) return(NULL)

    # Dezile des Parameters
    q_low  <- quantile(dt[[p]], 0.1, na.rm = TRUE)
    q_high <- quantile(dt[[p]], 0.9, na.rm = TRUE)

    if (q_low == q_high) return(NULL)

    val_low  <- mean(dt[get(p) <= q_low, get(target_var)], na.rm = TRUE)
    val_high <- mean(dt[get(p) >= q_high, get(target_var)], na.rm = TRUE)

    data.table(
      parameter = p,
      low_val   = val_low,
      high_val  = val_high,
      range     = abs(val_high - val_low)
    )
  })

  tornado_dt <- rbindlist(tornado_data[!sapply(tornado_data, is.null)])

  if (nrow(tornado_dt) == 0) {
    cat("Keine variierten Parameter gefunden.\n")
    return(NULL)
  }

  # Schöne Labels
  label_map <- c(
    pop_n_population       = "Bestandsgrösse",
    pop_age_mean           = "Durchschnittsalter",
    pop_pension_mean       = "Mittlere Rente",
    pop_share_married      = "Anteil Verheiratete",
    pop_spouse_pension_rate = "Ehegattenrente (%)",
    weight_gov_bonds       = "Gov Bonds Gewicht",
    weight_corp_bonds      = "Corp Bonds Gewicht",
    weight_equities        = "Aktien Gewicht",
    weight_real_estate     = "Immobilien Gewicht",
    weight_alternatives    = "Alternatives Gewicht",
    initial_gov_bond_duration = "Gov Bond Duration",
    initial_corp_bond_duration = "Corp Bond Duration"
  )

  tornado_dt[, param_label := ifelse(
    parameter %in% names(label_map),
    label_map[parameter],
    parameter
  )]

  baseline <- mean(dt[[target_var]], na.rm = TRUE)

  p <- ggplot(tornado_dt, aes(x = reorder(param_label, range))) +
    geom_segment(aes(y = low_val, yend = high_val, xend = param_label),
                 linewidth = 6, color = "#2E86AB", alpha = 0.7) +
    geom_point(aes(y = low_val), size = 3, color = "#1B5E7B") +
    geom_point(aes(y = high_val), size = 3, color = "#1B5E7B") +
    geom_hline(yintercept = baseline, linetype = "dashed", color = "red", linewidth = 0.8) +
    annotate("text", x = 0.5, y = baseline, label = sprintf("Basis: %.1f%%", baseline * 100),
             hjust = -0.1, color = "red", size = 3.5) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(
      title = paste("Tornado-Diagramm:", target_label),
      subtitle = "Effekt der Parameter-Variation (10. vs. 90. Perzentil)",
      x = NULL,
      y = target_label
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.major.y = element_blank())

  return(p)
}


# --------------------------------------------------------------------------
# 3b. Heatmap: Asset Allocation vs. Risiko
# --------------------------------------------------------------------------

plot_sensitivity_allocation_heatmap <- function(risk_dt,
                                                 target_var = "prob_underfunding",
                                                 target_label = "P(Unterdeckung)",
                                                 x_var = "weight_equities",
                                                 y_var = "weight_gov_bonds",
                                                 x_label = "Aktien",
                                                 y_label = "Gov Bonds") {
  #' Heatmap: Zwei Allokationsdimensionen vs. Zielgrösse
  #'
  #' @param risk_dt      data.table aus compute_scenario_risk_metrics()
  #' @param target_var   Zielgrösse
  #' @param x_var, y_var Achsen-Variablen
  #' @return ggplot

  dt <- as.data.table(risk_dt)

  # Aggregiere über andere Dimensionen (Mittelwert)
  agg <- dt[, .(target = mean(get(target_var), na.rm = TRUE)),
            by = .(x = get(x_var), y = get(y_var))]

  p <- ggplot(agg, aes(x = x, y = y, fill = target)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.1f%%", target * 100)),
              color = "white", size = 3.5, fontface = "bold") +
    scale_fill_viridis_c(name = target_label, labels = scales::percent,
                         option = "magma", direction = -1) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = paste("Heatmap:", target_label),
      subtitle = paste(x_label, "vs.", y_label, "(Mittelwert über andere Parameter)"),
      x = x_label,
      y = y_label
    ) +
    theme_minimal(base_size = 12)

  return(p)
}


# --------------------------------------------------------------------------
# 3c. Duration-Modus Vergleich
# --------------------------------------------------------------------------

plot_sensitivity_duration_mode <- function(risk_dt,
                                            target_var = "prob_underfunding",
                                            target_label = "P(Unterdeckung)") {
  #' Box-Plot: Risiko nach Duration-Modus-Kombination
  #'
  #' @param risk_dt      data.table aus compute_scenario_risk_metrics()
  #' @param target_var   Zielgrösse
  #' @return ggplot

  dt <- as.data.table(risk_dt)

  dt[, dur_combo := paste0("Gov:", gov_bond_duration_mode,
                           " / Corp:", corp_bond_duration_mode)]

  # Box-Plot
  p <- ggplot(as.data.frame(dt),
              aes(x = reorder(dur_combo, get(target_var), FUN = median),
                  y = get(target_var),
                  fill = dur_combo)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    coord_flip() +
    labs(
      title = paste("Duration-Modus:", target_label),
      subtitle = "Roter Diamant = Mittelwert",
      x = NULL,
      y = target_label
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")

  return(p)
}


# --------------------------------------------------------------------------
# 3d. Sammelstiftung-Effekt
# --------------------------------------------------------------------------

plot_sensitivity_sammelstiftung <- function(risk_dt,
                                            target_var = "prob_underfunding",
                                            target_label = "P(Unterdeckung)") {
  #' Vergleich: Mit vs. ohne Sammelstiftung-Modus
  #'
  #' @param risk_dt    data.table aus compute_scenario_risk_metrics()
  #' @param target_var Zielgrösse
  #' @return ggplot

  dt <- as.data.table(risk_dt)

  if (!"sammelstiftung_enabled" %in% names(dt)) {
    cat("Spalte 'sammelstiftung_enabled' nicht vorhanden.\n")
    return(NULL)
  }

  dt[, ss_label := fifelse(sammelstiftung_enabled == TRUE | sammelstiftung_enabled == "True",
                           "Sammelstiftung", "Kein SS-Modus")]

  # Paired comparison: gleiche Strategie mit/ohne SS
  paired <- dt[, .(mean_target = mean(get(target_var), na.rm = TRUE)),
               by = .(ss_label, weight_gov_bonds, weight_corp_bonds, weight_equities,
                     weight_real_estate, weight_alternatives,
                     gov_bond_duration_mode, corp_bond_duration_mode)]

  p <- ggplot(as.data.frame(dt), aes(x = ss_label, y = get(target_var), fill = ss_label)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
    geom_jitter(alpha = 0.15, width = 0.15, size = 1) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red") +
    scale_fill_manual(values = c("Sammelstiftung" = "#2E86AB", "Kein SS-Modus" = "#E94F37")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(
      title = paste("Sammelstiftung-Effekt:", target_label),
      subtitle = "Roter Diamant = Mittelwert",
      x = NULL,
      y = target_label
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")

  # Statistischer Test
  vals_ss <- dt[ss_label == "Sammelstiftung", get(target_var)]
  vals_no <- dt[ss_label == "Kein SS-Modus", get(target_var)]

  if (length(vals_ss) > 1 && length(vals_no) > 1) {
    wt <- wilcox.test(vals_ss, vals_no, conf.int = TRUE)
    cat(sprintf("\n=== Wilcoxon-Test: Sammelstiftung-Effekt auf %s ===\n", target_label))
    cat(sprintf("  Mittelwert SS:      %.2f%%\n", mean(vals_ss, na.rm = TRUE) * 100))
    cat(sprintf("  Mittelwert kein SS: %.2f%%\n", mean(vals_no, na.rm = TRUE) * 100))
    cat(sprintf("  p-Wert:             %.4f %s\n", wt$p.value,
                ifelse(wt$p.value < 0.001, "***",
                       ifelse(wt$p.value < 0.01, "**",
                              ifelse(wt$p.value < 0.05, "*", "n.s.")))))
  }

  return(p)
}


# --------------------------------------------------------------------------
# 3e. Populations-Sensitivität: Facetten-Plot
# --------------------------------------------------------------------------

plot_sensitivity_population <- function(risk_dt,
                                         target_var = "prob_underfunding",
                                         target_label = "P(Unterdeckung)") {
  #' Einfluss der Populations-Parameter auf die Zielgrösse
  #' Facetten-Plot: Jeder variierte Pop-Parameter als Panel
  #'
  #' @param risk_dt      data.table
  #' @param target_var   Zielgrösse
  #' @return ggplot

  dt <- as.data.table(risk_dt)

  pop_vars <- c("pop_n_population", "pop_age_mean", "pop_pension_mean",
                "pop_share_married", "pop_spouse_pension_rate")
  pop_vars <- intersect(pop_vars, names(dt))

  pop_labels <- c(
    pop_n_population       = "Bestandsgrösse",
    pop_age_mean           = "Durchschnittsalter",
    pop_pension_mean       = "Mittlere Rente (CHF)",
    pop_share_married      = "Anteil Verheiratete",
    pop_spouse_pension_rate = "Ehegattenrente (%)"
  )

  # Long-Format für Facetten
  plot_rows <- lapply(pop_vars, function(pv) {
    if (uniqueN(dt[[pv]]) < 2) return(NULL)
    data.table(
      param_name  = pop_labels[pv],
      param_value = as.numeric(dt[[pv]]),
      target      = dt[[target_var]]
    )
  })

  plot_dt <- rbindlist(plot_rows[!sapply(plot_rows, is.null)])

  if (nrow(plot_dt) == 0) {
    cat("Keine variierten Populations-Parameter gefunden.\n")
    return(NULL)
  }

  p <- ggplot(as.data.frame(plot_dt),
              aes(x = factor(param_value), y = target)) +
    geom_boxplot(fill = "#2E86AB", alpha = 0.6, outlier.alpha = 0.2) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
    facet_wrap(~ param_name, scales = "free_x", ncol = 3) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(
      title = paste("Populations-Sensitivität:", target_label),
      subtitle = "Roter Diamant = Mittelwert; Boxplot = Streuung über Strategie-Varianten",
      x = "Parameter-Wert",
      y = target_label
    ) +
    theme_minimal(base_size = 11) +
    theme(strip.text = element_text(face = "bold", size = 10),
          axis.text.x = element_text(size = 8))

  return(p)
}


# --------------------------------------------------------------------------
# 3f. Deckungsgrad-Fächer: Zeitverlauf pro Szenario-Gruppe
# --------------------------------------------------------------------------

plot_sensitivity_dg_fan <- function(dt,
                                    group_var = "gov_bond_duration_mode",
                                    group_label = "Duration-Modus",
                                    max_scenarios_per_group = 5) {
  #' Deckungsgrad-Fächer (P5/P25/Median/P75/P95) nach Szenario-Gruppe
  #'
  #' @param dt              data.table aus load_sensitivity_data()
  #' @param group_var       Gruppierungsvariable
  #' @param group_label     Label für die Legende
  #' @param max_scenarios_per_group  Max. Szenarien pro Gruppe (Performance)
  #' @return ggplot

  dt <- as.data.table(dt)

  if (!group_var %in% names(dt)) {
    stop("Spalte '", group_var, "' nicht in den Daten.")
  }

  # Für Performance: nur Subset
  selected_scenarios <- dt[, .(n = .N), by = .(scenario_id, grp = get(group_var))][
    , .SD[1:min(.N, max_scenarios_per_group)], by = grp]$scenario_id

  dg_stats <- dt[scenario_id %in% selected_scenarios, .(
    median_dg = median(deckungsgrad, na.rm = TRUE),
    p5_dg     = quantile(deckungsgrad, 0.05, na.rm = TRUE),
    p25_dg    = quantile(deckungsgrad, 0.25, na.rm = TRUE),
    p75_dg    = quantile(deckungsgrad, 0.75, na.rm = TRUE),
    p95_dg    = quantile(deckungsgrad, 0.95, na.rm = TRUE)
  ), by = .(year, group = get(group_var))]

  p <- ggplot(as.data.frame(dg_stats), aes(x = year)) +
    geom_ribbon(aes(ymin = p5_dg, ymax = p95_dg, fill = group), alpha = 0.15) +
    geom_ribbon(aes(ymin = p25_dg, ymax = p75_dg, fill = group), alpha = 0.25) +
    geom_line(aes(y = median_dg, color = group), linewidth = 1) +
    geom_hline(yintercept = 1.0, linetype = "dotted", color = "gray40") +
    geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", alpha = 0.5) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, NA)) +
    scale_x_continuous(breaks = seq(0, 50, 5)) +
    labs(
      title = paste("Deckungsgrad-Fächer nach", group_label),
      subtitle = "Bänder: P5-P95 (hell), P25-P75 (dunkel), Linie: Median",
      x = "Jahr",
      y = "Deckungsgrad",
      fill = group_label,
      color = group_label
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  return(p)
}


# --------------------------------------------------------------------------
# 3g. Interaktionseffekte: Allocation x Duration x Population
# --------------------------------------------------------------------------

plot_sensitivity_interaction <- function(risk_dt,
                                          x_var = "pop_age_mean",
                                          color_var = "gov_bond_duration_mode",
                                          target_var = "prob_underfunding",
                                          x_label = "Durchschnittsalter",
                                          color_label = "Duration-Modus",
                                          target_label = "P(Unterdeckung)") {
  #' Interaktionsplot: Parameter x vs. Zielgrösse, eingefärbt nach Gruppe
  #'
  #' @param risk_dt    data.table
  #' @param x_var      X-Achse
  #' @param color_var  Einfärbung
  #' @param target_var Zielgrösse
  #' @return ggplot

  dt <- as.data.table(risk_dt)

  p <- ggplot(as.data.frame(dt),
              aes(x = get(x_var), y = get(target_var), color = factor(get(color_var)))) +
    geom_point(alpha = 0.4, size = 2) +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.15, linewidth = 1) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(
      title = paste("Interaktion:", x_label, "x", color_label),
      x = x_label,
      y = target_label,
      color = color_label
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  return(p)
}


# ==============================================================================
# 4. KOMPLETT-AUSWERTUNG: Alle Analysen auf einmal
# ==============================================================================

run_sensitivity_analysis <- function(data_dir = "data/sensitivity/",
                                     n_cores = NULL,
                                     dg_threshold = 0.80,
                                     save_plots = FALSE,
                                     plot_dir = "data/sensitivity/plots/") {
  #' Führt die komplette Sensitivitätsanalyse durch:
  #' 1. Daten laden (parallelisiert)
  #' 2. Risikometriken berechnen
  #' 3. Alle Visualisierungen erstellen
  #'
  #' @param data_dir      Verzeichnis mit sensitivity_scenario_*.csv Dateien
  #' @param n_cores       Anzahl CPU-Kerne (NULL = auto)
  #' @param dg_threshold  Schwelle für Unterdeckung
  #' @param save_plots    Plots als PNG speichern?
  #' @param plot_dir      Verzeichnis für PNG-Dateien
  #' @return Liste mit dt (Rohdaten), risk (Risikometriken), plots (alle Grafiken)

  if (is.null(n_cores)) n_cores <- max(1, detectCores() - 1)
  setDTthreads(n_cores)

  cat(sprintf("\n%s\n", paste(rep("=", 70), collapse = "")))
  cat("SENSITIVITÄTSANALYSE - KOMPLETT-AUSWERTUNG\n")
  cat(sprintf("%s\n\n", paste(rep("=", 70), collapse = "")))

  overall_start <- Sys.time()

  # --- 1. Daten laden ---
  cat("--- 1. DATEN LADEN ---\n")
  dt <- load_sensitivity_data(data_dir, n_cores = n_cores)

  # --- 2. Risikometriken ---
  cat("\n--- 2. RISIKOMETRIKEN ---\n")
  risk <- compute_scenario_risk_metrics(dt, dg_threshold = dg_threshold,
                                        n_cores = n_cores)

  cat(sprintf("\n=== RISIKO-ÜBERSICHT ===\n"))
  cat(sprintf("  Szenarien total:        %d\n", nrow(risk)))
  cat(sprintf("  P(UF) Mittelwert:       %.1f%%\n", mean(risk$prob_underfunding) * 100))
  cat(sprintf("  P(UF) Range:            %.1f%% - %.1f%%\n",
              min(risk$prob_underfunding) * 100, max(risk$prob_underfunding) * 100))
  cat(sprintf("  P(Default) Mittelwert:  %.1f%%\n", mean(risk$prob_default) * 100))
  cat(sprintf("  End-DG Mittelwert:      %.1f%%\n", mean(risk$end_dg_mean) * 100))
  cat(sprintf("  End-DG P5:              %.1f%%\n", mean(risk$end_dg_p5) * 100))

  # --- 3. Grafiken ---
  cat("\n--- 3. GRAFIKEN ---\n")
  plots <- list()

  # 3a. Tornado: P(Unterdeckung)
  cat("  Tornado-Diagramm (Unterdeckung)...\n")
  plots$tornado_uf <- plot_sensitivity_tornado(risk, "prob_underfunding", "P(Unterdeckung)")

  # 3a2. Tornado: P(Default)
  cat("  Tornado-Diagramm (Default)...\n")
  plots$tornado_default <- plot_sensitivity_tornado(risk, "prob_default", "P(Default)")

  # 3a3. Tornado: End-DG
  cat("  Tornado-Diagramm (End-Deckungsgrad)...\n")
  plots$tornado_dg <- plot_sensitivity_tornado(risk, "end_dg_mean", "E[Deckungsgrad]")

  # 3b. Heatmap: Equities vs. Gov Bonds
  cat("  Heatmap (Aktien vs. Gov Bonds)...\n")
  plots$heatmap_eq_gov <- plot_sensitivity_allocation_heatmap(
    risk, "prob_underfunding", "P(Unterdeckung)",
    "weight_equities", "weight_gov_bonds", "Aktien", "Gov Bonds"
  )

  # 3b2. Heatmap: Corp Bonds vs. Real Estate
  cat("  Heatmap (Corp Bonds vs. Immobilien)...\n")
  plots$heatmap_corp_re <- plot_sensitivity_allocation_heatmap(
    risk, "prob_underfunding", "P(Unterdeckung)",
    "weight_corp_bonds", "weight_real_estate", "Corp Bonds", "Immobilien"
  )

  # 3c. Duration-Modi
  cat("  Duration-Modus Vergleich...\n")
  plots$duration_uf <- plot_sensitivity_duration_mode(risk, "prob_underfunding", "P(Unterdeckung)")
  plots$duration_dg <- plot_sensitivity_duration_mode(risk, "end_dg_mean", "E[End-Deckungsgrad]")

  # 3d. Sammelstiftung
  cat("  Sammelstiftung-Effekt...\n")
  plots$ss_uf <- plot_sensitivity_sammelstiftung(risk, "prob_underfunding", "P(Unterdeckung)")
  plots$ss_dg <- plot_sensitivity_sammelstiftung(risk, "end_dg_mean", "E[End-Deckungsgrad]")

  # 3e. Populations-Sensitivität
  cat("  Populations-Sensitivität...\n")
  plots$pop_uf <- plot_sensitivity_population(risk, "prob_underfunding", "P(Unterdeckung)")
  plots$pop_dg <- plot_sensitivity_population(risk, "end_dg_mean", "E[End-Deckungsgrad]")

  # 3f. DG-Fächer nach Duration-Modus
  cat("  DG-Fächer (Duration-Modus)...\n")
  plots$fan_duration <- plot_sensitivity_dg_fan(
    dt, "gov_bond_duration_mode", "Duration-Modus"
  )

  # 3f2. DG-Fächer nach Sammelstiftung
  if ("sammelstiftung_enabled" %in% names(dt)) {
    cat("  DG-Fächer (Sammelstiftung)...\n")
    plots$fan_ss <- plot_sensitivity_dg_fan(
      dt, "sammelstiftung_enabled", "Sammelstiftung"
    )
  }

  # 3g. Interaktionseffekte
  cat("  Interaktionsplots...\n")
  plots$interact_age_dur <- plot_sensitivity_interaction(
    risk, "pop_age_mean", "gov_bond_duration_mode",
    "prob_underfunding", "Durchschnittsalter", "Duration-Modus", "P(Unterdeckung)"
  )

  plots$interact_size_alloc <- plot_sensitivity_interaction(
    risk, "pop_n_population", "weight_equities",
    "prob_underfunding", "Bestandsgrösse", "Aktienquote", "P(Unterdeckung)"
  )

  plots$interact_married_ss <- plot_sensitivity_interaction(
    risk, "pop_share_married", "sammelstiftung_enabled",
    "prob_underfunding", "Anteil Verheiratete", "Sammelstiftung", "P(Unterdeckung)"
  )

  # --- Plots speichern falls gewünscht ---
  if (save_plots) {
    dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
    cat(sprintf("\n--- PLOTS SPEICHERN nach %s ---\n", plot_dir))
    for (pname in names(plots)) {
      if (!is.null(plots[[pname]])) {
        filepath <- file.path(plot_dir, paste0("sens_", pname, ".png"))
        ggsave(filepath, plots[[pname]], width = 12, height = 8, dpi = 150)
        cat(sprintf("  Gespeichert: %s\n", filepath))
      }
    }
  }

  # --- Alle Plots anzeigen ---
  cat("\n--- PLOTS ANZEIGEN ---\n")
  for (pname in names(plots)) {
    if (!is.null(plots[[pname]])) {
      cat(sprintf("  Plot: %s\n", pname))
      print(plots[[pname]])
    }
  }

  total_time <- as.numeric(difftime(Sys.time(), overall_start, units = "secs"))
  cat(sprintf("\n%s\n", paste(rep("=", 70), collapse = "")))
  cat(sprintf("ANALYSE ABGESCHLOSSEN in %.1f Sek.\n", total_time))
  cat(sprintf("%s\n", paste(rep("=", 70), collapse = "")))

  return(list(
    dt    = dt,
    risk  = risk,
    plots = plots
  ))
}

# ==============================================================================
# cashflow_range_table()  &  cashflow_range_plot()
# ------------------------------------------------------------------------------
# Analysiert die SCHWANKUNGSBREITE der Cashflows in Abhängigkeit von vier
# Bestandseigenschaften:
#   1. Grösse           (bestand_n_total)
#   2. Durchschnittsalter (bestand_avg_age)
#   3. Zivilstand        (bestand_share_married)
#   4. Ehegattenrente    (bestand_spouse_pension_rate)
#
# Metriken:
#   CV          = SD / |Mean|              (Variationskoeffizient)
#   Range_rel   = (P95 - P5) / |Median|   (robuste Schwankungsbreite)
#   IQR_rel     = (P75 - P25) / |Median|  (Interquartilsabstand relativ)
#
# TABELLE: Pivotiert – Metriken als Zeilen, Klassen als Spalten
#          Ein Block pro Eigenschaft
#
# PLOT:    Linienplot – Schwankungsbreite über Simulationsjahre,
#          eine Linie pro Klasse, facettiert nach Eigenschaft
#
# Verwendung:
#   result <- cashflow_range_table(df)
#   result$table_gt          # formatierte gt-Tabelle
#   result$table_df          # roher data.frame (für Export)
#   cashflow_range_plot(df)  # Linienplot
# ==============================================================================

# ---- Hilfsfunktion: Klassen-Labels erstellen ---------------------------------
# format_fn: optionale Funktion um Breakpoints zu formatieren (z.B. Tsd. für CHF)
.make_bins <- function(x, n_bins, format_fn = NULL) {
  probs  <- seq(0, 1, length.out = n_bins + 1)
  breaks <- unique(quantile(x, probs = probs, na.rm = TRUE))
  
  if (length(breaks) < 3) {
    # Zu wenige eindeutige Werte → direkte Faktoren mit echten Werten
    vals <- sort(unique(round(x, 3)))
    if (!is.null(format_fn)) {
      return(factor(format_fn(x), levels = format_fn(vals)))
    }
    return(factor(round(x, 3), levels = vals))
  }
  
  # Breaks mit format_fn oder kompakt formatieren
  if (!is.null(format_fn)) {
    # Labels manuell aus formatierten Breaks bauen
    lo <- format_fn(breaks[-length(breaks)])
    hi <- format_fn(breaks[-1])
    labs <- paste0("[", lo, ", ", hi, "]")
    lev <- cut(x, breaks = breaks, include.lowest = TRUE, labels = labs)
  } else {
    lev <- cut(x, breaks = breaks, include.lowest = TRUE, dig.lab = 4)
  }
  return(lev)
}

# Format-Funktionen für spezifische Eigenschaften
.fmt_chf_tsd <- function(x) paste0(round(x / 1000, 0), "k")
.fmt_pct1    <- function(x) paste0(round(x * 100, 0), "%")
.fmt_num1    <- function(x) format(round(x, 1), big.mark = "'")

# Welche format_fn soll für welche Spalte verwendet werden?
.get_format_fn <- function(col) {
  chf_cols  <- c("bestand_pension_mean", "bestand_pension_median",
                 "bestand_total_initial_pension", "bestand_W0")
  pct_cols  <- c("bestand_share_married", "bestand_share_f", "bestand_share_m",
                 "bestand_share_single", "pop_share_married", "pop_share_female",
                 "bestand_spouse_pension_rate", "pop_spouse_pension_rate")
  if (col %in% chf_cols) return(.fmt_chf_tsd)
  if (col %in% pct_cols) return(.fmt_pct1)
  return(NULL)
}

# ---- Hilfsfunktion: Schwankungsmetriken pro Gruppe & Jahr -------------------
# Explizite .data$-Referenz damit dplyr die Spalte in jedem Scope findet
.compute_metrics <- function(df_grp) {
  df_grp %>%
    summarise(
      n_obs      = n(),
      cf_mean    = mean(.data[["cashflow_rent"]],           na.rm = TRUE),
      cf_median  = median(.data[["cashflow_rent"]],         na.rm = TRUE),
      cf_sd      = sd(.data[["cashflow_rent"]],             na.rm = TRUE),
      cf_p5      = quantile(.data[["cashflow_rent"]], 0.05, na.rm = TRUE),
      cf_p25     = quantile(.data[["cashflow_rent"]], 0.25, na.rm = TRUE),
      cf_p75     = quantile(.data[["cashflow_rent"]], 0.75, na.rm = TRUE),
      cf_p95     = quantile(.data[["cashflow_rent"]], 0.95, na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    mutate(
      CV        = cf_sd   / abs(cf_mean),
      Range_rel = (cf_p95 - cf_p5)  / abs(cf_median),
      IQR_rel   = (cf_p75 - cf_p25) / abs(cf_median)
    )
}


# ==============================================================================
# 1. TABELLE
# ==============================================================================

cashflow_range_table <- function(
    df,
    n_bins        = 4,
    print_table   = TRUE
) {
  #' @param df          Data frame mit MC-Pfaden
  #' @param n_bins      Anzahl Quantilklassen pro Eigenschaft (Standard: 4)
  #' @param print_table Tabelle auf Konsole ausgeben?
  #' @return Liste: $table_df, $table_gt
  
  suppressPackageStartupMessages({
    library(dplyr); library(tidyr); library(scales)
  })
  has_gt <- requireNamespace("gt", quietly = TRUE)
  if (has_gt) library(gt)
  
  df <- as.data.frame(df)
  
  # Ehegattenrente: direkte Spalte oder Fallback
  has_spouse <- "bestand_spouse_pension_rate" %in% names(df)
  
  # ---- Eigenschaften definieren ----------------------------------------------
  props <- list(
    list(col = "bestand_n_total",          label = "Grösse (n)"),
    list(col = "bestand_avg_age",          label = "Durchschnittsalter"),
    list(col = "bestand_share_married",    label = "Verheiratetenanteil"),
    list(col = if (has_spouse) "bestand_spouse_pension_rate"
         else            "bestand_share_married",
         label = if (has_spouse) "Ehegattenrente-Rate"
         else            "Ehegattenrente (Proxy: Verheiratetenanteil)"),
    list(col = if ("bestand_spouse_age_diff_mean" %in% names(df))
      "bestand_spouse_age_diff_mean"
      else if ("pop_spouse_age_diff" %in% names(df))
        "pop_spouse_age_diff"
      else NULL,
      label = "Altersunterschied Ehegatte"),
    list(col = if ("pop_share_female" %in% names(df)) "pop_share_female"
         else if ("bestand_share_f" %in% names(df)) "bestand_share_f"
         else NULL,
         label = "Frauenanteil (Population)"),
    list(col = if ("bestand_pension_mean" %in% names(df)) "bestand_pension_mean"
         else NULL,
         label = "Rentenhöhe Ø (CHF/Jahr)")
  )
  props <- Filter(function(p) !is.null(p$col) && p$col %in% names(df), props)
  
  # ---- Pro Eigenschaft: aggregiere ÜBER ALLE JAHRE --------------------------
  # (Schwankung = Streuung der Cashflows über MC-Pfade, nicht über Zeit)
  
  blocks <- lapply(props, function(p) {
    
    col    <- p$col
    fmt_fn <- .get_format_fn(col)
    
    df_bin <- df %>%
      dplyr::filter(!is.na(.data[[col]]), !is.na(.data[["cashflow_rent"]])) %>%
      mutate(Klasse = .make_bins(.data[[col]], n_bins, format_fn = fmt_fn))
    
    # Metriken pro Klasse (über alle Jahre + Pfade)
    raw <- df_bin %>%
      group_by(Klasse) %>%
      .compute_metrics()
    
    # Pivot: Metriken als Zeilen, Klassen als Spalten
    pivot <- raw %>%
      select(Klasse, CV, Range_rel, IQR_rel) %>%
      mutate(across(c(CV, Range_rel, IQR_rel), ~round(. * 100, 2))) %>%  # in %
      pivot_longer(cols = c(CV, Range_rel, IQR_rel),
                   names_to = "Metrik", values_to = "Wert") %>%
      pivot_wider(names_from = Klasse, values_from = Wert) %>%
      mutate(
        Metrik = recode(Metrik,
                        CV        = "CV = SD / |Mean| (%)",
                        Range_rel = "P95-P5 / |Median| (%)",
                        IQR_rel   = "IQR / |Median| (%)"
        ),
        Eigenschaft = p$label
      ) %>%
      select(Eigenschaft, Metrik, everything())
    
    return(pivot)
  })
  
  tbl_df <- bind_rows(blocks)
  
  # ---- Konsolen-Ausgabe ------------------------------------------------------
  if (print_table) {
    cat("\n=== CASHFLOW-SCHWANKUNGSBREITEN NACH BESTANDSEIGENSCHAFTEN ===\n")
    cat("Metriken in %, höher = grössere Schwankung\n\n")
    for (b in blocks) {
      cat(sprintf("── %s ──\n", unique(b$Eigenschaft)))
      print(b %>% select(-Eigenschaft), row.names = FALSE)
      cat("\n")
    }
  }
  
  # ---- gt-Tabelle ------------------------------------------------------------
  tbl_gt <- NULL
  if (has_gt) {
    # Spalten dynamisch ermitteln (Klassennamen variieren je nach Daten)
    klassen_cols <- setdiff(names(tbl_df), c("Eigenschaft", "Metrik"))
    
    tbl_gt <- tbl_df %>%
      gt(groupname_col = "Eigenschaft", rowname_col = "Metrik") %>%
      tab_header(
        title    = md("**Cashflow-Schwankungsbreiten** nach Bestandseigenschaften"),
        subtitle = md(paste0(
          "Alle Metriken in % — höherer Wert = grössere Schwankung über MC-Pfade | ",
          n_bins, " Klassen pro Eigenschaft"))
      ) %>%
      fmt_number(
        columns  = all_of(klassen_cols),
        decimals = 1,
        pattern  = "{x}%"
      ) %>%
      # Heatmap-Farbe pro Zeile (jede Metrik hat eigene Skala)
      data_color(
        columns = all_of(klassen_cols),
        rows    = grepl("CV", Metrik),
        method  = "numeric",
        palette = c("#2166ac", "#f7f7f7", "#d73027"),
        na_color = "white"
      ) %>%
      data_color(
        columns = all_of(klassen_cols),
        rows    = grepl("P95", Metrik),
        method  = "numeric",
        palette = c("#2166ac", "#f7f7f7", "#d73027"),
        na_color = "white"
      ) %>%
      data_color(
        columns = all_of(klassen_cols),
        rows    = grepl("IQR", Metrik),
        method  = "numeric",
        palette = c("#2166ac", "#f7f7f7", "#d73027"),
        na_color = "white"
      ) %>%
      tab_spanner(
        label   = "Klassen (Quantile der Eigenschaft)",
        columns = all_of(klassen_cols)
      ) %>%
      tab_stubhead(label = "Metrik") %>%
      tab_source_note(md(paste0(
        "*CV = SD/|Mean| · P95–P5/|Median| · IQR/|Median| | ",
        "cashflow\\_range\\_table() | ", format(Sys.Date(), "%d.%m.%Y"), "*"
      ))) %>%
      opt_stylize(style = 6, color = "blue") %>%
      opt_table_font(font = list(google_font("Source Sans Pro"), default_fonts())) %>%
      tab_options(
        row_group.font.weight = "bold",
        row_group.background.color = "#e8f0f7",
        stub.font.weight = "bold"
      )
    
    if (print_table) print(tbl_gt)
  } else {
    message("Paket 'gt' nicht installiert → install.packages('gt')")
  }
  
  invisible(list(table_df = tbl_df, table_gt = tbl_gt))
}


# ==============================================================================
# 2. LINIENPLOT – Schwankungsbreite über Simulationsjahre
# ==============================================================================

cashflow_range_plot <- function(
    df,
    metric  = "CV",         # "CV", "Range_rel", "IQR_rel", oder Vektor mit mehreren
    n_bins  = 4,
    palette = "Set2"        # RColorBrewer-Palette
) {
  #' Linienplot der Cashflow-Schwankungsbreite über die 40 Simulationsjahre
  #'
  #' @param df      Data frame mit MC-Pfaden (muss Spalte 'year' enthalten)
  #' @param metric  Welche Metrik(en) plotten: "CV", "Range_rel", "IQR_rel"
  #' @param n_bins  Anzahl Klassen pro Eigenschaft
  #' @param palette RColorBrewer-Palette für Linienfarben
  #' @return ggplot-Objekt (unsichtbar)
  
  suppressPackageStartupMessages({
    library(dplyr); library(tidyr); library(ggplot2); library(scales); library(tibble)
  })
  
  df <- as.data.frame(df)
  has_spouse <- "bestand_spouse_pension_rate" %in% names(df)
  
  # Metrik-Labels
  metric_labels <- c(
    CV        = "CV = SD / |Mean|",
    Range_rel = "P95-P5 / |Median|",
    IQR_rel   = "IQR / |Median|"
  )
  metric <- intersect(metric, names(metric_labels))
  if (length(metric) == 0) stop("Ungültige Metrik. Wähle: CV, Range_rel, IQR_rel")
  
  # ---- Eigenschaften ---------------------------------------------------------
  props <- list(
    list(col = "bestand_n_total",            label = "Grösse (n)"),
    list(col = "bestand_avg_age",            label = "Durchschnittsalter"),
    list(col = "bestand_share_married",      label = "Verheiratetenanteil"),
    list(col = if (has_spouse) "bestand_spouse_pension_rate"
         else            "bestand_share_married",
         label = if (has_spouse) "Ehegattenrente-Rate"
         else            "Ehegattenrente (Proxy)"),
    list(col = if ("bestand_spouse_age_diff_mean" %in% names(df))
      "bestand_spouse_age_diff_mean"
      else if ("pop_spouse_age_diff" %in% names(df))
        "pop_spouse_age_diff"
      else NULL,
      label = "Altersunterschied Ehegatte"),
    list(col = if ("pop_share_female" %in% names(df)) "pop_share_female"
         else if ("bestand_share_f" %in% names(df)) "bestand_share_f"
         else NULL,
         label = "Frauenanteil (Population)"),
    list(col = if ("bestand_pension_mean" %in% names(df)) "bestand_pension_mean"
         else NULL,
         label = "Rentenhöhe Ø (CHF/Jahr)")
  )
  # Eigenschaften ohne vorhandene Spalte entfernen
  props <- Filter(function(p) !is.null(p$col) && p$col %in% names(df), props)
  
  # ---- Pro Eigenschaft: Metriken pro Jahr & Klasse ---------------------------
  all_data <- lapply(props, function(p) {
    col <- p$col
    
    fmt_fn  <- .get_format_fn(col)
    df_bin <- df %>%
      dplyr::filter(!is.na(.data[[col]]), !is.na(.data[["cashflow_rent"]]), !is.na(.data[["year"]])) %>%
      mutate(Klasse = as.character(.make_bins(.data[[col]], n_bins, format_fn = fmt_fn)))
    
    df_bin %>%
      group_by(year, Klasse) %>%
      .compute_metrics() %>%
      mutate(
        Eigenschaft = p$label,
        # Rang innerhalb dieser Eigenschaft (1 = kleinste Klasse)
        Klasse_rang = as.integer(factor(Klasse, levels = sort(unique(as.character(Klasse)))))
      )
  }) %>%
    bind_rows()
  
  # ---- In Long-Format für ggplot ---------------------------------------------
  plot_data <- all_data %>%
    select(Eigenschaft, Klasse, Klasse_rang, year, all_of(metric)) %>%
    pivot_longer(cols = all_of(metric),
                 names_to = "Metrik", values_to = "Wert") %>%
    mutate(
      Metrik_label = metric_labels[Metrik],
      Klasse       = factor(Klasse, levels = unique(Klasse))
    )
  
  # ---- Farbpalette -----------------------------------------------------------
  # Klassen-Labels unterscheiden sich pro Eigenschaft (z.B. "[50,200]" vs "[65,70]").
  # Wir färben nach dem Rang (1 = kleinste Klasse) einheitlich über alle Facets.
  basis_farben <- if (requireNamespace("RColorBrewer", quietly = TRUE)) {
    RColorBrewer::brewer.pal(min(8, max(3, n_bins)), palette)[seq_len(min(n_bins, 8))]
  } else {
    scales::hue_pal()(n_bins)
  }
  
  farben_named <- plot_data %>%
    distinct(Klasse, Klasse_rang) %>%
    mutate(farbe = basis_farben[pmin(Klasse_rang, length(basis_farben))]) %>%
    select(Klasse, farbe) %>%
    tibble::deframe()
  
  # ---- Endpunkt-Labels: letzter nicht-NA Wert pro Linie & Facet -------------
  label_data <- plot_data %>%
    group_by(Eigenschaft, Metrik, Klasse) %>%
    dplyr::filter(!is.na(Wert)) %>%
    slice_max(year, n = 1) %>%
    ungroup() %>%
    # Vertikalen Versatz berechnen um Überlappungen zu minimieren:
    # Linien mit ähnlichem Endwert innerhalb eines Facets werden gestaffelt.
    group_by(Eigenschaft, Metrik) %>%
    arrange(Wert, .by_group = TRUE) %>%
    mutate(
      rang_in_facet = row_number(),
      n_in_facet    = n(),
      # Y-Nudge: kleiner gleichmässiger Versatz basierend auf Rang im Facet
      # Skaliert mit der Wert-Spannweite damit es proportional bleibt
      y_spread   = max(Wert, na.rm = TRUE) - min(Wert, na.rm = TRUE),
      y_nudge    = if_else(
        n_in_facet > 1 & y_spread < 0.05,   # Linien liegen sehr nah beieinander
        (rang_in_facet - median(seq_len(n_in_facet[1]))) * 0.015,
        0
      )
    ) %>%
    ungroup()
  
  use_repel <- requireNamespace("ggrepel", quietly = TRUE)
  
  x_max   <- max(plot_data$year, na.rm = TRUE)
  x_break <- sort(unique(c(1, 5, 10, 20, 30, x_max)))
  
  # ---- Plot ------------------------------------------------------------------
  n_metrics <- length(metric)
  
  p <- ggplot(plot_data,
              aes(x = year, y = Wert, color = Klasse, group = Klasse)) +
    geom_line(linewidth = 0.9, alpha = 0.85) +
    geom_point(size = 1.0, alpha = 0.5) +
    {
      if (use_repel) {
        # ggrepel: automatisches Anti-Overlap, Labels rechts der Linien
        ggrepel::geom_text_repel(
          data            = label_data,
          aes(label       = Klasse, y = Wert + y_nudge),
          hjust           = 0,
          direction       = "y",
          nudge_x         = 1.5,
          segment.size    = 0.3,
          segment.alpha   = 0.5,
          segment.linetype = "dotted",
          box.padding     = 0.15,
          point.padding   = 0.1,
          force           = 0.8,
          force_pull      = 0.5,
          max.overlaps    = Inf,
          size            = 2.8,
          fontface        = "bold",
          show.legend     = FALSE
        )
      } else {
        # Fallback ohne ggrepel: manueller Y-Versatz
        geom_text(
          data        = label_data,
          aes(label   = Klasse, y = Wert + y_nudge),
          hjust       = -0.08,
          size        = 2.8,
          fontface    = "bold",
          show.legend = FALSE
        )
      }
    } +
    {
      if (n_metrics > 1)
        facet_grid(Eigenschaft ~ Metrik_label, scales = "free_y",
                   labeller = labeller(Metrik_label = label_value))
      else
        facet_wrap(~ Eigenschaft, ncol = 4, scales = "free_y")
    } +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    scale_x_continuous(
      breaks = x_break,
      expand = expansion(mult = c(0.02, 0.30))
    ) +
    scale_color_manual(values = farben_named) +
    guides(color = "none") +
    labs(
      title    = "Cashflow-Schwankungsbreite über Simulationsjahre",
      subtitle = paste0(
        "Metrik: ", paste(metric_labels[metric], collapse = " | "),
        " — ", n_bins, " Klassen pro Eigenschaft"
      ),
      x = "Simulationsjahr",
      y = "Schwankungsbreite (relativ)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text       = element_text(face = "bold", size = 9),
      strip.background = element_rect(fill = "#e8f0f7", color = NA),
      legend.position  = "none",
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold"),
      plot.subtitle    = element_text(color = "grey40"),
      panel.spacing    = unit(1.0, "lines")
    )
  
  print(p)
  invisible(p)
}


# ==============================================================================
# Verwendungsbeispiele
# ==============================================================================
#
# # Tabelle (pivotiert, alle drei Metriken):
# result <- cashflow_range_table(df, n_bins = 4)
# result$table_df          # roher data.frame
# result$table_gt          # formatiertes gt-Objekt
#
# # gt-Tabelle als HTML speichern:
# gt::gtsave(result$table_gt, "schwankungsbreiten_tabelle.html")
#
# # Linienplot mit einer Metrik:
# cashflow_range_plot(df, metric = "CV")
#
# # Linienplot mit allen drei Metriken (Grid):
# cashflow_range_plot(df, metric = c("CV", "Range_rel", "IQR_rel"))
#
# # Andere Klassenzahl oder Palette:
# cashflow_range_plot(df, metric = "Range_rel", n_bins = 3, palette = "Dark2")


# ==============================================================================
# cashflow_hypothesis_test()
# ------------------------------------------------------------------------------
# Testet für jede der 7 Bestandseigenschaften die Hypothese:
#
#   H0: Die Eigenschaft hat KEINEN Einfluss auf die Cashflow-Schwankungsbreite
#   H1: Die Eigenschaft hat einen Einfluss auf die Cashflow-Schwankungsbreite
#
# EINHEIT DER ANALYSE: CV pro Simulationspfad
#   - CV(Pfad) = SD(cashflow_rent über Jahre) / |Mean(cashflow_rent über Jahre)|
#   - Dies vermeidet das Autokorrelationsproblem (Jahre innerhalb eines Pfades
#     sind keine unabhängigen Beobachtungen)
#   - Jeder Pfad liefert eine unabhängige Beobachtung der Schwankungsbreite
#
# TESTS:
#   1. Kruskal-Wallis-Test (nichtparametrisch, robust gegen Schiefe der CV-Verteilung)
#   2. Post-hoc Dunn-Test mit Bonferroni-Korrektur (paarweise Gruppenvergleiche)
#   3. Effektgrösse η² (Eta-Quadrat) = (H - k + 1) / (n - k)
#
# EIGENSCHAFTEN (Spalten im df):
#   bestand_n_total           Bestandsgrösse
#   bestand_avg_age           Durchschnittsalter
#   bestand_share_married     Verheiratetenanteil
#   bestand_spouse_pension_rate Ehegattenrente-Rate
#   bestand_spouse_age_diff_mean Altersunterschied Ehegatte
#   pop_share_female          Frauenanteil (Population)
#   bestand_pension_mean      Rentenhöhe Ø
#
# Verwendung:
#   result <- cashflow_hypothesis_test(df)
#   result$summary          # Übersichtstabelle aller 7 Tests
#   result$posthoc          # Named list: Dunn-Tests pro Eigenschaft
#   result$plots$overview   # Forest-Plot der Effektgrössen
#   result$plots$boxplots   # Boxplots CV nach Klasse, facettiert
# ==============================================================================

cashflow_hypothesis_test <- function(
    df,
    n_bins          = 4,         # Klassen pro Eigenschaft (für Kruskal-Wallis)
    alpha           = 0.05,      # Signifikanzniveau
    p_adjust_method = "bonferroni",  # Korrektur für Post-hoc: "bonferroni", "holm", "BH"
    min_n_per_group = 10,        # Mindest-Beobachtungen pro Gruppe für validen Test
    print_results   = TRUE
) {
  #' @param df              Data frame mit MC-Pfaden (path_nr, year, cashflow_rent,
  #'                        plus Bestandseigenschaften)
  #' @param n_bins          Anzahl Quantilklassen pro Eigenschaft
  #' @param alpha           Signifikanzniveau (Default: 0.05)
  #' @param p_adjust_method Methode für Mehrfachtestkorrektur
  #' @param min_n_per_group Mindest-N pro Gruppe; Gruppen darunter werden gewarnt
  #' @param print_results   Ergebnisse auf Konsole ausgeben?
  #' @return Liste: $summary, $posthoc, $plots

  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(scales)
  })

  has_dunn   <- requireNamespace("dunn.test",  quietly = TRUE)
  has_rstatix <- requireNamespace("rstatix",   quietly = TRUE)
  has_gt     <- requireNamespace("gt",         quietly = TRUE)

  if (!has_dunn && !has_rstatix) {
    message(
      "Hinweis: Kein Post-hoc-Paket gefunden.\n",
      "  Installiere eines der folgenden:\n",
      "    install.packages('dunn.test')   # empfohlen\n",
      "    install.packages('rstatix')     # Alternative\n",
      "  Kruskal-Wallis-Tests werden trotzdem durchgeführt."
    )
  }

  df <- as.data.frame(df)

  # ---- Bestandseigenschaften definieren --------------------------------------
  has_spouse     <- "bestand_spouse_pension_rate"   %in% names(df)
  has_age_diff   <- "bestand_spouse_age_diff_mean"  %in% names(df)
  has_pop_female <- "pop_share_female"              %in% names(df)
  has_pension    <- "bestand_pension_mean"          %in% names(df)

  properties <- list(
    list(col = "bestand_n_total",
         label = "Bestandsgrösse (n)",
         unit  = "Personen"),
    list(col = "bestand_avg_age",
         label = "Durchschnittsalter",
         unit  = "Jahre"),
    list(col = "bestand_share_married",
         label = "Verheiratetenanteil",
         unit  = "%"),
    list(col = if (has_spouse) "bestand_spouse_pension_rate" else NULL,
         label = "Ehegattenrente-Rate",
         unit  = "% der Rente"),
    list(col = if (has_age_diff) "bestand_spouse_age_diff_mean"
               else if ("pop_spouse_age_diff" %in% names(df)) "pop_spouse_age_diff"
               else NULL,
         label = "Altersunterschied Ehegatte",
         unit  = "Jahre"),
    list(col = if (has_pop_female) "pop_share_female"
               else if ("bestand_share_f" %in% names(df)) "bestand_share_f"
               else NULL,
         label = "Frauenanteil (Population)",
         unit  = "%"),
    list(col = if (has_pension) "bestand_pension_mean" else NULL,
         label = "Rentenhöhe Ø",
         unit  = "CHF/Jahr")
  )
  # Eigenschaften ohne vorhandene Spalte entfernen
  properties <- Filter(function(p) !is.null(p$col) && p$col %in% names(df), properties)

  # ---- Schritt 1: CV pro Simulationspfad berechnen ---------------------------
  # Aggregation über alle Simulationsjahre: ein CV-Wert pro Pfad
  # Bestandseigenschaften sind innerhalb eines Pfades konstant → 1. Zeile genügt
  prop_cols <- unique(sapply(properties, `[[`, "col"))

  # Pfad-ID: falls globale path_nr nicht vorhanden, source_file + path_nr
  if (!"path_nr" %in% names(df)) {
    stop("Spalte 'path_nr' nicht gefunden. Bitte Daten mit load_*() laden.")
  }
  # Eindeutigen Pfad-Schlüssel bauen (source_file falls vorhanden)
  if ("source_file" %in% names(df)) {
    df <- df %>% mutate(.path_key = paste(source_file, path_nr, sep = "__"))
  } else {
    df <- df %>% mutate(.path_key = as.character(path_nr))
  }

  # CV pro Pfad: SD(cashflow) / |Mean(cashflow)| über alle Jahre
  path_cv <- df %>%
    dplyr::filter(!is.na(.data[["cashflow_rent"]])) %>%
    dplyr::group_by(.data[[".path_key"]]) %>%
    dplyr::summarise(
      cf_mean = mean(.data[["cashflow_rent"]], na.rm = TRUE),
      cf_sd   = sd(.data[["cashflow_rent"]],   na.rm = TRUE),
      cv      = cf_sd / abs(cf_mean),
      n_years = n(),
      # Bestandseigenschaften: konstant pro Pfad, erste Zeile nehmen
      across(all_of(prop_cols), ~ first(na.omit(.))),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(cv), is.finite(cv), n_years >= 5)

  n_paths_total <- nrow(path_cv)
  if (n_paths_total < 30) {
    warning("Nur ", n_paths_total, " Pfade verfügbar. Testergebnisse sind wenig aussagekräftig.")
  }

  # ---- Schritt 2: Tests pro Eigenschaft --------------------------------------
  results_list <- vector("list", length(properties))
  posthoc_list <- vector("list", length(properties))
  plot_df_list <- vector("list", length(properties))

  for (i in seq_along(properties)) {
    prop  <- properties[[i]]
    col   <- prop$col
    label <- prop$label

    # Klassen bilden (Quantilbins)
    x_vals <- path_cv[[col]]
    breaks <- unique(quantile(x_vals, probs = seq(0, 1, length.out = n_bins + 1),
                              na.rm = TRUE))

    if (length(breaks) >= 3) {
      path_cv$.grp <- cut(x_vals, breaks = breaks, include.lowest = TRUE, dig.lab = 4)
    } else {
      # Wenige eindeutige Werte → direkte Faktorisierung
      path_cv$.grp <- factor(round(x_vals, 3))
    }

    # Gruppen mit zu wenig Beobachtungen markieren
    grp_counts <- table(path_cv$.grp)
    small_grps <- names(grp_counts)[grp_counts < min_n_per_group]
    if (length(small_grps) > 0) {
      warning(sprintf(
        "[%s] %d Gruppe(n) mit < %d Beobachtungen: %s",
        label, length(small_grps), min_n_per_group,
        paste(small_grps, collapse = ", ")
      ))
    }

    # Gruppen mit mind. 2 Klassen vorhanden?
    valid_grps <- names(grp_counts)[grp_counts >= 2]
    if (length(valid_grps) < 2) {
      results_list[[i]] <- tibble(
        Eigenschaft = label, n_pfade = n_paths_total,
        n_gruppen = length(grp_counts), H_stat = NA, df_kw = NA,
        p_value = NA, p_adj = NA, eta_sq = NA,
        signifikant = NA, effekt = "nicht testbar"
      )
      next
    }

    # ---- Kruskal-Wallis-Test ----
    kw <- kruskal.test(cv ~ .grp, data = path_cv)

    # Effektgrösse η²: (H - k + 1) / (n - k)
    # H = KW-Statistik, k = Anzahl Gruppen, n = Gesamtstichprobe
    k   <- length(unique(na.omit(path_cv$.grp)))
    n   <- sum(!is.na(path_cv$.grp) & !is.na(path_cv$cv))
    eta_sq <- max(0, (kw$statistic - k + 1) / (n - k))

    # Effektstärke nach Cohen (für H-basiertes η²)
    effekt_label <- dplyr::case_when(
      eta_sq < 0.01  ~ "vernachlässigbar",
      eta_sq < 0.06  ~ "klein",
      eta_sq < 0.14  ~ "mittel",
      TRUE           ~ "gross"
    )

    results_list[[i]] <- tibble(
      Eigenschaft = label,
      Einheit     = prop$unit,
      n_pfade     = n,
      n_gruppen   = k,
      H_stat      = round(kw$statistic, 3),
      df_kw       = kw$parameter,
      p_value     = kw$p.value,
      eta_sq      = round(eta_sq, 4),
      effekt      = effekt_label,
      signifikant = kw$p.value < alpha
    )

    # ---- Post-hoc Dunn-Test ------------------------------------------------
    ph <- NULL
    if (has_rstatix) {
      ph_raw <- rstatix::dunn_test(
        path_cv %>% dplyr::filter(!is.na(.grp), !is.na(cv)),
        cv ~ .grp,
        p.adjust.method = p_adjust_method
      )
      ph <- ph_raw %>%
        select(group1, group2, statistic, p, p.adj, p.adj.signif) %>%
        mutate(Eigenschaft = label)
    } else if (has_dunn) {
      ph_raw <- dunn.test::dunn.test(
        x = path_cv$cv[!is.na(path_cv$.grp) & !is.na(path_cv$cv)],
        g = path_cv$.grp[!is.na(path_cv$.grp) & !is.na(path_cv$cv)],
        method = p_adjust_method, alpha = alpha, kw = FALSE, label = TRUE,
        wrap = FALSE, table = FALSE, list = FALSE, rmc = FALSE, altp = FALSE
      )
      ph <- tibble(
        Eigenschaft = label,
        group1      = sub(" - .*", "",  ph_raw$comparisons),
        group2      = sub(".* - ", "",  ph_raw$comparisons),
        statistic   = round(ph_raw$Z,   3),
        p           = ph_raw$P,
        p.adj       = ph_raw$P.adjusted,
        p.adj.signif = dplyr::case_when(
          ph_raw$P.adjusted < 0.001 ~ "***",
          ph_raw$P.adjusted < 0.01  ~ "**",
          ph_raw$P.adjusted < 0.05  ~ "*",
          ph_raw$P.adjusted < 0.10  ~ ".",
          TRUE                       ~ "ns"
        )
      )
    }
    posthoc_list[[i]] <- ph
    names(posthoc_list)[i] <- label

    # ---- Daten für Boxplot-Facet -------------------------------------------
    plot_df_list[[i]] <- path_cv %>%
      dplyr::filter(!is.na(.grp), !is.na(cv)) %>%
      mutate(Eigenschaft = label,
             Klasse      = as.character(.grp))
  }

  # ---- Schritt 3: Ergebnistabelle zusammenfassen ----------------------------
  summary_df <- bind_rows(results_list) %>%
    mutate(
      p_value_fmt = dplyr::case_when(
        is.na(p_value)    ~ "—",
        p_value < 0.001   ~ "< 0.001",
        p_value < 0.01    ~ sprintf("%.3f", p_value),
        TRUE              ~ sprintf("%.3f", p_value)
      ),
      sig_symbol = dplyr::case_when(
        is.na(p_value)    ~ "—",
        p_value < 0.001   ~ "***",
        p_value < 0.01    ~ "**",
        p_value < 0.05    ~ "*",
        p_value < 0.10    ~ ".",
        TRUE              ~ "ns"
      )
    )

  # ---- Schritt 4: Plots -----------------------------------------------------
  plots <- list()

  ## 4a. Forest-Plot: Effektgrösse η² pro Eigenschaft
  forest_df <- summary_df %>%
    dplyr::filter(!is.na(eta_sq)) %>%
    mutate(
      Eigenschaft = factor(Eigenschaft, levels = rev(Eigenschaft)),
      sig_col     = if_else(signifikant, "signifikant (p < .05)", "nicht signifikant")
    )

  # Schwellenwert-Linien als separate Daten (kompatibel mit diskreter Y-Achse)
  vline_df <- data.frame(
    xintercept = c(0.01, 0.06, 0.14),
    label      = c("klein", "mittel", "gross")
  )
  # Label-Position: oberste Eigenschaft im Faktor (= letzte Zeile = oben im Plot)
  top_level <- levels(forest_df$Eigenschaft)[length(levels(forest_df$Eigenschaft))]

  plots$overview <- ggplot(forest_df,
                           aes(x = eta_sq, y = Eigenschaft, color = sig_col)) +
    geom_vline(data = vline_df, aes(xintercept = xintercept),
               linetype = "dashed", color = "grey70", linewidth = 0.4,
               inherit.aes = FALSE) +
    geom_label(data = vline_df,
               aes(x = xintercept, y = top_level, label = label),
               inherit.aes = FALSE,
               vjust = -0.4, hjust = -0.05,
               size = 2.6, color = "grey50",
               linewidth = 0, fill = "white", alpha = 0.8) +
    geom_segment(aes(x = 0, xend = eta_sq, yend = Eigenschaft),
                 linewidth = 1.2, alpha = 0.7) +
    geom_point(aes(size = H_stat), alpha = 0.9) +
    geom_text(aes(label = paste0(sig_symbol, "  η²=", sprintf("%.3f", eta_sq))),
              hjust = -0.15, size = 3, color = "grey30") +
    scale_color_manual(
      values = c("signifikant (p < .05)" = "#1a6faf", "nicht signifikant" = "#b0b0b0"),
      name   = NULL
    ) +
    scale_size_continuous(name = "KW H-Statistik", range = c(3, 8)) +
    scale_x_continuous(
      limits = c(0, max(forest_df$eta_sq, na.rm = TRUE) * 1.5),
      labels = number_format(accuracy = 0.001),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_y_discrete() +
    labs(
      title    = "Einfluss der Bestandseigenschaften auf Cashflow-Schwankungsbreite",
      subtitle = paste0(
        "Kruskal-Wallis-Test | Effektgrösse η² | n = ", n_paths_total, " Pfade | ",
        n_bins, " Klassen pro Eigenschaft | α = ", alpha
      ),
      x = "Effektgrösse η² (Eta-Quadrat)",
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position    = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(face = "bold"),
      plot.subtitle      = element_text(color = "grey40", size = 9)
    )

  ## 4b. Boxplots: CV-Verteilung nach Klasse, facettiert nach Eigenschaft
  plot_df <- bind_rows(plot_df_list)

  if (nrow(plot_df) > 0) {
    # Eigenschaft-Reihenfolge nach η² (absteigend)
    prop_order <- summary_df %>%
      dplyr::filter(!is.na(eta_sq)) %>%
      arrange(desc(eta_sq)) %>%
      pull(Eigenschaft)

    plot_df <- plot_df %>%
      mutate(Eigenschaft = factor(Eigenschaft, levels = prop_order))

    # p-Wert-Labels für Facet-Titel
    kw_labels <- summary_df %>%
      mutate(facet_label = paste0(
        Eigenschaft, "\n",
        "KW: H=", sprintf("%.1f", H_stat), ", ",
        "p", ifelse(p_value < 0.001, "<0.001", paste0("=", sprintf("%.3f", p_value))),
        " ", sig_symbol, " | η²=", sprintf("%.3f", eta_sq)
      )) %>%
      select(Eigenschaft, facet_label)

    plot_df <- plot_df %>%
      left_join(kw_labels, by = "Eigenschaft") %>%
      # Rang (1 = kleinste Klasse) innerhalb jeder Eigenschaft bestimmen –
      # gleicher Rang = gleiche Farbe über alle Facets, nur n_bins Farben nötig
      dplyr::group_by(Eigenschaft) %>%
      dplyr::mutate(
        Klasse_rang = as.integer(factor(Klasse, levels = sort(unique(Klasse))))
      ) %>%
      dplyr::ungroup()

    # Farbpalette: genau n_bins Farben, nach Rang vergeben
    basis_farben_bp <- RColorBrewer::brewer.pal(min(8, max(3, n_bins)), "Set2")[seq_len(min(n_bins, 8))]
    rang_farben_bp  <- plot_df %>%
      dplyr::distinct(Klasse, Klasse_rang) %>%
      dplyr::mutate(farbe = basis_farben_bp[pmin(Klasse_rang, length(basis_farben_bp))]) %>%
      tibble::deframe()

    plots$boxplots <- ggplot(plot_df,
                             aes(x = Klasse, y = cv, fill = Klasse)) +
      geom_boxplot(outlier.size = 0.6, outlier.alpha = 0.4,
                   alpha = 0.75, linewidth = 0.4) +
      geom_jitter(width = 0.15, size = 0.4, alpha = 0.2, color = "grey30") +
      facet_wrap(~ facet_label, scales = "free_x", ncol = 4) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      scale_fill_manual(values = rang_farben_bp) +
      guides(fill = "none") +
      labs(
        title    = "Cashflow-CV nach Bestandseigenschaft und Klasse",
        subtitle = paste0(
          "CV = SD(cashflow) / |Mean(cashflow)| pro Simulationspfad | ",
          "Sortiert nach Effektgrösse η² (absteigend)"
        ),
        x = "Klasse (Quantilbins der Eigenschaft)",
        y = "CV (Cashflow-Variationskoeffizient)"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        strip.text        = element_text(face = "bold", size = 7.5),
        strip.background  = element_rect(fill = "#e8f0f7", color = NA),
        axis.text.x       = element_text(angle = 25, hjust = 1, size = 7),
        panel.grid.minor  = element_blank(),
        plot.title        = element_text(face = "bold"),
        plot.subtitle     = element_text(color = "grey40", size = 9)
      )
  }

  # ---- Schritt 5: Konsolen-Ausgabe ------------------------------------------
  if (print_results) {
    cat("\n")
    cat("══════════════════════════════════════════════════════════════════════\n")
    cat(" HYPOTHESENTEST: Einfluss der Bestandseigenschaften auf Cashflow-CV\n")
    cat("══════════════════════════════════════════════════════════════════════\n")
    cat(sprintf(" Test:       Kruskal-Wallis (nichtparametrisch)\n"))
    cat(sprintf(" Post-hoc:   Dunn-Test (%s-Korrektur)\n", p_adjust_method))
    cat(sprintf(" n (Pfade):  %d | Klassen: %d | α = %.2f\n\n",
                n_paths_total, n_bins, alpha))

    out <- summary_df %>%
      mutate(
        `η²`      = sprintf("%.4f", eta_sq),
        Effekt    = effekt,
        `H (df)`  = sprintf("%.1f (%d)", H_stat, df_kw),
        p         = p_value_fmt,
        Sig       = sig_symbol
      ) %>%
      select(Eigenschaft, `n Pfade` = n_pfade, `Gruppen` = n_gruppen,
             `H (df)`, p, Sig, `η²`, Effekt)

    print(as.data.frame(out), row.names = FALSE)

    # Post-hoc Zusammenfassung
    cat("\n── Post-hoc (Dunn-Test, nur signifikante Paare) ──────────────────────\n")
    for (nm in names(posthoc_list)) {
      ph <- posthoc_list[[nm]]
      if (is.null(ph)) next
      sig_pairs <- ph %>% dplyr::filter(p.adj < alpha)
      if (nrow(sig_pairs) == 0) {
        cat(sprintf("  %-35s keine sign. Paare\n", nm))
      } else {
        cat(sprintf("  %s  (%d sign. Paare von %d):\n", nm, nrow(sig_pairs), nrow(ph)))
        for (j in seq_len(min(nrow(sig_pairs), 5))) {
          cat(sprintf("    %s vs %s  →  p.adj=%s %s\n",
                      sig_pairs$group1[j], sig_pairs$group2[j],
                      sprintf("%.4f", sig_pairs$p.adj[j]),
                      sig_pairs$p.adj.signif[j]))
        }
        if (nrow(sig_pairs) > 5)
          cat(sprintf("    ... und %d weitere Paare\n", nrow(sig_pairs) - 5))
      }
    }
    cat("\n Signifikanzcodes: *** p<.001  ** p<.01  * p<.05  . p<.10  ns p≥.10\n")
    cat(" Effektgrösse η²:  <.01 vernachlässigbar | .01-.06 klein | .06-.14 mittel | >.14 gross\n")
    cat("══════════════════════════════════════════════════════════════════════\n\n")
  }

  # ---- Plots ausgeben -------------------------------------------------------
  if (!is.null(plots$overview))  print(plots$overview)
  if (!is.null(plots$boxplots))  print(plots$boxplots)

  # ---- gt-Tabelle (optional) ------------------------------------------------
  tbl_gt <- NULL
  if (has_gt) {
    library(gt)
    tbl_gt <- summary_df %>%
      select(Eigenschaft, Einheit, n_pfade, n_gruppen,
             H_stat, df_kw, p_value_fmt, sig_symbol, eta_sq, effekt) %>%
      gt() %>%
      tab_header(
        title    = md("**Kruskal-Wallis-Test**: Einfluss auf Cashflow-Schwankungsbreite"),
        subtitle = md(paste0(
          "H₀: Eigenschaft hat *keinen* Einfluss auf CV(Cashflow) | ",
          "n = ", n_paths_total, " Pfade | α = ", alpha
        ))
      ) %>%
      cols_label(
        Eigenschaft  = "Eigenschaft",
        Einheit      = "Einheit",
        n_pfade      = "n Pfade",
        n_gruppen    = "Gruppen",
        H_stat       = "H-Statistik",
        df_kw        = "df",
        p_value_fmt  = "p-Wert",
        sig_symbol   = "",
        eta_sq       = md("η²"),
        effekt       = "Effekt"
      ) %>%
      fmt_number(columns = H_stat, decimals = 2) %>%
      fmt_number(columns = eta_sq, decimals = 4) %>%
      data_color(
        columns = eta_sq,
        method  = "numeric",
        palette = c("#f7fbff", "#2171b5"),
        domain  = c(0, max(summary_df$eta_sq, na.rm = TRUE))
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(
          columns = sig_symbol,
          rows    = p_value_fmt != "ns" & !is.na(eta_sq)
        )
      ) %>%
      tab_footnote(
        footnote = md(
          "η² < .01 vernachlässigbar | .01–.06 klein | .06–.14 mittel | > .14 gross"
        )
      ) %>%
      tab_footnote(
        footnote = md(paste0(
          "Post-hoc: Dunn-Test mit ", p_adjust_method, "-Korrektur. ",
          "Pakete: dunn.test oder rstatix."
        ))
      ) %>%
      opt_stylize(style = 6, color = "blue") %>%
      opt_table_font(font = list(google_font("Source Sans Pro"), default_fonts()))
  }

  invisible(list(
    summary   = summary_df,
    posthoc   = posthoc_list,
    path_cv   = path_cv,
    plots     = plots,
    table_gt  = tbl_gt
  ))
}


# ==============================================================================
# Verwendungsbeispiele
# ==============================================================================
#
# # Vollständiger Test mit Default-Einstellungen:
# result <- cashflow_hypothesis_test(df)
#
# # Übersichtstabelle:
# result$summary
#
# # gt-Tabelle als HTML speichern:
# gt::gtsave(result$table_gt, "hypothesentest.html")
#
# # Nur Post-hoc für eine bestimmte Eigenschaft:
# result$posthoc[["Bestandsgrösse (n)"]]
#
# # Plots einzeln aufrufen:
# result$plots$overview   # Forest-Plot
# result$plots$boxplots   # Boxplots
#
# # Andere Klassenzahl oder strengeres Signifikanzniveau:
# result <- cashflow_hypothesis_test(df, n_bins = 5, alpha = 0.01)
#
# # Holm-Korrektur (weniger konservativ als Bonferroni):
# result <- cashflow_hypothesis_test(df, p_adjust_method = "holm")

# ============================================================================
# ENDE DER FUNKTIONSDATEI
# ============================================================================
# Letzte Änderung: 2026-31-05
# Status: Bereit für OneDrive-Synchronisation
# ============================================================================
