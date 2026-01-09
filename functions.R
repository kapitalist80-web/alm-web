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
  
  # --- PLOT 9: Multivariate Übersicht (Scatter Matrix Style) ---
  plots$scatter_overview <- stats_by_config %>%
    select(cv_cashflow, bestand_n_total, bestand_avg_age, 
           bestand_share_f, bestand_share_married, pension_cv) %>%
    pivot_longer(-cv_cashflow, names_to = "variable", values_to = "value") %>%
    mutate(variable = recode(variable,
                             bestand_n_total = "Bestandsgrösse",
                             bestand_avg_age = "Durchschnittsalter",
                             bestand_share_f = "Frauenanteil",
                             bestand_share_married = "Verheiratetenanteil",
                             pension_cv = "Pensions-Heterogenität")) %>%
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
  
  cor_long <- cor_data %>%
    as.data.frame() %>%
    rownames_to_column("var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "correlation")
  
  plots$correlation <- cor_long %>%
    mutate(
      var1 = recode(var1, cv_cashflow = "CV", bestand_n_total = "Grösse",
                    bestand_avg_age = "Alter", bestand_share_f = "Frauen",
                    bestand_share_married = "Verheiratet", 
                    bestand_pension_mean = "Pension Ø", pension_cv = "Pension CV"),
      var2 = recode(var2, cv_cashflow = "CV", bestand_n_total = "Grösse",
                    bestand_avg_age = "Alter", bestand_share_f = "Frauen",
                    bestand_share_married = "Verheiratet",
                    bestand_pension_mean = "Pension Ø", pension_cv = "Pension CV")
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
# ENDE DER FUNKTIONSDATEI
# ============================================================================
# Letzte Änderung: 2025-01-15
# Status: Bereit für OneDrive-Synchronisation
# ============================================================================
