import pandas as pd
import numpy as np

# Zufallsgenerator initialisieren
np.random.seed(42) 

N_POPULATION = 50  # Anzahl Rentner

# Aktuelles Jahr für die Geburtsjahrberechnung
CURRENT_YEAR = 2024

# 1. Alter (Alter im aktuellen Jahr)
# Normalverteilung um 78 Jahre, Standardabweichung 8, Grenzen 65 bis 100
age_mean = 69
age_std = 8
min_age = 65
max_age = 100
ages = np.round(np.clip(np.random.normal(age_mean, age_std, N_POPULATION), min_age, max_age)).astype(int)

# 2. Geschlecht (ca. 55% weiblich)
genders = np.random.choice(['F', 'M'], size=N_POPULATION, p=[0.55, 0.45])

# 3. Zivilstand (ca. 60% verheiratet oder verwitwet)
# Verheiratete: Berücksichtigen das Risiko der Hinterbliebenenrente
# Verwitwete: Stellen bereits Hinterbliebenenrenten dar
marital_status_choices = ['Married', 'Single', 'Widowed']
# Annahme: 50% verheiratet (mit vollem Risiko), 10% verwitwet (bereits geringere Rente), 40% ledig
marital_statuses = np.random.choice(marital_status_choices, size=N_POPULATION, p=[0.50, 0.40, 0.10])

# 4. Rentenhöhe
# InitialPension (Volle Rente)
pension_mean = 30000 
pension_std = 10000
pensions = np.round(np.clip(np.random.normal(pension_mean, pension_std, N_POPULATION), 10000, 80000), -2)

# 5. Ehegatten-Altersdifferenz (SpouseAgeDiff)
# Normalverteilung: Mittelwert und Standardabweichung konfigurierbar
# Konvention: negativ = Partner jünger, positiv = Partner älter
spouse_age_diff_mean = -3  # Mittelwert: Partner 3 Jahre jünger
spouse_age_diff_std = 2     # Standardabweichung
# Für Männer: Partner typisch jünger (negativer Wert), für Frauen: Partner typisch älter (positiver Wert)
spouse_age_diffs = np.round(np.where(
    genders == 'M',
    np.random.normal(spouse_age_diff_mean, spouse_age_diff_std, N_POPULATION),
    np.random.normal(-spouse_age_diff_mean, spouse_age_diff_std, N_POPULATION)
)).astype(int)

# 6. Ehegattenrente (SpousePensionRate)
# Anteil der Rente, den der überlebende Ehegatte erhält
spouse_pension_rate = 0.40  # 40%

# Berechnung des Geburtsjahres
birth_years = CURRENT_YEAR - ages

# Erstellen des DataFrame
population_df = pd.DataFrame({
    'ID': np.arange(1, N_POPULATION + 1),
    'Age': ages,
    'Gender': genders,
    'MaritalStatus': marital_statuses,
    'InitialPension': pensions,
    'BirthYear': birth_years,
    'SpouseAgeDiff': spouse_age_diffs,
    'SpousePensionRate': spouse_pension_rate,
})

# Adjustment für Witwen/Witwer: Ihre Rente ist bereits die Hinterbliebenenrente (40%)
# Hier wird angenommen, dass die InitialPension von Witwen/Witwern bereits die reduzierte Rente ist.
# Für Simulationszwecke ist es aber sinnvoller, die InitialPension als die volle Rente zu führen,
# und die Status 'Widowed' einfach als 'Active' (mit reduzierter Rente) zu behandeln.
# Für dieses Simulationsmodell behalten wir die volle Rente und nehmen an, dass 'Widowed' nur den Zivilstand beschreibt.
# Die Logik im Skript muss dann die Rente von 'Widowed' als 100% behandeln, bis sie stirbt.
# Nur der im Skript erzeugte 'Widow/er' erhält die 40% Rente.

# Speichern des Bestandes
try:
    population_df.to_csv('data/rentnerbestand_initial.csv', index=False)
    print("Die Datei 'data/rentnerbestand_initial.csv' wurde erfolgreich erstellt.")
except FileNotFoundError:
    print("Fehler: Stellen Sie sicher, dass der Ordner 'data/' existiert, bevor Sie speichern.")
