"""
Generiert die technische Dokumentation als Word-Dokument (.docx)
für die ALM-Simulation (main_alm_simulation.py) und R-Analysefunktionen (functions.R).
"""

from docx import Document
from docx.shared import Pt, Inches, Cm, RGBColor
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.enum.table import WD_TABLE_ALIGNMENT
from docx.enum.style import WD_STYLE_TYPE
from docx.oxml.ns import qn
from docx.oxml import OxmlElement
import datetime


def set_cell_shading(cell, color_hex):
    """Setzt Hintergrundfarbe einer Tabellenzelle."""
    shading = OxmlElement('w:shd')
    shading.set(qn('w:fill'), color_hex)
    shading.set(qn('w:val'), 'clear')
    cell._tc.get_or_add_tcPr().append(shading)


def add_formula(doc, formula_text, label=None):
    """Fügt eine zentrierte Formel mit optionalem Label hinzu."""
    p = doc.add_paragraph()
    p.alignment = WD_ALIGN_PARAGRAPH.CENTER
    p.paragraph_format.space_before = Pt(6)
    p.paragraph_format.space_after = Pt(6)
    run = p.add_run(formula_text)
    run.font.name = 'Cambria Math'
    run.font.size = Pt(11)
    run.font.italic = True
    if label:
        run2 = p.add_run(f'    ({label})')
        run2.font.size = Pt(9)
        run2.font.color.rgb = RGBColor(100, 100, 100)
    return p


def add_formula_block(doc, lines):
    """Fügt einen mehrzeiligen Formelblock hinzu."""
    for line in lines:
        p = doc.add_paragraph()
        p.alignment = WD_ALIGN_PARAGRAPH.CENTER
        p.paragraph_format.space_before = Pt(2)
        p.paragraph_format.space_after = Pt(2)
        run = p.add_run(line)
        run.font.name = 'Cambria Math'
        run.font.size = Pt(11)
        run.font.italic = True


def add_variable_table(doc, variables):
    """Fügt eine Variablen-Beschreibungstabelle hinzu."""
    table = doc.add_table(rows=1, cols=3)
    table.style = 'Light Grid Accent 1'
    table.alignment = WD_TABLE_ALIGNMENT.CENTER

    headers = ['Variable', 'Typ', 'Beschreibung']
    for i, h in enumerate(headers):
        cell = table.rows[0].cells[i]
        cell.text = h
        for paragraph in cell.paragraphs:
            for run in paragraph.runs:
                run.font.bold = True
                run.font.size = Pt(9)

    for var, typ, desc in variables:
        row = table.add_row()
        cells = row.cells
        r0 = cells[0].paragraphs[0].add_run(var)
        r0.font.name = 'Cambria Math'
        r0.font.size = Pt(9)
        r0.font.italic = True
        r1 = cells[1].paragraphs[0].add_run(typ)
        r1.font.size = Pt(9)
        r2 = cells[2].paragraphs[0].add_run(desc)
        r2.font.size = Pt(9)

    doc.add_paragraph()  # Abstand


def create_documentation():
    doc = Document()

    # =====================================================================
    # STYLES
    # =====================================================================
    style = doc.styles['Normal']
    style.font.name = 'Calibri'
    style.font.size = Pt(10)
    style.paragraph_format.line_spacing = 1.15

    for level in range(1, 4):
        h_style = doc.styles[f'Heading {level}']
        h_style.font.color.rgb = RGBColor(0, 51, 102)

    # =====================================================================
    # TITELSEITE
    # =====================================================================
    for _ in range(6):
        doc.add_paragraph()

    title = doc.add_paragraph()
    title.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run = title.add_run('Technische Dokumentation')
    run.font.size = Pt(28)
    run.font.color.rgb = RGBColor(0, 51, 102)
    run.bold = True

    subtitle = doc.add_paragraph()
    subtitle.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run = subtitle.add_run('ALM-Simulator für Schweizer Pensionskassen')
    run.font.size = Pt(18)
    run.font.color.rgb = RGBColor(0, 102, 153)

    doc.add_paragraph()

    subtitle2 = doc.add_paragraph()
    subtitle2.alignment = WD_ALIGN_PARAGRAPH.CENTER
    run = subtitle2.add_run('Finanzmathematische Grundlagen und Funktionsreferenz')
    run.font.size = Pt(14)
    run.font.color.rgb = RGBColor(100, 100, 100)

    for _ in range(4):
        doc.add_paragraph()

    meta = doc.add_paragraph()
    meta.alignment = WD_ALIGN_PARAGRAPH.CENTER
    meta.add_run(f'Erstellt: {datetime.date.today().strftime("%d.%m.%Y")}').font.size = Pt(10)
    meta2 = doc.add_paragraph()
    meta2.alignment = WD_ALIGN_PARAGRAPH.CENTER
    meta2.add_run('Autor: Michel Bossong').font.size = Pt(10)
    meta3 = doc.add_paragraph()
    meta3.alignment = WD_ALIGN_PARAGRAPH.CENTER
    meta3.add_run('Quelle: main_alm_simulation.py, functions.R').font.size = Pt(10)

    doc.add_page_break()

    # =====================================================================
    # INHALTSVERZEICHNIS
    # =====================================================================
    doc.add_heading('Inhaltsverzeichnis', level=1)
    toc_items = [
        ('1', 'Einleitung und Überblick'),
        ('2', 'Anlageklassen und Korrelationsstruktur'),
        ('3', 'Simulation korrelierter Renditen (Gaussian Copula)'),
        ('4', 'Bond-Renditemodell (Staats-, Unternehmens- und Infrastrukturanleihen)'),
        ('5', 'Sterblichkeitsmodellierung und Bestandsführung'),
        ('6', 'Barwertberechnung der Verpflichtungen W(t)'),
        ('7', 'Leibrentenbarwertfaktor und Ehegattenanwartschaft'),
        ('8', 'Duration-Berechnung (Macaulay-Duration der Liabilities)'),
        ('9', 'Duration-Management und Cash Flow Matching'),
        ('10', 'Vermögensentwicklung und Deckungsgrad'),
        ('11', 'Spezialfunktionen (Interest Rate Cap, Sonder-Rente, Sammelstiftung)'),
        ('12', 'Risikometriken und Optimierung'),
        ('13', 'R-Analysefunktionen (functions.R) — Kurzübersicht'),
    ]
    for num, title_text in toc_items:
        p = doc.add_paragraph()
        p.paragraph_format.space_after = Pt(2)
        run = p.add_run(f'{num}.  {title_text}')
        run.font.size = Pt(10)

    doc.add_page_break()

    # =====================================================================
    # 1. EINLEITUNG
    # =====================================================================
    doc.add_heading('1. Einleitung und Überblick', level=1)

    doc.add_paragraph(
        'Diese Dokumentation beschreibt die finanzmathematischen Grundlagen und Rechenschritte '
        'des ALM-Simulators (Asset-Liability Management) für Schweizer Pensionskassen. '
        'Der Simulator verwendet Monte-Carlo-Methoden, um die zukünftige Entwicklung von '
        'Vermögen (Assets) und Verpflichtungen (Liabilities) unter stochastischen '
        'Kapitalmarktszenarien zu modellieren.'
    )

    doc.add_paragraph(
        'Das zentrale Skript main_alm_simulation.py implementiert folgende Kernkomponenten:'
    )

    items = [
        'Korrelierte Rendite-Simulation mit Fat-Tail-Unterstützung (Gaussian Copula / Student-t)',
        'Realistisches Bond-Modell mit Coupon, Duration-Effekt, Pull-to-Par und Default-Risiko',
        'Aktuarielle Barwertberechnung der Verpflichtungen inkl. Ehegattenanwartschaft',
        'Macaulay-Duration der Liabilities mit Duration-Matching-Strategien',
        'Cash Flow Matching (CFM) mit Tranchen-basiertem Bond-Portfolio',
        'Sterblichkeitsmodellierung basierend auf Schweizer Sterbetafeln (LPP/BVG)',
        'Risikomanagement: Interest Rate Cap, Sonder-Rente, Sammelstiftung-Modus',
    ]
    for item in items:
        doc.add_paragraph(item, style='List Bullet')

    doc.add_page_break()

    # =====================================================================
    # 2. ANLAGEKLASSEN
    # =====================================================================
    doc.add_heading('2. Anlageklassen und Korrelationsstruktur', level=1)

    doc.add_paragraph(
        'Das Modell umfasst 6 Anlageklassen. Der Index 0 (Zinsen) dient als Basiszinsfaktor '
        'und wird nicht direkt als Anlageklasse investiert, sondern steuert die Zinsstrukturkurve.'
    )

    table = doc.add_table(rows=7, cols=5)
    table.style = 'Light Grid Accent 1'
    table.alignment = WD_TABLE_ALIGNMENT.CENTER

    headers = ['Index', 'Klasse', 'Deutsch', 'Parameter', 'Beschreibung']
    for i, h in enumerate(headers):
        cell = table.rows[0].cells[i]
        cell.text = h
        for paragraph in cell.paragraphs:
            for run in paragraph.runs:
                run.font.bold = True
                run.font.size = Pt(9)

    data = [
        ['0', 'InterestRate', 'Zinsen', '\u03bc\u2080, \u03c3\u2080', 'Basiszins (Cash-Rate), kein direktes Investment'],
        ['1', 'GovBonds', 'Staatsanleihen', '\u03bc\u2081, \u03c3\u2081, D_Gov', 'Bond-Modell mit Duration'],
        ['2', 'CorpBonds', 'Unternehmensanleihen', '\u03bc\u2082, \u03c3\u2082, D_Corp, s_c', 'Inkl. Credit Spread und Default'],
        ['3', 'Equities', 'Aktien', '\u03bc\u2083, \u03c3\u2083, \u03bd\u2083', 'Log-Normal mit optionalem Fat Tail'],
        ['4', 'RealEstate', 'Immobilien', '\u03bc\u2084, \u03c3\u2084, \u03bd\u2084', 'Log-Normal Rendite'],
        ['5', 'Alternatives', 'Infrastruktur', '\u03bc\u2085, \u03c3\u2085, D_Alt, s_a', 'Bond-Modell (Infrastructure Debt)'],
    ]
    for row_idx, row_data in enumerate(data):
        for col_idx, val in enumerate(row_data):
            cell = table.rows[row_idx + 1].cells[col_idx]
            run = cell.paragraphs[0].add_run(val)
            run.font.size = Pt(9)

    doc.add_paragraph()

    doc.add_heading('Korrelationsmatrix', level=2)
    doc.add_paragraph(
        'Die Abhängigkeitsstruktur zwischen den 6 Faktoren wird durch eine 6\u00d76 '
        'Korrelationsmatrix \u03a3 definiert. Die Cholesky-Zerlegung L der Matrix wird '
        'vorab berechnet, sodass \u03a3 = L \u00b7 L\u1d40:'
    )
    add_formula(doc, '\u03a3 = L \u00b7 L\u1d40,    wobei L = cholesky(\u03a3)', 'Cholesky')

    doc.add_page_break()

    # =====================================================================
    # 3. SIMULATION KORRELIERTER RENDITEN
    # =====================================================================
    doc.add_heading('3. Simulation korrelierter Renditen', level=1)
    doc.add_paragraph('Funktion: simulate_all_returns()')
    doc.add_paragraph(
        'Die Renditesimulation erfolgt in mehreren Schritten, um korrelierte Fat-Tail-Verteilungen '
        'zu erzeugen. Der Ansatz basiert auf einer Gaussian Copula mit optionaler '
        'Student-t Marginaltransformation.'
    )

    doc.add_heading('Schritt 1: Korrelierte Standardnormalverteilungen', level=2)
    doc.add_paragraph(
        'Für jeden Zeitpunkt t \u2208 {1, ..., T} wird ein Vektor Z_t aus unabhängigen '
        'Standard-Normalverteilungen erzeugt und mittels der Cholesky-Matrix L korreliert:'
    )
    add_formula_block(doc, [
        'Z_t ~ N(0, I_n),    Z_t \u2208 \u211d\u2076',
        'X_t = Z_t \u00b7 L\u1d40,    X_t ~ N(0, \u03a3)',
    ])

    doc.add_heading('Schritt 2: Transformation zu Fat Tails (Gaussian Copula)', level=2)
    doc.add_paragraph(
        'Falls für eine Anlageklasse i Freiheitsgrade \u03bd_i < 100 definiert sind, '
        'wird die korrelierte Normalverteilung über den Copula-Ansatz in eine '
        'Student-t-Verteilung transformiert:'
    )
    add_formula_block(doc, [
        'u_i = \u03a6(X_{t,i})                     \u2192 Transformation zu Uniform(0,1)',
        'X\'_{t,i} = t\u207b\u00b9_{\u03bd_i}(u_i) \u00b7 \u221a[(\u03bd_i - 2) / \u03bd_i]    \u2192 Inverse CDF der t-Verteilung',
    ])
    doc.add_paragraph(
        'Der Skalierungsfaktor \u221a[(\u03bd-2)/\u03bd] stellt sicher, dass die transformierte '
        'Variable Varianz 1 hat. Kleinere \u03bd-Werte erzeugen dickere Tails (mehr Extremereignisse).'
    )

    doc.add_heading('Schritt 3: Log-Renditen mit Drift-Korrektur', level=2)
    doc.add_paragraph(
        'Aus den korrelierten (ggf. fat-tailed) Zufallsvariablen werden die Log-Renditen berechnet:'
    )
    add_formula_block(doc, [
        'ln(1 + r_{i,t}) = (\u03bc_i - \u03c3_i\u00b2/2) + \u03c3_i \u00b7 X\'_{t,i}',
        'r_{i,t} = exp[ln(1 + r_{i,t})] - 1',
    ])
    doc.add_paragraph(
        'Die Drift-Korrektur -\u03c3\u00b2/2 (Jensen\'s Inequality Correction) stellt sicher, dass '
        'E[exp(\u03c3X)] korrekt den erwarteten Drift \u03bc_i ergibt.'
    )

    doc.add_page_break()

    # =====================================================================
    # 4. BOND-RENDITEMODELL
    # =====================================================================
    doc.add_heading('4. Bond-Renditemodell', level=1)
    doc.add_paragraph(
        'Das Modell verwendet ein realistisches Bond-Modell für Staatsanleihen (Gov), '
        'Unternehmensanleihen (Corp) und Infrastructure Debt (Alt). '
        'Die Gesamtrendite setzt sich aus vier Komponenten zusammen.'
    )

    doc.add_heading('4.1 Zinsstrukturkurve', level=2)
    doc.add_paragraph(
        'Der Marktzins für eine Anlageklasse wird aus dem simulierten Basiszins r1_t, '
        'der aktuellen Duration D und der Steigung der Zinsstrukturkurve (Slope) hergeleitet:'
    )
    add_formula(doc, 'i_Gov(t) = r1_t + D_Gov(t) \u00b7 Slope', 'Gov Bonds Zins')
    add_formula(doc, 'i_Corp(t) = r1_t + D_Corp(t) \u00b7 Slope + s_c', 'Corp Bonds Zins')
    add_formula(doc, 'i_Alt(t) = r1_t + D_Alt(t) \u00b7 Slope + s_a', 'Infrastructure Debt Zins')

    doc.add_paragraph('Wobei s_c der Credit Spread für Unternehmensanleihen und s_a der Spread '
                      'für Infrastructure Debt ist.')

    doc.add_heading('4.2 Coupon-Rendite', level=2)
    doc.add_paragraph(
        'Der Coupon wird bei Kauf fixiert und bleibt bis zur nächsten Neuanlage konstant. '
        'Die effektive Coupon-Rendite bezieht sich auf den aktuellen Marktwert (MV), '
        'nicht den Par-Wert:'
    )
    add_formula(doc, 'r_Coupon(t) = C_fix / MV(t)', 'Coupon-Rendite')
    doc.add_paragraph(
        'Bei einem Discount-Bond (MV < 1) ist die effektive Coupon-Rendite höher als der '
        'nominale Coupon, bei einem Premium-Bond (MV > 1) entsprechend niedriger.'
    )

    doc.add_heading('4.3 Duration-Effekt (Kursänderung durch Zinsänderung)', level=2)
    doc.add_paragraph(
        'Die Kursänderung aufgrund von Zinsänderungen wird mittels modifizierter Duration '
        'und Konvexität berechnet. Es wird eine Taylor-Approximation 2. Ordnung verwendet:'
    )
    add_formula(doc, '\u0394r1 = r1_t - r1_{t-1}', 'Zinsänderung')
    add_formula_block(doc, [
        'Konvexität = (D\u00b2 + D) / (1 + i)\u00b2',
        'Duration-Komponente = -D \u00b7 \u0394r1 / (1 + i)',
        'Konvexitäts-Komponente = 0.5 \u00b7 Konvexität \u00b7 (\u0394r1)\u00b2',
        'r_Duration(t) = Duration-Komponente + Konvexitäts-Komponente',
    ])
    doc.add_paragraph(
        'Der Duration-Effekt wird auf [-50%, +50%] begrenzt, um unrealistische Kurssprünge zu vermeiden. '
        'Bei steigenden Zinsen (\u0394r1 > 0) sinkt der Bondkurs (negativer Duration-Effekt), '
        'bei sinkenden Zinsen steigt er.'
    )

    doc.add_heading('4.4 Pull-to-Par-Effekt', level=2)
    doc.add_paragraph(
        'Der Bond konvergiert linear gegen Par (100%) bei Fälligkeit. Der jährliche '
        'Amortisationsbetrag wird additiv zum Marktwert addiert:'
    )
    add_formula(doc, 'PtP_absolut = (1 - MV) / D_Rest', 'Pull-to-Par')
    doc.add_paragraph(
        'Bei einem Discount (MV < 1) ist PtP positiv (Wertsteigerung), '
        'bei einem Premium (MV > 1) negativ (Wertverlust). '
        'Der Effekt wird auf [-10%, +10%] pro Jahr begrenzt.'
    )

    doc.add_heading('4.5 Marktwert-Update', level=2)
    doc.add_paragraph(
        'Der Marktwert wird in jedem Zeitschritt aktualisiert. Der Duration-Effekt wirkt '
        'multiplikativ, Pull-to-Par additiv:'
    )
    add_formula_block(doc, [
        'MV_nach_Duration = MV(t) \u00b7 (1 + r_Duration)',
        'MV(t+1) = MV_nach_Duration + PtP_absolut',
        'MV(t+1) \u2208 [0.5, 1.5]     (Begrenzung)',
    ])

    doc.add_heading('4.6 Default-Verlust (nur Corp Bonds & Infrastructure Debt)', level=2)
    doc.add_paragraph(
        'Corporate Bonds und Infrastructure Debt haben ein stochastisches Default-Risiko:'
    )
    add_formula_block(doc, [
        'Falls U ~ Uniform(0,1) < p_Default :',
        '    L_Default = Exposure \u00b7 LGD',
        'Sonst: L_Default = 0',
    ])
    add_variable_table(doc, [
        ('p_Default', 'float', 'Jährliche Ausfallwahrscheinlichkeit (z.B. 0.3% für Corp, 1.3% für Infra)'),
        ('Exposure', 'float', 'Anteil des Portfolios, der bei Default betroffen ist (z.B. 2%)'),
        ('LGD', 'float', 'Loss Given Default - Verlustquote bei Ausfall (z.B. 40%)'),
    ])

    doc.add_heading('4.7 Gesamte Bond-Rendite', level=2)
    add_formula(doc,
                'r_Bond(t) = r_Coupon(t) + (MV(t+1) - MV(t)) / MV(t) - L_Default',
                'Total Bond Return')
    doc.add_paragraph(
        'Die Gesamtrendite setzt sich zusammen aus: Coupon-Einkommen + Marktwertänderung '
        '(Duration-Effekt + Pull-to-Par) - eventuelle Default-Verluste.'
    )

    doc.add_page_break()

    # =====================================================================
    # 5. STERBLICHKEITSMODELLIERUNG
    # =====================================================================
    doc.add_heading('5. Sterblichkeitsmodellierung und Bestandsführung', level=1)

    doc.add_heading('5.1 Sterbetafeln (LPP/BVG)', level=2)
    doc.add_paragraph(
        'Die Sterblichkeit wird basierend auf Schweizer Sterbetafeln (LPP = Loi sur la prévoyance '
        'professionnelle / BVG = Bundesgesetz über die berufliche Vorsorge) modelliert. '
        'Für jedes Alter x und Geschlecht g wird die einjährige Sterbewahrscheinlichkeit q_x '
        'aus der Tafel gelesen:'
    )
    add_formula(doc, 'q_x,g = P(Tod im Alter x | Geschlecht g)', 'Sterbewahrscheinlichkeit')
    doc.add_paragraph('Funktion: get_qx(age, gender, survival_table)')
    doc.add_paragraph('Für Alter > 110 oder < 0 wird q_x = 1.0 (sicherer Tod) angenommen.')

    doc.add_heading('5.2 Vorberechnete Sterblichkeits-Arrays', level=2)
    doc.add_paragraph(
        'Funktion: precompute_mortality_arrays() — Für Performance werden die q_x-Werte in '
        'NumPy-Arrays vorberechnet (qx_M[age] und qx_F[age]).'
    )

    doc.add_heading('5.3 Kumulative Überlebenswahrscheinlichkeiten', level=2)
    doc.add_paragraph('Funktion: precompute_survival_probs()')
    doc.add_paragraph(
        'Die kumulative Wahrscheinlichkeit, dass eine Person mit Startalter x noch k Jahre überlebt:'
    )
    add_formula_block(doc, [
        '\u2096p_x = \u220f_{j=0}^{k-1} (1 - q_{x+j})',
        '\u2080p_x = 1    (Startwert)',
    ])

    doc.add_heading('5.4 Timing-Konvention und Bestandsführung', level=2)
    doc.add_paragraph('Die Simulation folgt dieser Reihenfolge pro Zeitschritt t:')
    items = [
        '1. Rentenzahlung: Alle lebenden Rentner erhalten ihre Rente (nachschüssig)',
        '2. Sterblichkeitsprüfung: Für jede Person wird mit Wahrscheinlichkeit q_x der Tod simuliert',
        '3. Witwenrente: Bei Tod eines Verheirateten wird der überlebende Partner als Witwe/r aufgenommen',
        '4. Alterung: Alle Überlebenden werden um 1 Jahr gealtert',
    ]
    for item in items:
        doc.add_paragraph(item, style='List Bullet')

    doc.add_heading('5.5 Ehegattenrente (Witwenrente)', level=2)
    doc.add_paragraph(
        'Stirbt ein verheirateter Rentner, erhält der überlebende Ehepartner eine Witwenrente:'
    )
    add_formula(doc, 'R_Witwe = R_initial \u00b7 \u03b1_spouse', 'Witwenrente')
    add_variable_table(doc, [
        ('R_initial', 'CHF', 'Ursprüngliche Rente des Verstorbenen (InitialPension)'),
        ('\u03b1_spouse', 'float', 'Ehegattenrente-Satz (Default: 40% = 0.40)'),
        ('SpouseAgeDiff', 'int', 'Altersdifferenz zum Partner (Default: M -3, F +3)'),
    ])

    doc.add_page_break()

    # =====================================================================
    # 6. BARWERTBERECHNUNG W(t)
    # =====================================================================
    doc.add_heading('6. Barwertberechnung der Verpflichtungen W(t)', level=1)
    doc.add_paragraph('Funktion: calculate_liability_barwert_base()')

    doc.add_heading('6.1 Technischer Zinssatz', level=2)
    doc.add_paragraph(
        'Der technische Zinssatz i_tech wird aus dem Basiszins, der Zinsstruktur und einem '
        'Liability-Discount-Spread hergeleitet:'
    )
    add_formula(doc, 'i_tech(t) = max(r1_t + D_tech \u00b7 Slope + Spread_L,  Floor)', 'Tech. Zins')
    add_variable_table(doc, [
        ('D_tech', 'float', 'Duration für technischen Zinssatz (TECHNICAL_RATE_DURATION)'),
        ('Slope', 'float', 'Steigung der Zinsstrukturkurve (YIELD_CURVE_SLOPE)'),
        ('Spread_L', 'float', 'Liability Discount Spread (LIABILITY_DISCOUNT_SPREAD)'),
        ('Floor', 'float', 'Mindest-Technischer-Zinssatz (TECHNICAL_RATE_FLOOR)'),
    ])

    doc.add_heading('6.2 Diskontfaktor', level=2)
    add_formula(doc, 'v = 1 / (1 + i_tech)', 'Diskontfaktor')

    doc.add_heading('6.3 Barwert der eigenen Rente (Leibrentenbarwert)', level=2)
    doc.add_paragraph('Funktion: precompute_annuity_factors()')
    doc.add_paragraph(
        'Der Leibrentenbarwertfaktor ä_x gibt den Barwert einer jährlichen Zahlung von 1 CHF '
        'an eine Person im Alter x an, solange diese lebt. Die Berechnung erfolgt rekursiv '
        'von oben nach unten:'
    )
    add_formula_block(doc, [
        'ä_x = v \u00b7 p_x \u00b7 (1 + ä_{x+1})',
        'ä_{max_age} = 0    (Randbedingung)',
    ])
    doc.add_paragraph('Ausgeschrieben:')
    add_formula(doc, 'ä_x = \u2211_{k=1}^{\u03c9-x} v\u1d4f \u00b7 \u2096p_x', 'Leibrentenbarwert')
    doc.add_paragraph('wobei \u03c9 = 110 (maximales Alter) und \u2096p_x die kumulative Überlebenswahrscheinlichkeit ist.')

    doc.add_paragraph()
    doc.add_paragraph('Der Barwert der eigenen Rente für Person i:')
    add_formula(doc, 'BW_eigene(i) = R_i \u00b7 ä_{x_i,g_i}', 'BW eigene Rente')
    doc.add_paragraph('wobei R_i die jährliche Rente und x_i, g_i Alter und Geschlecht der Person sind.')

    doc.add_heading('6.4 Barwert der Ehegattenanwartschaft', level=2)
    doc.add_paragraph(
        'Für verheiratete Rentner wird die Anwartschaft auf eine zukünftige Ehegattenrente '
        'berechnet. Dies erfordert eine doppelte Summation über alle möglichen Todeszeitpunkte '
        'des Rentners und die bedingte Überlebenswahrscheinlichkeit des Ehepartners:'
    )
    add_formula_block(doc, [
        'BW_Anwartschaft(i) = R_spouse \u00b7 \u2211_{k=0}^{\u03c9-x} [',
        '    P(Rentner stirbt in Jahr k)',
        '    \u00b7 P(Ehepartner lebt in Jahr k)',
        '    \u00b7 v\u1d4f \u00b7 ä_{y+k, g_spouse}',
        ']',
    ])
    doc.add_paragraph('Wobei:')
    add_formula_block(doc, [
        'P(Rentner stirbt in k) = \u2096p_x \u00b7 q_{x+k}',
        'P(Ehepartner lebt in k) = \u2096\u208a\u2081p_y',
        'y = x + SpouseAgeDiff    (Alter des Ehepartners)',
    ])

    doc.add_heading('6.5 Gesamte Verpflichtung W(t)', level=2)
    add_formula(doc, 'W(t) = \u2211_{i \u2208 Lebende} [BW_eigene(i) + BW_Anwartschaft(i)]', 'Gesamt-Liability')
    doc.add_paragraph(
        'Die Summe läuft über alle lebenden Personen im Bestand (Status \u2260 Dead), '
        'einschliesslich der bereits verwitweten Personen (die keine eigene Anwartschaft '
        'mehr generieren, aber deren eigene Rente im Barwert enthalten ist).'
    )

    doc.add_page_break()

    # =====================================================================
    # 7. LEIBRENTENBARWERTFAKTOR
    # =====================================================================
    doc.add_heading('7. Leibrentenbarwertfaktor — Detailberechnung', level=1)

    doc.add_heading('7.1 Rekursive Berechnung', level=2)
    doc.add_paragraph(
        'Die Berechnung erfolgt rückwärts vom Maximalalter \u03c9 = 110 zum Alter 0. '
        'Die Verwaltungskosten (Admin Fee) pro Person werden zum Barwert addiert:'
    )
    add_formula_block(doc, [
        'ä_\u03c9 = 0',
        'ä_x = v \u00b7 (1 - q_x) \u00b7 (1 + ä_{x+1})     für x = \u03c9-1, \u03c9-2, ..., 0',
    ])
    doc.add_paragraph(
        'Diese rekursive Formel ist äquivalent zur direkten Summenformel, aber effizienter '
        'zu berechnen (O(n) statt O(n\u00b2)).'
    )

    doc.add_heading('7.2 Cache-Mechanismus', level=2)
    doc.add_paragraph(
        'Die Annuity-Faktoren werden nur neu berechnet, wenn sich der technische Zinssatz '
        'signifikant ändert (|i_tech_neu - i_tech_cached| > 0.0001). Dies vermeidet '
        'redundante Berechnungen und beschleunigt die Simulation erheblich.'
    )

    doc.add_page_break()

    # =====================================================================
    # 8. DURATION-BERECHNUNG
    # =====================================================================
    doc.add_heading('8. Duration-Berechnung (Macaulay-Duration der Liabilities)', level=1)
    doc.add_paragraph('Funktion: calculate_liability_duration()')

    doc.add_heading('8.1 Macaulay-Duration — Definition', level=2)
    doc.add_paragraph(
        'Die Macaulay-Duration misst die gewichtete mittlere Laufzeit aller zukünftigen '
        'Cashflows. Für die Liabilities einer Pensionskasse ist dies der Zeitpunkt, '
        'zu dem im Durchschnitt die Verpflichtungen fällig werden.'
    )
    add_formula(doc, 'D_Mac = \u2211(k \u00b7 CF_k \u00b7 v\u1d4f) / \u2211(CF_k \u00b7 v\u1d4f)', 'Macaulay-Duration')

    doc.add_heading('8.2 Duration pro Versicherten', level=2)
    doc.add_paragraph(
        'Für jede Person i wird die individuelle Duration berechnet, die sowohl die eigene '
        'Rente als auch die Ehegattenanwartschaft berücksichtigt:'
    )

    doc.add_paragraph('Eigene Rente:', style='List Bullet')
    add_formula_block(doc, [
        'PV_eigene(i) = \u2211_{k=1}^{\u03c9-x} R_i \u00b7 \u2096p_x \u00b7 v\u1d4f',
        'WT_eigene(i) = \u2211_{k=1}^{\u03c9-x} k \u00b7 R_i \u00b7 \u2096p_x \u00b7 v\u1d4f',
    ])

    doc.add_paragraph('Ehegattenanwartschaft (für verheiratete Aktive):', style='List Bullet')
    add_formula_block(doc, [
        'PV_spouse(i) = \u2211_k \u2211_j R_sp \u00b7 P(Tod in k) \u00b7 P(Witwe lebt j) \u00b7 v^{k+j}',
        'WT_spouse(i) = \u2211_k \u2211_j (k+j) \u00b7 R_sp \u00b7 P(Tod in k) \u00b7 P(Witwe lebt j) \u00b7 v^{k+j}',
    ])

    doc.add_paragraph('Individuelle Duration:')
    add_formula(doc, 'D_i = (WT_eigene(i) + WT_spouse(i)) / (PV_eigene(i) + PV_spouse(i))', 'Duration Person i')

    doc.add_heading('8.3 Portfolio-gewichtete Gesamt-Duration', level=2)
    add_formula(doc, 'D_Liab = \u2211(PV_i \u00b7 D_i) / \u2211(PV_i)', 'Gesamt-Duration')
    doc.add_paragraph(
        'Die Gesamt-Duration wird barwertgewichtet berechnet: Personen mit höheren '
        'Verpflichtungen haben einen grösseren Einfluss auf die Gesamt-Duration.'
    )

    doc.add_heading('8.4 Berechnungsintervall', level=2)
    doc.add_paragraph(
        'Die Liability-Duration wird standardmässig alle 5 Jahre exakt berechnet. '
        'In den Zwischenjahren wird sie um 1 pro Jahr approximiert (Duration sinkt, '
        'da der Bestand altert). Im Liability-Matching-Modus wird sie jährlich berechnet.'
    )

    doc.add_page_break()

    # =====================================================================
    # 9. DURATION-MANAGEMENT UND CFM
    # =====================================================================
    doc.add_heading('9. Duration-Management und Cash Flow Matching', level=1)
    doc.add_paragraph('Funktion: get_duration_for_mode()')

    doc.add_heading('9.1 Duration-Modi', level=2)

    table = doc.add_table(rows=5, cols=3)
    table.style = 'Light Grid Accent 1'
    headers = ['Modus', 'Verhalten', 'Formel']
    for i, h in enumerate(headers):
        cell = table.rows[0].cells[i]
        cell.text = h
        for p in cell.paragraphs:
            for r in p.runs:
                r.font.bold = True
                r.font.size = Pt(9)

    modes = [
        ['fixed', 'Duration sinkt jährlich um 1', 'D(t+1) = max(0, D(t) - 1)'],
        ['fixed_reset', 'Wie fixed, aber periodischer Reset', 'D(t+1) = D_init  wenn (t+1) mod Interval = 0'],
        ['liability_matching', 'Duration = Liability-Duration (jährlich)', 'D(t) = D_Liab(t)'],
        ['cashflow_matching', 'Gewichtete Duration der CFM-Tranchen', 'D(t) = \u2211(D_k \u00b7 w_k) / \u2211(w_k)'],
    ]
    for row_idx, row_data in enumerate(modes):
        for col_idx, val in enumerate(row_data):
            cell = table.rows[row_idx + 1].cells[col_idx]
            run = cell.paragraphs[0].add_run(val)
            run.font.size = Pt(9)

    doc.add_paragraph()

    doc.add_heading('9.2 Cash Flow Matching (CFM)', level=2)
    doc.add_paragraph('Funktion: calculate_expected_cashflows(), calculate_cfm_tranches()')
    doc.add_paragraph(
        'Beim Cash Flow Matching werden die erwarteten zukünftigen Rentenzahlungen berechnet '
        'und mit Bond-Tranchen dedizierter Laufzeiten abgedeckt:'
    )

    doc.add_paragraph('Schritt 1: Erwartete Cashflows berechnen', style='List Bullet')
    add_formula(doc, 'E[CF_k] = \u2211_i (R_i + Fee) \u00b7 \u2096p_{x_i} + E[CF_spouse,k]', 'Erwartete Cashflows')

    doc.add_paragraph('Schritt 2: Barwerte der Cashflows', style='List Bullet')
    add_formula(doc, 'PV_k = E[CF_k] / (1 + d)\u1d4f', 'Barwert Cashflow')
    doc.add_paragraph('wobei d = r1_erwartet + Slope \u00b7 10 (mindestens 1%).')

    doc.add_paragraph('Schritt 3: Gewichtung der Tranchen', style='List Bullet')
    add_formula(doc, 'w_k = PV_k / \u2211 PV_j', 'Tranchen-Gewichte')

    doc.add_paragraph('Schritt 4: Bond-Beträge pro Tranche', style='List Bullet')
    add_formula(doc, 'V_k = w_k \u00b7 V_Bond_Portfolio', 'Tranchen-Beträge')

    doc.add_heading('9.3 CFM-Coupon-Berechnung', level=2)
    doc.add_paragraph('Funktion: get_cfm_coupon_rate()')
    doc.add_paragraph(
        'Der gewichtete durchschnittliche Coupon des CFM-Portfolios basiert auf den bei '
        'Kauf (t=0) fixierten Coupons der einzelnen Tranchen:'
    )
    add_formula(doc, 'C_CFM(t) = \u2211_{k>t} [C_k \u00b7 w_k] / \u2211_{k>t} w_k', 'CFM-Coupon')
    doc.add_paragraph('wobei C_k = r1_initial + D_k \u00b7 Slope + Spread der Coupon der Tranche k ist.')

    doc.add_page_break()

    # =====================================================================
    # 10. VERMÖGENSENTWICKLUNG UND DECKUNGSGRAD
    # =====================================================================
    doc.add_heading('10. Vermögensentwicklung und Deckungsgrad', level=1)
    doc.add_paragraph('Funktion: run_monte_carlo_path_full() — Abschnitt D')

    doc.add_heading('10.1 Portfolio-Rendite', level=2)
    doc.add_paragraph(
        'Die Gesamtrendite des Portfolios ist die gewichtete Summe der Einzelrenditen:'
    )
    add_formula(doc,
                'r_port(t) = w_Gov \u00b7 r_Gov(t) + w_Corp \u00b7 r_Corp(t) + w_Eq \u00b7 r_Eq(t) '
                '+ w_RE \u00b7 r_RE(t) + w_Alt \u00b7 r_Alt(t)',
                'Portfolio-Rendite')
    doc.add_paragraph('Hinweis: w_InterestRate = 0 (kein direktes Investment in den Basiszins).')

    doc.add_heading('10.2 Mean Reversion für Aktien (optional)', level=2)
    doc.add_paragraph(
        'Bei aktivierter Mean Reversion wird nach starken kumulierten Verlusten ein '
        'zusätzlicher positiver Drift hinzugefügt:'
    )
    add_formula_block(doc, [
        'CumReturn_Eq(t) = \u220f_{s=1}^{t} (1 + r_Eq(s)) - 1',
        'ExpReturn_Eq(t) = (1 + \u03bc_LT)^t - 1',
        'Shortfall(t) = CumReturn_Eq(t) - ExpReturn_Eq(t)',
        'Falls Shortfall < Threshold:',
        '    Adj = min(-Strength \u00b7 Shortfall, 0.15)',
        '    r\'_Eq(t) = r_Eq(t) + Adj',
    ])

    doc.add_heading('10.3 Vermögensgleichung', level=2)
    add_formula(doc, 'V(t) = V(t-1) \u00b7 (1 + r_port(t)) - CF_Total(t) + Cap_Payout(t)', 'Vermögen')

    add_variable_table(doc, [
        ('V(t)', 'CHF', 'Vermögen (Assets) am Ende von Jahr t'),
        ('r_port(t)', 'float', 'Gewichtete Portfoliorendite in Jahr t'),
        ('CF_Total(t)', 'CHF', 'Gesamte Cashflows = Renten + Verwaltungskosten + Sonder-Rente'),
        ('Cap_Payout(t)', 'CHF', 'Auszahlung aus Interest Rate Cap (falls aktiv)'),
    ])

    doc.add_heading('10.4 Deckungsgrad (Funding Ratio)', level=2)
    add_formula_block(doc, [
        'DG(t) = V(t) / W(t)     falls W(t) > 0',
        'DG(t) = 2.0             falls W(t) = 0 und V(t) > 0',
        'DG(t) = 0.0             sonst',
    ])
    doc.add_paragraph(
        'Der Deckungsgrad ist die zentrale Kennzahl der Simulation. Ein DG < 100% bedeutet '
        'Unterdeckung, ein DG < 80% gilt als kritisch (Default-Schwelle).'
    )

    doc.add_heading('10.5 Initialer Barwert und Startkapital', level=2)
    doc.add_paragraph('Funktion: calculate_liability_barwert_initial()')
    add_formula_block(doc, [
        'i_tech,0 = r1_erwartet + D_tech \u00b7 Slope + Spread_L',
        'W(0) = Barwert(Bestand, i_tech,0)',
        'V(0) = W(0) \u00b7 (1 + GRR)',
    ])
    add_variable_table(doc, [
        ('GRR', 'float', 'General Reserve Rate (Wertschwankungsreserve)'),
        ('DG(0)', 'float', '= 1 + GRR (Start-Deckungsgrad, z.B. 1.05 = 105%)'),
    ])

    doc.add_page_break()

    # =====================================================================
    # 11. SPEZIALFUNKTIONEN
    # =====================================================================
    doc.add_heading('11. Spezialfunktionen', level=1)

    doc.add_heading('11.1 Interest Rate Cap', level=2)
    doc.add_paragraph(
        'Ein Interest Rate Cap schützt gegen steigende Zinsen. Die Prämie wird am Anfang '
        'bezahlt und reduziert das Startkapital:'
    )
    add_formula_block(doc, [
        'Notional = V(0) \u00b7 (w_Gov + w_Corp)',
        'Prämie = Notional \u00b7 Premium_Rate',
        'V(0)_eff = V(0) - Prämie',
        '',
        'Falls r1_t > r1_0 + Strike und t < Cap_Duration:',
        '    Payout = (r1_t - Strike_Level) \u00b7 V(t-1) \u00b7 (w_Gov + w_Corp)',
    ])

    doc.add_heading('11.2 Sonder-Rente (Special Pension Payout)', level=2)
    doc.add_paragraph(
        'Bei Überdeckung über einem Schwellwert wird eine einmalige Sonder-Rente ausgeschüttet, '
        'um den Deckungsgrad auf ein Ziel-Niveau zu senken:'
    )
    add_formula_block(doc, [
        'Falls DG(t) > DG_Threshold:',
        '    V_target = W(t) \u00b7 DG_Target',
        '    Payout = max(0, V(t) - V_target)',
        '    V(t)_neu = V(t) - Payout',
    ])

    doc.add_heading('11.3 Sammelstiftung-Modus', level=2)
    doc.add_paragraph(
        'Im Sammelstiftung-Modus wird alle X Jahre ein neuer Rentnerbestand (identisch zum '
        'initialen Bestand) hinzugefügt, sofern der Deckungsgrad > 100%:'
    )
    add_formula_block(doc, [
        'Falls (t+1) mod Interval = 0 und V(t) > W(t):',
        '    W_neu = Barwert(neuer Bestand, i_tech)',
        '    V_neu = W_neu \u00b7 (1 + GRR)',
        '    W(t) = W(t) + W_neu',
        '    V(t) = V(t) + V_neu',
    ])

    doc.add_page_break()

    # =====================================================================
    # 12. RISIKOMETRIKEN
    # =====================================================================
    doc.add_heading('12. Risikometriken und Optimierung', level=1)

    doc.add_heading('12.1 Risikometriken', level=2)
    doc.add_paragraph('Funktion: calculate_risk_metrics()')
    add_formula_block(doc, [
        'P(Unterdeckung) = |{Pfade mit min(DG) < 80%}| / N_Pfade',
        'P(Default) = |{Pfade mit min(V_t) \u2264 0}| / N_Pfade',
    ])
    doc.add_paragraph(
        'Für jeden Unterdeckungs- oder Default-Event werden die Ursachen analysiert '
        '(Aktiencrash, Zinssenkung, Zinsanstieg, Corporate Defaults, Liability-Explosion).'
    )

    doc.add_heading('12.2 Zielfunktion der Optimierung', level=2)
    doc.add_paragraph('Datei: optimize_alm_beta.py')
    doc.add_paragraph(
        'Die Optimierung minimiert eine gewichtete Kombination der Risikometriken:'
    )
    add_formula(doc,
                'Objective = \u03b1 \u00b7 P(Unterdeckung) + \u03b2 \u00b7 P(Default) + \u03b3 \u00b7 E[Shortfall]',
                'Zielfunktion')
    add_variable_table(doc, [
        ('\u03b1', 'float', 'Gewicht für Unterdeckung (Default: 1.0)'),
        ('\u03b2', 'float', 'Gewicht für Default (Default: 5.0)'),
        ('\u03b3', 'float', 'Gewicht für erwarteten Shortfall (Default: 0.00001)'),
    ])

    doc.add_heading('12.3 Optimierte Parameter', level=2)
    items = [
        'Asset-Gewichte: w_Gov, w_Corp, w_Eq, w_RE (w_Alt als Residuum)',
        'Bond-Durations: D_Gov_initial, D_Corp_initial',
        'Nebenbedingungen: \u2211 w_i = 1, w_i \u2265 0, w_Alt \u2264 25% (Art. 53 BVV2)',
    ]
    for item in items:
        doc.add_paragraph(item, style='List Bullet')

    doc.add_page_break()

    # =====================================================================
    # 13. R-FUNKTIONEN
    # =====================================================================
    doc.add_heading('13. R-Analysefunktionen (functions.R) — Kurzübersicht', level=1)
    doc.add_paragraph(
        'Die R-Funktionsdatei stellt Analyse- und Visualisierungsfunktionen bereit, die auf '
        'den exportierten CSV-Daten der Monte-Carlo-Simulation arbeiten.'
    )

    # Tabelle der R-Funktionen
    table = doc.add_table(rows=1, cols=3)
    table.style = 'Light Grid Accent 1'
    headers = ['Funktion', 'Zweck', 'Outputs']
    for i, h in enumerate(headers):
        cell = table.rows[0].cells[i]
        cell.text = h
        for p in cell.paragraphs:
            for r in p.runs:
                r.font.bold = True
                r.font.size = Pt(9)

    r_funcs = [
        ['load_mc_data()',
         'Lädt alle CSV-Dateien aus dem Datenordner und kombiniert sie zu einem DataFrame. '
         'Erstellt eindeutige Pfad-Nummern über mehrere Dateien hinweg.',
         'data.frame mit allen Simulationspfaden'],
        ['analyze_default_by_interest_shock()',
         'Analysiert den Zusammenhang zwischen Zinsschocks und Default-Wahrscheinlichkeit. '
         'Berechnet relatives Risiko und führt Chi\u00b2-Tests durch.',
         'Barplot Default-Rate, Density-Plot r1_t, Dezil-Analyse, Chi\u00b2-Test'],
        ['analyze_r1t_default_comparison()',
         'Vergleicht die r1_t-Verteilung zwischen Default-Pfaden und allen Pfaden. '
         'Nutzt Kolmogorov-Smirnov-Tests zur Signifikanzprüfung.',
         'Density-Plot, Box-Plot, KS-Test-Ergebnisse'],
        ['plot_path_analysis()',
         'Erstellt einen Einzelpfad-Plot mit Deckungsgrad-Zeitreihe und Referenzlinien (80%, 100%).',
         'ggplot-Objekt'],
        ['plot_boxplot_difference()',
         'Box-Plot des Deckungsgrads für ein bestimmtes Jahr mit optionalen Datenpunkten.',
         'ggplot-Objekt'],
        ['plot_path_analysis_detailed()',
         'Detaillierte Mehr-Panel-Analyse eines Pfades inkl. Witwen, V_t, W_t, Rentner-Anzahl '
         'mit Vergleich zu statistischen Benchmarks (Median, P5-P95).',
         '4-Panel-Plot (Witwen/Returns, V_t/W_t/DG, Rentner, Parameter-Tabelle)'],
        ['analyze_cashflow_dispersion()',
         'Analysiert die Streuung der Cashflows in Abhängigkeit der Bestandseigenschaften '
         '(Grösse, Alter, Geschlecht, Ehestand, Pensions-Heterogenität).',
         '10 Plots inkl. Heatmap und Korrelationsmatrix'],
        ['analyze_return_volatility()',
         'Analysiert die Portfolio-Return-Volatilität nach Asset Allocation und Duration. '
         'Unterstützt Duration-Matching (Neutralisierung des Duration-Effekts).',
         'Efficient Frontier, Ranking, Duration-Analyse, Heatmap'],
    ]

    for func_data in r_funcs:
        row = table.add_row()
        for col_idx, val in enumerate(func_data):
            cell = row.cells[col_idx]
            run = cell.paragraphs[0].add_run(val)
            run.font.size = Pt(8)
            if col_idx == 0:
                run.font.name = 'Consolas'
                run.font.bold = True

    doc.add_paragraph()

    doc.add_heading('Verwendete statistische Methoden in R', level=2)
    methods = [
        'Kolmogorov-Smirnov-Test: Vergleich von Verteilungen (Default-Pfade vs. alle Pfade)',
        'Chi\u00b2-Test: Unabhängigkeitstest Zinsschock vs. Default',
        'Variationskoeffizient (CV): Streuungsmass für Cashflow-Analyse',
        'Pearson-Korrelation: Zusammenhang zwischen Bestandseigenschaften und CV',
        'LOESS-Regression: Nicht-parametrische Glättung in Scatter-Plots',
        'Sharpe Ratio: Risikoadjustierte Rendite (Mean Return / Std.Dev.)',
        'VaR/CVaR (5%): Value at Risk und Conditional VaR der Portfolio-Rendite',
    ]
    for m in methods:
        doc.add_paragraph(m, style='List Bullet')

    # =====================================================================
    # SPEICHERN
    # =====================================================================
    output_path = '/home/user/alm-web/ALM_Technische_Dokumentation.docx'
    doc.save(output_path)
    print(f'Dokumentation gespeichert: {output_path}')
    return output_path


if __name__ == '__main__':
    create_documentation()
