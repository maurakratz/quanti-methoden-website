# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# SITZUNG 11 Logistische Regressionen                   -----------
# ______________________________________________________-----------


# FAHRPLAN: Eine Regression läuft immer in dieser Reihenfolge ab:
# Schritt 1: Variablen auswählen (Theorie!), sichten & aufbereiten
# Schritt 2: VOR dem Modell prüfen:
# Skalenniveaus, Unabhängigkeit, Exogenität, Ausreichende Fallzahl/Basisrate
# Schritt 3: Modell rechnen & interpretieren (b, Modellgüte, p/KI)
# Schritt 4: NACH dem Modell: alle Annahmen prüfen (Diagnostik) + ggf.
# nachbessern
# Schritt 5: schön visualisieren




# 00 setup -------------

# Pakete
library(haven)
library(labelled)
library(dplyr)
library(ggplot2)
library(gmodels)


library(texreg) #install.packages("texreg")
library(broom) # Modelloutput als tidy tibble
library(survey) # gewichtete regressionen
library(parameters) # Koeffizienten & Odds Ratios aufbereiten (easystats)
library(performance) #install.packages("performance")
library(ggeffects) # vorhergesagte Wahrscheinlichkeiten
library(report) # sprachliche Interpretationshilfe
library(car) # VIF

options(scipen = 999)
# verhindert wissenschaftliche Notation bei großen Zahlen,



# Daten
allbus_c_2023_raw <- haven::read_dta("./data/ZA8831_v1-3-0.dta")

# RQ: Inwiefern hängt die Wahlabsicht fuer die AfD mit dem Vertrauen in
#   politische Parteien zusammen?
# AV ist binär -> lineare Regression ungeeignet -> logistische Regression

# mit labelled die passenden Variablen finden
allbus_c_2023_raw %>%
  labelled::look_for("Wahlabsicht")

# Variablenplan für das Modell:
# vote_afd = Wahlabsicht AfD (AV, binär: 1 = AfD, 0 = andere) <- aus pv01
# trust_parties = Vertrauen in politische Parteien, 1-7 (UV) <- aus pt15
# age = Alter in Jahren (UV)
# sex_bi = Geschlecht, dichotom: MANN/FRAU (UV)
# educ3 = Schulabschluss, 3 Stufen: niedrig/mittel/hoch (UV) <- aus educ


# 01 Datenaufbereitung -------------

# Wahlabsicht AfD:
allbus_c_2023_raw %>%
  count(pv01)
# 42 = AfD, 1-6 und 90 = andere
# 91 = "würde nicht wählen" -> zu nA
# <0 -> zu NA

# Vertrauen in parteien
allbus_c_2023_raw %>%
  count(pt15)
# 1 gar kein bis 7 großes Vertrauen
# < 0 -> zu NA

# Alter
allbus_c_2023_raw %>%
  count(age)
# < 0 -> zu NA

# Geschlecht
allbus_c_2023_raw %>%
  count(sex)
# < 0 -> zu NA
# 3 = Divers -> zu NA

# Bildungsstand
allbus_c_2023_raw %>%
  count(educ)
# 6 und 7 zu NA
# < 0 -> zu NA
# 1 und 2 zusammenfassen
# 3 bleibt so
# 4 und 5 zusammenfassen


allbus_c_2023 <- allbus_c_2023_raw %>%
  # AV: Wahlabsicht AfD
  dplyr::mutate(
    vote_afd = dplyr::case_when(
      pv01 == 42 ~ 1, # AfD
      pv01 == 91 ~ NA_real_, # "würde nicht wählen" -> Missing
      pv01 < 0   ~ NA_real_, # Missings
      .default   = 0 # andere Partei (inkl. "andere Partei", Code 90)
    ),
    # zentrale UV: Vertrauen in Parteien, 1-7, metrisch behandelt
    trust_parties = dplyr::case_when(pt15 < 0 ~ NA_real_, .default = as.double(pt15)),
    # Alter (metrisch)
    age = dplyr::case_when(age < 0 ~ NA_real_, .default = age),
    # Geschlecht dichotomisieren: DIVERS (n = 18) auf NA
    sex_bi = haven::as_factor(dplyr::case_when(sex %in% c(1, 2) ~ sex, .default = NA)) %>%
      forcats::fct_drop(),
    # Bildung: dünn besetzte Kategorien zusammenfassen (s. Praxistipp Folien),
    # "anderer Abschluss" & "noch Schüler:in" -> NA; niedrig = Referenz
    educ3 = factor(
      dplyr::case_when(
        educ %in% c(1, 2) ~ "niedrig", # ohne Abschluss + Haupt-/Volksschule
        educ == 3 ~ "mittel", # mittlere Reife
        educ %in% c(4, 5) ~ "hoch", # (Fach-)Hochschulreife
        .default = NA_character_
      ),
      levels = c("niedrig", "mittel", "hoch")
    )
  )

#missings prüfen
allbus_c_2023 %>%
  count(vote_afd)

allbus_c_2023 %>%
  count(trust_parties)

allbus_c_2023 %>%
  count(age)

allbus_c_2023 %>%
  count(sex_bi)

allbus_c_2023 %>%
  count(educ3)


# 02 Vorabprüfung ----------------------

# VOR dem Modell zu prüfen (logistische Regression):

# 1 Skalenniveau:
# AV binär (0/1), "1" = das interessierende Ereignis;
# UVs metrisch oder kategorial (Dummies)

# 2 Exogenität: relevante Kontrollvariablen im Modell? (Theorie!)

# 3 Unabhängige Beobachtungen: kein Cluster, keine Messwiederholung (Design)

# 4 Ausreichende Fallzahl: Basisrate + ~10 Ereignisse pro Prädiktor (s. oben)

# Basisrate: Anteil AfD-Wahlabsicht unter gültigen Fällen
# Wie verteilt sich die AV? (absolute Häufigkeiten)
allbus_c_2023 %>%
  dplyr::count(vote_afd)

# Basisrate = Anteil der Fälle mit AV == 1 (hier: AfD-Wahlabsicht)
# Bei einer 0/1-Variable ist der Mittelwert genau dieser Anteil,
allbus_c_2023 %>%
  dplyr::filter(!is.na(vote_afd)) %>%
  dplyr::summarise(
    n_afd      = sum(vote_afd),   # Anzahl AV == 1
    n_gesamt   = dplyr::n(),      # Anzahl gültiger Fälle
    basisrate  = mean(vote_afd),   # Anteil AV == 1  (= n_afd / n_gesamt)
    basisrate = n_afd / n_gesamt
  )
# Wichtig, weil die seltenere Kategorie genug Fälle braucht
# (Faustregel ~10 pro Prädiktor) und weil die Basisrate der
# Maßstab für die Modellgüte ist (s. Schritt 05).

# Faustregel anwenden:
# Modell hat 4 Prädiktoren (trust_parties, age, sex_bi, educ3)
# benötigt: 4 * 10 = 40 Fälle in der selteneren Kategorie
# vorhanden: 480 Fälle
# Faustregel klar erfüllt, Modell ist tragfähig

# 5 Separation

# Separation heißt: UV-Kategorie mit (fast) nur 0 oder 1 der AV)

# Separation prüfen: Kreuztabelle kategoriale UV x AV
# Gibt es eine Kategorie, die fast nur 0en oder fast nur 1en enthält?
allbus_c_2023 %>%
  dplyr::count(educ3, vote_afd)


# gleiches für sex_bi
allbus_c_2023 %>%
  dplyr::count(sex_bi, vote_afd)

gmodels::CrossTable(
  allbus_c_2023$sex_bi,
  allbus_c_2023$vote_afd,
  prop.r = FALSE,
  prop.c = FALSE,
  prop.t = FALSE,
  prop.chisq = FALSE
)



# 03 Regressionen rechnen -------------

# family = binomial(link = "logit") macht aus dem GLM eine logistische Regression.
# Anders als lm() schätzt glm() per Maximum-Likelihood (iterativ).

# einfache logistische Regression
model_log_1 <- glm(vote_afd ~ trust_parties,
                   data = allbus_c_2023,
                   family = binomial(link = "logit"))

# multiple logistische Regression
model_log_2 <- glm(vote_afd ~ trust_parties + age + sex_bi + educ3,
                   data = allbus_c_2023,
                   family = binomial(link = "logit"))

texreg::screenreg(list(model_log_1, model_log_2),
                  custom.model.names = c("Modell 1", "Modell 2"),
                  digits = 3,
                  single.row = TRUE)
# Achtung: In dieser Tabelle stehen die ROH-Koeffizienten = log-Odds.
# Die sind nur in Richtung & Signifikanz interpretierbar!

# Für PDF/HTML in einem Quarto-Chunk mit #| results: asis:
# texreg::texreg(list(model_log_1, model_log_2), use.packages = FALSE,
#                float.pos = "H")


## Interpretation der log odds & Modellgüte -------------------

# Vertrauen in Parteien, Frausein und hohe Bildung senken die Chancen, AfD
# zu wählen. Alle drei sind hochsignifikante Effekte

# Modellgüte

# AIC/BIC: nur relativ nützlich, zum Vergleich mit anderen Modellen für
# dieselbe AV (kleiner = besser) - allein stehend nicht interpretierbar.
# hier: unser multiples Modell ist besser als unser einfaches

# Pseudo-R² nach Tjur (Pendant zum R² der linearen Regression)
performance::r2_tjur(model_log_2)

# Gesamtüberblick zur Modellgüte (AIC, BIC, R², ...)
performance::model_performance(model_log_2)
# AIC/BIC: siehe oben
# PCP = 0.815 -> das Modell liegt in ~82 % der Fälle richtig (Achtung,
# das ist v.a. die Basisrate, siehe Konfusionsmatrix unten).

## Interpretation der odds ratios -------------------

# erst die exponenzierten log-Odds (= Odds Ratios) sind interpretierbar
exp(coef(model_log_2))

# komfortabel inkl. Konfidenzintervallen (easystats):
parameters::model_parameters(model_log_2, exponentiate = TRUE)

# alternativ mit broom:
broom::tidy(model_log_2, exponentiate = TRUE, conf.int = TRUE)


# Visualisierung der Odds Ratios als Forest Plot

# quick and dirty
plot(parameters::model_parameters(model_log_2, exponentiate = TRUE))

# flexibler mit ggplot2
or_plot_dat <- broom::tidy(model_log_2, exponentiate = TRUE, conf.int = TRUE) %>%
  dplyr::filter(term != "(Intercept)")  # Intercept hier nicht sinnvoll interpretierbar

ggplot2::ggplot(or_plot_dat,
                mapping = ggplot2::aes(x = estimate, y = term)) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  ggplot2::geom_pointrange(mapping = ggplot2::aes(xmin = conf.low, xmax = conf.high)) +
  ggplot2::scale_x_log10() +  # log-Skala: OR 0.5 und OR 2 liegen optisch symmetrisch zu 1
  ggplot2::labs(x = "Odds Ratio (log-Skala)", y = NULL,
                title = "Odds Ratios: AfD-Wahlabsicht") +
  ggplot2::theme_minimal(base_size = 13)


# Interpretation Odds Ratio:
# - OR < 1 senkt, OR > 1 erhöht die Chance auf AV == 1; OR = 1 kein Effekt
# - prozentuale Veränderung der Odds = (OR - 1) * 100

# Referenzkategorien: Mann (sex_bi), niedrige Bildung (educ3).
# Signifikant, wenn das KI die 1 NICHT einschließt (1 = kein Effekt).

# trust_parties = 0.53*** : pro Punkt mehr Vertrauen sinken die Odds einer
# AfD-Wahl um ~47 %
# age = 0.984***: pro Lebensjahr -1.6 % Odds
# sex_bi[FRAU] = 0.54*** : Frauen vs. Männer ~46 % geringere Odds.
# educ3[mittel] = 1.30 : nicht signifikant!(KI 0.93-1.81 schließt 1 ein)
# educ3[hoch] = 0.33***: hohe vs. niedrige Bildung ~67 % geringere Odds.
# (Intercept) = 3.58 : Baseline-Odds der Referenzperson bei trust=0, age=0
#   -> ausserhalb des Datenbereichs, NICHT inhaltlich interpretierbar.

# Merke: Richtung über OR </> 1, Stärke über (OR-1)*100 %,
# Signifikanz über "schliesst das KI die 1 ein?".


## Interpretation der predicted probabilities -------------

# was bedeutet der Effekt für die Wahrscheinlichkeit p?

# ggpredict hält die übrigen UVs konstant (Mittelwert / Referenzkategorie).
pred_trust <- ggeffects::ggpredict(model_log_2, terms = "trust_parties")

pred_trust
# Wenn das in eine schöne Tabelle gepackt werden soll, z.B. für PDF/HTML
# in einem Quarto-Chunk mit #| results: asis und ggf.
# insight::print_md(pred_trust)


# Was bedeuten diese vorhergesagten Wahrscheinlichkeiten?

# Gezeigt wird P(AfD-Wahl) je Vertrauenswert, übrige UVs konstant gehalten
# (age = 53.6, sex_bi = MANN, educ3 = niedrig -> "Referenzperson").

# - Bei sehr geringem Vertrauen (1) liegt die Wahrscheinlichkeit bei ~45 %,
#   bei hohem Vertrauen (7) nur noch bei ~2 %.
# - Der Effekt ist also nicht nur signifikant, sondern auch inhaltlich groß.
# - Achtung S-Kurve: der Rückgang ist NICHT konstant. Von 1->2 fällt p um
#   ~15 Prozentpunkte (0.45 -> 0.30), von 6->7 nur noch um ~1 (0.03 -> 0.02).
#   Derselbe "+1 Vertrauen"-Schritt wirkt je nach Ausgangsniveau anders.
# - Werte gelten für die Referenzperson; für Frauen/höhere Bildung läge das
#   ganze Niveau niedriger (s. Odds Ratios oben).
# - 95%-KI: Schätzung am Rand (hohes Vertrauen) präziser als in der Mitte.

# grafische Darstellung

# quick and dirty
plot(pred_trust)

# etwas ausgereifter
plot(pred_trust) +
  ggplot2::labs(
    x = "Vertrauen in Parteien",
    y = "P(AfD-Wahl)",
    title = "Vorhergesagte Wahrscheinlichkeit einer AfD-Wahlabsicht"
  )
# ggf. müsst ihr einmal "see" installieren und laden

# mit ggplot
ggplot2::ggplot(pred_trust,
                mapping = ggplot2::aes(x = x, y = predicted)) +
  ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin = conf.low, ymax = conf.high),
                       alpha = 0.15) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::labs(
    x = "Vertrauen in Parteien",
    y = "Vorhergesagte Wahrscheinlichkeit",
    title = "AfD-Wahlabsicht nach Vertrauen in Parteien"
  ) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::theme_minimal(base_size = 13)
# Achtung: Der Effekt auf p ist NICHT konstant (S-Kurve) - nahe p = 0.5
# am größten, an den Rändern kleiner. Deshalb Wahrscheinlichkeiten immer
# für konkrete UV-Werte angeben.

# Klassifikationsgüte:
# Konfusionsmatrix: wie gut trifft das Modell bei Schwelle 0.5?
model_data <- model_log_2$model # tatsächlich genutzte Fälle
pred_class <- as.integer(predict(model_log_2, type = "response") > 0.5)
table(Vorhersage = pred_class, Beobachtet = model_data$vote_afd)
# Interpretation: Bei seltenem Ereignis sagt das Modell oft fast immer 0
# vorher -> hohe Gesamttrefferquote, aber kaum Treffer für AV == 1.
# Die Trefferquote allein ist daher trügerisch.
# In diesem Fall: Das Modell sagt fast immer "keine AfD" vorher und liegt
# damit oft richtig, OHNE viel zu erklären.


# Bei seltenem Ereignis ist Schwelle 0.5 zu konservativ - das Modell
# "traut sich" kaum, AfD vorherzusagen (s. Konfusionsmatrix oben).
# Einfache Nachbesserung: Schwelle auf die Basisrate setzen statt auf 0.5.

basisrate <- mean(model_data$vote_afd)   # ≈ 0.12 (s. Schritt 02)

pred_class_neu <- as.integer(predict(model_log_2, type = "response") > basisrate)
table(Vorhersage = pred_class_neu, Beobachtet = model_data$vote_afd)

# Vergleich zur alten Schwelle (0.5):
# - Sensitivität (AfD-Fälle erkannt) steigt deutlich
# - dafür sinkt die Spezifität (mehr Falsch-Positive: "keine AfD"
#   fälschlich als "AfD" eingestuft)
# -> klassischer Trade-off: es gibt keine "richtige" Schwelle, nur einen
#    Kompromiss je nach Anwendungsfall (z.B. wie teuer ein False Positive
#    vs. ein False Negative ist).

# zeigt den Trade-off (Sensitivität vs. Spezifität) über ALLE möglichen
# Schwellen hinweg - nicht nur für eine fest gewählte (0.5 oder Basisrate).
roc_perf <- performance::performance_roc(model_log_2)
roc_perf # AUC-Wert in der Konsole
plot(roc_perf) # ROC-Kurve
# Interpretation AUC: 0.5 = Modell rät nur (Zufall), 1.0 = perfekte Trennung.
# Faustregel: ab ~0.7 brauchbar, ab ~0.8 gut.


# 04 Diagnostik -----------------------

# Logistische Regression hat WENIGER Annahmen als die lineare:
# Homoskedastizität und Normalverteilung der Residuen entfallen (binäre AV).
# Zu prüfen bleiben:
# Multikollinearität der UVs (nur bei multipler Regression)
# Ausreißer
# optional: Linearität des Logits (fortgeschritten, hier nicht geprüft)
# könnte zum Beispiel mit performance::binned_residuals() oder Box-Tidwell
# geprüft werden


# 1) Keine Multikollinearität (nur bei mehreren UVs): VIF
performance::check_collinearity(model_log_2)
plot(performance::check_collinearity(model_log_2))
# Faustregel: VIF < 5 unproblematisch, > 10 kritisch.

# 2) Keine einflussreichen Ausreißer
performance::check_outliers(model_log_2)
plot(performance::check_outliers(model_log_2))
# Auffällige Fälle erst prüfen (Datenfehler? Extremfall?), nicht blind löschen.
# Alternative (Base R): Cook's Distance
# plot(model_log_2, which = 4)

# HINWEIS - fortgeschrittene/optionale Diagnostik (hier nicht Pflicht):
# Linearität des Logits: prüft, ob der Zusammenhang einer metrischen UV
#   mit dem Logit linear ist. Geht NICHT per einfachem Scatterplot, vielmehr:
#   performance::binned_residuals(model_log_2) bzw.
#   plot(performance::binned_residuals(model_log_2)). (Bei echten Sozialdaten
#   (meldet dieser Test fast immer "bad fit" - das ist normal, gehört aber
#   als Limitation erwähnt. Anschließend Box-Tidwell-Test: gezielt
#   nachschlagen, WELCHE UV ggf. transformiert werden müsste (UV * log(UV) als
#   Zusatzterm, signifikant -> Logit in dieser UV nicht linear).


# 05 OPTIONAL: survey-gewichtete logistische Regression --------------
# Streng genommen sollte ALLBUS für Inferenz gewichtet werden (wghtpew).
# glm(..., weights = ) ist dafür NICHT korrekt (Warnungen, falsche SEs).
# Korrekt über das survey-Paket mit svyglm:

  design <- survey::svydesign(
    ids = ~1,
    weights = ~wghtpew,
    data = allbus_c_2023 %>%
      dplyr::filter(!is.na(vote_afd), !is.na(trust_parties),
                    !is.na(age), !is.na(sex_bi), !is.na(educ3))
  )

  model_log_w <- survey::svyglm(
    vote_afd ~ trust_parties + age + sex_bi + educ3,
    design = design,
    family = quasibinomial(link = "logit")
  )

  parameters::model_parameters(model_log_w, exponentiate = TRUE)


