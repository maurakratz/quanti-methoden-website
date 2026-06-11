# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# SITZUNG 10 Regressionsvoraussetzungen                 -----------
# ______________________________________________________-----------


# FAHRPLAN: Eine Regression läuft immer in dieser Reihenfolge ab:
# Schritt 1: Variablen auswählen (Theorie!), sichten & aufbereiten
# Schritt 2: VOR dem Modell prüfen: Skalenniveaus, Unabhängigkeit, Linearität
# Schritt 3: Modell rechnen & interpretieren (b, R², p/KI)
# Schritt 4: NACH dem Modell: Annahmen prüfen (Diagnostik)
# -> kommt nächste Woche in Sitzung 10!
# Schritt 5: ggf. nachbessern (robuste SEs, Bootstrap) -> ebenfalls Sitzung 10




# 01 setup -------------

# Pakete
library(haven)
library(labelled)
library(dplyr)
library(ggplot2)


library(stargazer) #install.packages("stargazer")
library(texreg) #install.packages("texreg")
library(performance) #install.packages("performance")


options(scipen = 999)
# verhindert wissenschaftliche Notation bei großen Zahlen,
# z.B. 1000000 wird zu 1e+06


# Daten
allbus_c_2023_raw <- haven::read_dta("./data/ZA8831_v1-3-0.dta")

# Zur Erinnerung: Unsere RQ ist: "Lässt sich die allgemeine Lebenszufriedenheit
# auf Einkommen, Alter, Geschlecht und subjektiven Gesundheitszustand
# zurückführen?

# Variablen im Modell:
# ls01 = Allgemeine Lebenszufriedenheit, 0-10 (AV)
# incc = Monatliches Nettoeinkommen, kategorisiert (UV)
# age = Alter in Jahren (UV)
# sex_bi = Geschlecht, dichotom: MANN/FRAU (UV)
# hs01 = Subjektiver Gesundheitszustand, 1-5, invers gepolt (UV)

# 02 Datenaufbereitung -------------

allbus_c_2023 <- allbus_c_2023_raw %>%
  dplyr::mutate(
    # AV & metrische UVs: Missings (negativ) auf NA
    ls01 = dplyr::case_when(ls01 < 0 ~ NA_real_, .default = as.double(ls01)),
    incc = dplyr::case_when(incc < 0 ~ NA_real_, .default = as.double(incc)),
    age  = dplyr::case_when(age  < 0 ~ NA, .default = age),
    # Gesundheit: quasi-metrisch, 1-5, invers gepolt!
    hs01 = dplyr::case_when(hs01 < 0 ~ NA, .default = hs01),
    # Geschlecht dichotomisieren: DIVERS (n = 18) auf NA
    sex_bi = haven::as_factor(dplyr::case_when(sex %in% c(1, 2) ~ sex, .default = NA)) %>%
      forcats::fct_drop()
  )



# 03 Regressionen rechnen -------------

# einfache lineare Regression
model_1 <- lm(ls01 ~ incc,
              data = allbus_c_2023,
              weights = wghtpew)

model_2 <- lm(ls01 ~ incc + age + sex_bi + hs01,
              data = allbus_c_2023,
              weights = wghtpew)

texreg::screenreg(list(model_1, model_2),
                     custom.model.names = c("Modell 1", "Modell 2"),
                     digits = 3,
                     single.row = TRUE)



# Schritt 4: Regressionsvoraussetzungen prüfen --------------

# Regressionsvoraussetzungen:

# A) VOR der eigentlichen Regression zu prüfen:
## Eine lineare Beziehung wird angenommen (z.B. über Streudiagramm Beziehung ansehen)
## AV: muss metrisch sein, UV(n) metrisch oder dichotom
## Beobachtungen unabhängig?
# B) Nach (oder während) einer Regression abzurufen:
## Homoskedastizität der Residuen, z.B. Breusch-Pagan Test aus dem 'car' Paket oder plot(model) Funktion
## Normalverteilung der Residuen plot(Model) Funktion
## (extreme) Ausreißer analysieren, welche die Ergebnisse verzerren können

# grundsätzlich gibt es 2 Möglichkeiten mit diesen Voraussetzungen umzugehen:
# Entweder man prüft alles einzeln durch => sehr zeitaufwendig (
# siehe Extra-Skript zum THema Regressionsvoraussetzungen prüfen)
# oder man rechnet von Anfang an
# a) robuste Regressionen gegen Ausreißer
# b) Bootstraps gegen Nicht-Normalverteilung
# c) heteroskedastizitätsrobuste Standardfehler gegen Homoskedastizitätsverletzung

# die komfortable all-in-one-Lösung
# funktioniert, aber kann unzuverlässig sein:
performance::check_model(model_2)
performance::check_model(model_2_z)


## Linearität ---------------

# GRAFISCH: Residuen vs. Fitted Plot
# Residuen vs. Fitted (Homoskedastizität + Linearität)
plot(model_2, which = 1)
# Interpretation: Die rote Linie sollte annähernd waagerecht bei 0 verlaufen.
# Macht sie eine deutliche Kurve -> Linearität verletzt.

# STATISTISCH: Rainbow-Test
lmtest::raintest(model_2)
# Interpretation: H0 = Zusammenhang ist linear.
# p < .05 -> Linearität verletzt. p > .05 -> Linearität beibehalten.

# in diesem fall spricht beides für Linearität!


## Homoskedastizität der Residuen --------------------

# = Die Streuung der Residuen ist über alle vorhergesagten Werte hinweg gleich.

# GRAFISCH: Residuen vs. Fitted Plot (derselbe Plot wie bei 3.1!)
plot(model_2, which = 1)
# Interpretation: Punktwolke sollte gleichmäßig ("chaotisch") streuen.
# Problematisch: Trichterform (Streuung wird breiter/schmaler).

# STATISTISCH: Breusch-Pagan-Test
lmtest::bptest(model_2)
# Interpretation: H0 = Homoskedastizität.
# p < .05 -> Heteroskedastizität annehmen -> robuste Standardfehler verwenden

## Normalverteilung der Residuen --------------------

# GRAFISCH 1: Histogramm der standardisierten Residuen
hist(rstandard(model_2))
# Interpretation: Sollte annähernd glockenförmig (symmetrisch um 0) sein.

# Q-Q-Plot (Normalverteilung der Residuen)
plot(model_2, which = 2)
# Interpretation: Punkte sollten möglichst nah an der Diagonalen liegen.
# Abweichungen an den Rändern deuten auf Verletzung hin.

# STATISTISCH: Shapiro-Wilk-Test
shapiro.test(rstandard(model_2))
# Interpretation: H0 = Residuen sind normalverteilt.
# p < .05 -> Normalverteilung verwerfen.
# ACHTUNG: Bei großen Stichproben (wie hier!) wird der Test fast immer
# signifikant, auch bei minimalen Abweichungen. Die grafische Prüfung
# ist bei großem n aussagekräftiger!
# NB: shapiro.test() funktioniert nur bis n = 5000.

# beides spricht für Verletzung dieser Annahme normalverteilter Residuen!

## Ausreißer ------------------
# Ausreißer (Cook's Distance)
plot(model_2, which = 4)
# Interpretation Cook's Distance:
# Alle Werte liegen unter 0.01 - weit entfernt von kritischen Schwellen
# (gängige Faustregel: problematisch ab ~1, konservativ ab 4/n ≈ 0.0009).
# Die markierten Fälle (939, 3314, 4878) sind zwar die einflussreichsten,
# aber auch sie verzerren das Modell nicht nennenswert.
# -> Kein Handlungsbedarf: keine Fälle ausschließen.
# (Fall 4878 kennen wir schon aus dem Residuenplot - eine Person, die laut
# Modell zufrieden sein "müsste", aber sehr unzufrieden ist.)

## Multikollinearität der UVs (bei multipler Regression) --------------

# = Die UVs sollten nicht zu stark untereinander korrelieren.

# STATISTISCH: Variance Inflation Factor (VIF)
car::vif(model_2)
# Interpretation: VIF < 5 unproblematisch, VIF > 10 starke Multikollinearität.
# Bei Werten > 10: eine der korrelierten UVs ausschließen oder zusammenfassen.

# GRAFISCH (ergänzend): Korrelationsmatrix der metrischen UVs
allbus_c_2023 %>%
  dplyr::select(incc, age) %>%
  dplyr::filter(!is.na(incc), !is.na(age)) %>%
  correlation::correlation()
# Interpretation: Korrelationen zwischen UVs > 0.8 sind ein Warnsignal.


# schöner allrounder mit performance:
performance::check_model(model_1)



# Schritt 5: Konsequenzen ziehen und ggf. Modell nachbessern --------------

# Unsere Diagnostik ergab: Heteroskedastizität + nicht-normalverteilte Residuen
# -> beide Remedies anwenden und prüfen, ob die Ergebnisse stabil bleiben.

# Tipp: einfach IMMER heteroskedastizitätsrobuste SEs und Bootstraps rechnen -
# dann sind wir auf der sicheren Seite!


## HC3-robuste Standardfehler -------------------------------------
hc3 <- lmtest::coeftest(model_2, vcov = sandwich::vcovHC(model_2, type = "HC3"))
# Koeffizienten bleiben identisch, nur SEs und p-Werte werden korrigiert.


## Bootstrap ----------------------
# car::Boot() verlangt einen Datensatz ohne NAs -> vorher auf die
# Modellvariablen filtern (lm() hatte diese Zeilen ohnehin still entfernt,
# jetzt passiert es nur explizit):

allbus_model_data <- allbus_c_2023 %>%
  dplyr::filter(!is.na(ls01), !is.na(incc), !is.na(age),
                !is.na(sex_bi), !is.na(hs01), !is.na(wghtpew))

model_2 <- lm(ls01 ~ incc + age + sex_bi + hs01,
              data = allbus_model_data,
              weights = wghtpew)
# gleiches Ergebnis wie oben!

# Bootstrap rechnen: 2000 Stichproben aus unserer Stichprobe ziehen
# (mit Zurücklegen) und das Modell jeweils neu schätzen
set.seed(1234)  # Startwert festlegen für Reproduzierbarkeit
fit_b <- car::Boot(model_2, R = 2000)

summary(fit_b)
# Interpretation:
# - original: die Koeffizienten unseres Modells (unverändert)
# - bootSE: Standardfehler aus der Streuung über die 2000 Replikationen -
#   hier fast identisch mit den normalen SEs -> Ergebnisse sind robust
# - bootBias: Differenz zwischen Original und Bootstrap-Mittel -
#   nahe 0 = unproblematisch

confint(fit_b, level = .95)
# Interpretation:
# Keines der KIs enthält die 0 -> alle Effekte bleiben auch beim
# verteilungsfreien Bootstrap signifikant.
# z.B. incc [0.016, 0.039]: der wahre Einkommenseffekt liegt mit
# 95% Sicherheit in diesem Bereich - klein, aber von 0 verschieden.

# Fazit: Trotz verletzter Annahmen ändern sich die Schlussfolgerungen
# nicht - die Verletzungen waren bei n ~ 4500 praktisch folgenlos.
# Die Prüfung gehört trotzdem zum Handwerk!

# (NB zur Warnmeldung: R nutzt die Perzentil-Methode statt BCa fürs KI -
# das ist unproblematisch und kann ignoriert werden.)


texreg::screenreg(
  list(model_1, model_2, model_2, model_2),
  custom.model.names = c("M1 (bivariat)", "M2", "M2 (HC3)", "M2 (Bootstrap)"),
  override.se      = list(0, 0, hc3[, "Std. Error"], summary(fit_b)$bootSE),
  override.pvalues = list(0, 0, hc3[, "Pr(>|t|)"],   0),
  digits = 3
)
