# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# SITZUNG 10 Regressionsvoraussetzungen                 -----------
# ______________________________________________________-----------


# FAHRPLAN: Eine Regression läuft immer in dieser Reihenfolge ab:
# Schritt 1: Variablen auswählen (Theorie!), sichten & aufbereiten
# Schritt 2: VOR dem Modell prüfen:
# Skalenniveaus, Unabhängigkeit, Linearität, Exogenität
# Schritt 3: Modell rechnen & interpretieren (b, R², p/KI)
# Schritt 4: NACH dem Modell: alle Annahmen prüfen (Diagnostik)
# -> HEUTE!
# Schritt 5: ggf. nachbessern (z.B. quadrieren, robuste SEs)
# -> HEUTE!
# Schritt 6: Regressionsergebnisse schön darstellen!




# 01 setup -------------

# Pakete
library(haven)
library(labelled)
library(dplyr)
library(ggplot2)


library(stargazer) #install.packages("stargazer")
library(texreg) #install.packages("texreg")
library(performance) #install.packages("performance")
library(see)
library(qqplotr)

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

allbus_c_2023 %>%
  count(hs01)

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



# Hinweis: Das Modell mit den standardisierten Koeffizientnen ist
# sinnvoll für den Effektvergleich!
# Diagniostik führen wir aber am anderen, normalen Modell durch



# 04 Regressionsvoraussetzungen prüfen (Diagnostik) --------------

# Regressionsvoraussetzungen (Details: Folien Sitzung 10):

# A) VOR dem Modell zu prüfen (haben wir in Sitzung 09, Schritt 2 gemacht):
## 1 Skalenniveau: AV metrisch, UVen metrisch oder dichotom (Codebuch)
## 2 Exogenität: relevante Kontrollvariablen im Modell? (Theorie!)
## 3 Unabhängige Beobachtungen: keine Cluster, keine Messwiederholung (Studiendesign)
## 4 Linearität: Form des Zusammenhangs sichten (Scatterplot + Loess)
##    -> bestimmt die Modellspezifikation: linear, quadriert, logarithmiert?

# B) NACH dem Modell zu prüfen (Diagnostik) - das machen wir jetzt:
## 4 Linearität erneut: jetzt am Modell
## 5 Homoskedastizität der Residuen
## 6 Keine Multikollinearität - nur bei multipler Regression
## 7 Keine einflussreichen Ausreißer
## 8 Normalverteilung der Residuen - bei großem n unkritisch

# Hinweis: Diagnostik immer am normalen Modell (model_2), nicht am
# standardisierten - das ist nur ein Darstellungswerkzeug. Die Residuen
# sind ohnehin bis auf die Skalierung identisch.


## 4 Linearität ---------------

# GRAFISCH Residuen-vs.-Fitted-Plot

# performance-Paket
plot(performance::check_model(model_2,
                              check = "linearity", # nur linearität,
                              panel = FALSE)) # kein patchwork plot
# Alternative (Base R): Residuen vs. Fitted
# plot(model_2, which = 1)
# Interpretation: Die Referenzlinie sollte annähernd waagerecht bei 0
# verlaufen. Macht sie eine deutliche Kurve -> Linearität verletzt.

# Partial-Residual-Plots: Linearität je UV, unter Kontrolle der anderen
car::crPlots(model_2, terms = ~ incc + age + hs01)
# die blaue, gestrichelte Linie zeigt hier die Schätzung und die Pinke den
# empirischen Verlauf der Daten -> pinke Loess-Linie sollte der blauen folgen.
# Deutliche Krümmung bei einer UV -> diese UV transformieren (x², log, ...)
# NB: sex_bi (Dummy) ausgelassen - Linearität ist bei Dummies kein Thema,
# zwei Punkte liegen immer auf einer Geraden.

# hier sieht alles gut aus!

# STATISTISCH: Rainbow-Test
lmtest::raintest(model_2)
# Interpretation: H0 = Zusammenhang ist linear.
# p > .05 -> Linearität beibehalten. p < .05 -> verletzt.

# In unserem Fall spricht beides für Linearität!


## 5 Homoskedastizität der Residuen --------------------

# = Die Streuung der Residuen ist über alle vorhergesagten Werte hinweg gleich.

# GRAFISCH (performance):
plot(performance::check_heteroscedasticity(model_2))
# Grüne Linie soll flach verlaufen. Steigt/fällt sie deutlich,
# variiert die Streuung (= Heteroskedastizität).
# Befund hier: Linie nicht ganz flach (Streuung an den Rändern etwas
# größer) -> milde Heteroskedastizität

# Alternative (Base R): derselbe Plot wie bei Linearität!
# plot(model_2, which = 1)
# Rote Linie soll waagerecht bei 0 verlaufen (Linearität);
# Base R beschriftet automatisch die drei auffälligsten Fälle:
# Personen, die laut Modell zufrieden sein
# müssten (Fitted 8-9), aber es nicht so angegeben haben.

# STATISTISCH: Breusch-Pagan-Test
lmtest::bptest(model_2)
# Interpretation: H0 = Homoskedastizität.
# p < .05 -> Heteroskedastizität annehmen -> robuste Standardfehler verwenden.

# hier können wir also keine Homoskedastizität annehmen und müssen deshalb
# heteroskedastizitätsrobuste Standardfehler verwenden - das machen wir in Schritt 5.


## 6 Multikollinearität der UVs (nur bei multipler Regression) --------------

# = Die UVs sollten nicht zu stark untereinander korrelieren.

# GRAFISCH (performance):
plot(performance::check_collinearity(model_2))
# Alternative: als Tabelle
performance::check_collinearity(model_2)
# oder klassisch: car::vif(model_2)
# Interpretation: VIF < 5 unproblematisch, VIF > 10 starke Multikollinearität.
# Bei Werten > 10: eine der korrelierten UVen ausschließen oder zusammenfassen.

# OPTIONAL ERGÄNZEND: Korrelationsmatrix der metrischen UVs
allbus_c_2023 %>%
  dplyr::select(incc, age) %>%
  dplyr::filter(!is.na(incc), !is.na(age)) %>%
  correlation::correlation()
# Interpretation: Korrelationen zwischen UVs > 0.8 sind ein Warnsignal.


## 7 Ausreißer ------------------

# GRAFISCH (performance):
plot(performance::check_outliers(model_2))
# x-Achse: Leverage (Hebelwirkung: wie untypisch sind die UV-Werte eines Falls?)
# y-Achse: standardisierte Residuen (wie schlecht wird der Fall vorhergesagt?).
# Einflussreich ist, wer in beidem extrem ist:
# Die gestrichelten Konturlinien markieren die kritische
# Cook's-Distance-Schwelle (hier 0.8)

# Alternative (Base R): Cook's Distance
# plot(model_2, which = 4)
# Wie stark würden sich die Koeffizienten ändern, wenn dieser Fall rausfiele?
# Faustregeln: problematisch ab ~1; die konservative Schwelle 4/n markiert
# „auffällig, anschauen" — bei n ≈ 4470 wäre das 0.0009.

# -> Kein Handlungsbedarf: keine Fälle ausschließen.

# WICHTIG: Ausreißer prüfen heißt nicht automatisch ausschließen!
# Erst klären: Datenfehler oder echter Extremfall?


## 8 Normalverteilung der Residuen --------------------

# bei großem n von > ? unkritisch
# Ich zeige es der Vollständigkeit halber!

# GRAFISCH (performance): Q-Q-Plot
plot(performance::check_normality(model_2), type = "qq")
# Alternativen (Base R):
# plot(model_2, which = 2) # Q-Q-Plot
# Interpretation Q-Q-Plot: Punkte sollten möglichst nah an der Linie liegen.
# Abweichungen an den Rändern deuten auf Verletzung hin.

# STATISTISCH: Shapiro-Wilk-Test
shapiro.test(rstandard(model_2))
# Interpretation: H0 = Residuen sind normalverteilt.
# p < .05 -> Normalverteilung verwerfen.
# ACHTUNG 1: Bei großen Stichproben (wie hier!) wird der Test fast immer
# signifikant, auch bei minimalen Abweichungen. Die grafische Prüfung
# ist bei großem n aussagekräftiger!
# ACHTUNG 2: shapiro.test() funktioniert nur bis n = 5000.

# Beides spricht hier für eine Verletzung der Normalverteilungsannahme -
# bei n ~ 4500 dank zentralem Grenzwertsatz aber unkritisch (siehe Folien).


## All-in-one (Bonus) ------------------

# performance bietet alle Diagnostik-Plots auf einmal:
# performance::check_model(model_2)
# ACHTUNG: Wegen Paket-Updates (ggplot2 4.0) derzeit unzuverlässig -
# bleibt das Plots-Pane leer:
  # dev.off() ausführen,
  # testen, ob das Grafik-Device grundsätzlich funktioniert plot(1:10)
  # und dann Einzelchecks nutzen.



# 05: Konsequenzen ziehen und ggf. Modell nachbessern --------------

# BILANZ unserer Diagnostik:
# erfüllt:  Linearität (4), Multikollinearität (6), Ausreißer (7)
# verletzt: Homoskedastizität (5) -> robuste SEs (jetzt!)
#           Normalverteilung (8)  -> bei großem n unkritisch, keine Maßnahme

# Tipp: Robuste SEs (HC3) schaden nie - im Zweifel mitberichten.


## HC3-robuste Standardfehler -------------------------------------
hc3 <- lmtest::coeftest(model_2, vcov = sandwich::vcovHC(model_2, type = "HC3"))
hc3
# Koeffizienten bleiben identisch, nur SEs und p-Werte werden korrigiert.

# Vergleich: normale vs. robuste SEs
texreg::screenreg(
  list(model_2, model_2),
  custom.model.names = c("M2", "M2 (HC3)"),
  override.se      = list(0, hc3[, "Std. Error"]),
  override.pvalues = list(0, hc3[, "Pr(>|t|)"]),
  digits = 3
)
# Befund: Die Schlussfolgerungen ändern sich nicht - die milde
# Heteroskedastizität war bei n ~ 4500 praktisch folgenlos.
# Die Prüfung gehört trotzdem zum Handwerk!


## Exkurs: Nichtlinearität beheben (Demo) --------------------------

# Unsere Linearitätsprüfung war unauffällig - aber SO würde man
# nachbessern, wenn sie es nicht wäre:

# a) Quadratischer Term (bei U-Form), z.B. für Alter:
model_2_quad <- lm(ls01 ~ incc + age + I(age^2) + sex_bi + hs01,
                   data = allbus_c_2023,
                   weights = wghtpew)

texreg::screenreg(list(model_2, model_2_quad),
                  custom.model.names = c("M2", "M2 + Alter²"),
                  digits = 4)
# Lohnt sich der quadratische Term? Drei Indizien:
# - Ist I(age^2) signifikant?
# - Steigt das (Adj.) R² nennenswert?
# - Verschwindet die Krümmung im crPlot?
# ACHTUNG Interpretation: age und I(age^2) nur noch GEMEINSAM
# interpretierbar - der Alterseffekt hängt jetzt vom Alter selbst ab.

# b) Logarithmieren (bei Sättigung), z.B. für Einkommen:
model_2_log <- lm(ls01 ~ log(incc) + age + sex_bi + hs01,
                  data = allbus_c_2023,
                  weights = wghtpew)
summary(model_2_log)
# Nur bei positiven Werten möglich! (incc: 1-26, passt)
# Interpretation: Der Koeffizient gilt jetzt pro VERDOPPLUNG des
# Einkommens (genauer: pro log-Einheit), nicht mehr pro Kategorie.

# Nach jeder Respezifikation: Diagnostik wiederholen (zurück zu Schritt 4)!


## Bootstrap (optional / Ausblick) ----------------------------------

# Bei kleinem n + verletzter Normalverteilung wäre Bootstrapping eine
# Option: 2000 Stichproben mit Zurücklegen ziehen, Modell jeweils neu
# schätzen, SEs aus der Streuung der Replikationen gewinnen.
# Bei unserem n ist das nicht nötig - hier nur als Demo:

if(FALSE){
  # car::Boot() verlangt einen Datensatz ohne NAs -> vorher filtern
  # (lm() hatte diese Zeilen ohnehin still entfernt, jetzt explizit):
  allbus_model_data <- allbus_c_2023 %>%
    dplyr::filter(!is.na(ls01), !is.na(incc), !is.na(age),
                  !is.na(sex_bi), !is.na(hs01), !is.na(wghtpew))

  model_2_boot <- lm(ls01 ~ incc + age + sex_bi + hs01,
                     data = allbus_model_data,
                     weights = wghtpew)

  set.seed(1234)  # Reproduzierbarkeit
  fit_b <- car::Boot(model_2_boot, R = 2000)
  summary(fit_b)   # bootSE ~ normale SEs -> robust; bootBias nahe 0 -> ok
  confint(fit_b)   # kein KI enthält die 0 -> Effekte bleiben signifikant
  # (NB zur Warnmeldung: Perzentil- statt BCa-Methode - unproblematisch.)
}




# ALT --------------------
# 05: Konsequenzen ziehen und ggf. Modell nachbessern --------------

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
