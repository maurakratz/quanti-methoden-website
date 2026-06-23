# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# SITZUNG 09 Regressionen                               -----------
# ______________________________________________________-----------



# 01 setup -------------

# Pakete
library(haven)
library(labelled)
library(dplyr)
library(ggplot2)


library(stargazer) #install.packages("stargazer")
library(texreg) #install.packages("texreg")
library(performance) #install.packages("performance")
#library(modelsummary) #install.packages("modelsummary")
# modelsummary, report, effectsize, parameters

options(scipen = 999)
# verhindert wissenschaftliche Notation bei großen Zahlen,
# z.B. 1000000 wird zu 1e+06


# Daten
allbus_c_2023_raw <- haven::read_dta("./data/ZA8831_v1-3-0.dta")



# 02 Inferenzstatistik - Beispiel --------------

# Bei Inferenzstatistik geht es darum, herauszufinden, ob ein in der
# Stichprobe gefundener Effekt (wahrscheinlich) auch in der Grundgesamtheit
# existiert.

# Dafür können grundsätzlich zwei verschiedene Methoden der Schätzung angewendet werden
# Intervallschätzung (haben iene Ober-und Untergrenze)
# und Punktschätzung (p = ... oder ***)

# Wenn wir beispielsweise die Korrelation aus der
# vergangenen Sitzung nehmen, in der es um die Frage ging, ob es einen
# Zusammenhang zwischen Geschlecht und Erwerbsstatus gibt, können wir mit einem
# Chi-Quadrat-Test herausfinden, ob der beobachtete Zusammenhang in der
# Grundgesamtheit wahrscheinlich auch existiert.

allbus_c_2023 <- allbus_c_2023_raw %>%
  dplyr::mutate(
    sex_f  = haven::as_factor(dplyr::case_when(sex  < 0 ~ NA, .default = sex)) %>%
      forcats::fct_drop(),
    work_f = haven::as_factor(dplyr::case_when(work < 0 ~ NA, .default = work)) %>%
      forcats::fct_drop() #  entfernt leere Faktorlevels
  )


# Chi² Test
sum(stats::chisq.test(allbus_c_2023$sex_f, allbus_c_2023$work_f)$observed)

effectsize::cramers_v(
  stats::chisq.test(allbus_c_2023$sex_f, allbus_c_2023$work_f),
  ci = 0.95,
  alternative = "two.sided"
)

stats::chisq.test(allbus_c_2023$sex_f, allbus_c_2023$work_f) %>%
  report::report_table()

# Interpretation:

# p-Wert (Punktschätzung):
# p < .001 bedeutet: Wenn in der Grundgesamtheit KEIN Zusammenhang bestünde
# (Nullhypothese), wäre es äußerst unwahrscheinlich, einen so starken
# Zusammenhang wie in unserer Stichprobe zu beobachten. -> Wir verwerfen die
# Nullhypothese: der Zusammenhang ist statistisch signifikant.

# Konfidenzintervall (Intervallschätzung):
# Das 95%-KI [0.20, 0.24] bedeutet: Bei wiederholter Stichprobenziehung würde
# der wahre Cramér's V Wert der Grundgesamtheit in 95% der Fälle zwischen
# 0.20 und 0.24 liegen. Da das Intervall die 0 nicht einschließt, ist der
# Zusammenhang statistisch signifikant — und zwar ein mittlerer Effekt.



# 03 Regressionen -------------

# Regressionen sind ein Verfahren, um den Zusammenhang zwischen einer abhängigen
# Variable (y) und einer oder mehreren unabhängigen Variablen (x) zu modellieren.

# Regressionsgleichung: Y = a + b*x + Fehlerterm

# dabei gibt es drei zentrale Werte:
  # den Regressionskoeffizienten b => Stärke und Richtung des Zusammenhangs
  # das Bestimmtheitsmaß R² => Modellgüte: wie viel Prozent der Varianz der
  # abhängigen Variable durch die unabhängige(n) Variable(n) erklärt wird
  # den p-Wert oder das CI (Konfidenzintervall)

# FAHRPLAN: Eine Regression läuft immer in dieser Reihenfolge ab:
# Schritt 1: Variablen auswählen (Theorie!), sichten & aufbereiten
# Schritt 2: VOR dem Modell prüfen:
# Skalenniveaus, Unabhängigkeit, Linearität, Exogenität
# Schritt 3: Modell rechnen & interpretieren (b, R², p/KI)
# Schritt 4: NACH dem Modell: alle Annahmen prüfen (Diagnostik)
# -> kommt nächste Woche in Sitzung 10!
# Schritt 5: ggf. nachbessern (z.B. quadrieren, robuste SEs)
# -> ebenfalls Sitzung 10
# Schritt 6: Regressionsergebnisse schön darstellen!


# Schritt 1: Daten importieren, Variablen sichten & vorbereiten --------------

# Beispielforschungsfrage:
# Sagt Einkommen die allgemeine Lebenszufriedenheit vorher?

# Variablen sichten
allbus_c_2023 %>%
  labelled::look_for("Einkommen")
# die geeignetste Variable für uns ist incc mit 26 Kategorien und < 0 = missing

allbus_c_2023 %>%
  labelled::look_for("Lebenszufriedenheit")
# ls01 von 0-10 mit < 0 = missing

# Variablen aufbereiten
allbus_c_2023 <- allbus_c_2023 %>%
  dplyr::mutate(
    incc = dplyr::case_when(incc < 0 ~ NA, .default = incc),
    ls01 = dplyr::case_when(ls01 < 0 ~ NA, .default = ls01)
  )
# NB: keine Umwandlung zu Faktoren nötig

# Häufigkeiten prüfen
allbus_c_2023 %>%
  count(incc) %>%
  print(n = Inf)

allbus_c_2023 %>%
  dplyr::count(is.na(incc))
# 700 missings bei fast 5000 Befragten

allbus_c_2023 %>%
  count(ls01) %>%
  print(n = Inf)

allbus_c_2023 %>%
  tidyr::drop_na() %>%
  count(ls01) %>%
  sum()
# 80 missings bei fast 5000 Befragten


# Schritt 2: Vorabprüfungen ---------

# Skalenniveau, Unabhängigkeit d. Beobachtungen, Linearität d.
# Zusammenhangs und Exogenität prüfen !

## Skalenniveaus ------------
  # Das Einkommen incc ist eine ordinale Variable, da die Kategorien eine
  # natürliche Rangfolge haben, aber die Abstände zwischen den Kategorien
  # nicht gleich sind. Wir behandeln sie quasi-metrisch, was bei 26 Stufen
  # vertretbar ist. Alternativ müssten wir eine ordinale Regression rechnen,
  # z.B. mit MASS::polr())

  # Die Lebenszufriedenheit ls01 betrachten wie ebenfalls als quasi-metrische
  # Variable, da sie auf einer Skala von 0 bis 10 gemessen wird. Die Abstände
  # sind streng genommen nicht unbedingt gleich - aber vertretbar.


## Linearität ---------------------
allbus_c_2023 %>%
ggplot2::ggplot(aes(x = incc, y = ls01)) +
  geom_jitter() + # überlagernde Punkte anzeigen
  stat_smooth(method = "loess") + # Form des Zusammenhangs anzeigen (gerade = linear)
  xlab("Einkommen (kat.)") + ylab("Lebenszufriedenheit")
# erster Eindruck: Linearitätsannahme ist vertretbar,
# aber der Zusammenhang ist schwach.
# Genauere Prüfung nach der Spezifikation des Modells

## unabhängige Beobachtungen --------------
   # Keine empirische Prüfung - ergibt sich aus dem Studiendesign:
   # Der ALLBUS ist eine Zufallsstichprobe ohne Messwiederholung
   # -> Annahme erfüllt. Bei voneinander abhängigen Beobachtungen
   # (zum Beispiel zeitlich oder räumlich) müssen andere Modelle gerechnet werden

## Exogenität --------------

   # Keine empirische Prüfung möglich (der Fehlerterm ist unbeobachtbar!) -
   # theoriegeleitet fragen: Welche Variablen beeinflussen die AV UND eine
   # UV gemeinsam? Fehlen solche Variablen -> Omitted Variable Bias,
   # die Koeffizienten sind verzerrt.
   # Bei uns: Gesundheit und Alter beeinflussen Einkommen UND
   # Lebenszufriedenheit -> deshalb sind sie als Kontrollvariablen im Modell.
   # Kandidaten, die noch fehlen: Erwerbsstatus, Familienstand, Ost/West.
   # -> nie perfekt erfüllbar; wichtig ist die theoretische Begründung
   # der Modellspezifikation.



# Schritt 3: Regression durchführen ---------------

## einfache lineare Regression ----------
model_1 <- lm(ls01 ~ incc,
              data = allbus_c_2023,
              weights = wghtpew)
# NB: Mit weights = sind die Koeffizienten korrekt gewichtet.
# Die Standardfehler sind streng genommen nicht designkorrekt -
# für unsere Zwecke aber ausreichend. (Designkorrekte SEs: survey::svyglm())

# Ergebnis der Regression abrufen
model_1
summary(model_1)

# Modellgüte:
# R² = 0.024: Das Einkommen erklärt nur 2,4% der Varianz der
# Lebenszufriedenheit. Das Modell leistet also nur einen sehr geringen
# Erklärungsbeitrag zum Verständnis der Lebenszufriedenheit.

# Effektgröße (Relevanz):
# Regressionskoeffizient b = 0.055: Jede Erhöhung des Einkommens um eine
# Kategorie geht mit einem Anstieg der Lebenszufriedenheit um 0.055 Punkte
# (auf der Skala von 0-10) einher. Das ist ein sehr schwacher Effekt!

# Signifikanz:
# p-Wert < .001: Das Modell mit Einkommen erklärt signifikant mehr Varianz
# der Lebenszufriedenheit als ein Modell ohne Prädiktor (also nur mit Intercept).
# Das wäre das sogenannte Nullmodell, in dem der Mittelwert der
# Lebenszufriedenheit als Vorhersage für alle Fälle verwendet wird.

# Fazit:
# Das Modell mit Einkommen ist also statistisch signifikant, aber praktisch
# kaum relevant, da der Effekt sehr klein ist und das Modell wenig
# Varianz erklärt.



# Ergebnis schön darstellen in einer klassischen Regressionstabelle:

# Option 1: texreg (gut für Quarto/HTML)
# perfekt um Modelle zu vergleichen
texreg::screenreg(model_1) # Konsole
# Hinweis zu den texreg-Varianten:
# texreg::screenreg() -> Konsole (zum Arbeiten in RStudio)
# texreg::texreg()    -> LaTeX-Tabelle (für PDF-Rendering, mit #| results: asis)
# texreg::htmlreg()   -> HTML-Tabelle (falls ihr nach HTML rendert)
# eine gute Alternative ist modelsummary::modelsummary()

# Option 2: stargazer
# der old-school classic
stargazer::stargazer(model_1, type = "text")   # Konsole
# stargazer::stargazer(model_1, type = "html") in HTML



## multiple lineare regression ------------

# RQ: Sagt Einkommen die allgemeine Lebenszufriedenheit vorher?
# theoretisch sinnvolle Prädiktoren für die Lebenszufriedneheit wären
# neben Einkommen auch noch: Alter, Geschlecht, Gesundheit, Ost/West etc.

# Einkommen incc und Lebenszufriedenheit ls01 haben wir schon vorbereitet:
allbus_c_2023 %>%
  count(incc)
allbus_c_2023 %>%
  count(ls01)

# Nun also der Rest:

allbus_c_2023 %>%
  count(sex) # müssen wir dichotomisieren!

allbus_c_2023 %>%
  count(age) # missings raus

# Gesundheitszustand
allbus_c_2023 %>%
  count(hs01) # missings raus


allbus_c_2023 <- allbus_c_2023 %>%
  dplyr::mutate(
    # Geschlecht dichotomisieren: DIVERS (n = 18) auf NA
    sex_bi = haven::as_factor(dplyr::case_when(sex %in% c(1, 2) ~ sex, .default = NA)) %>%
      forcats::fct_drop(),
    # Alter: Missings raus
    age  = dplyr::case_when(age  < 0 ~ NA, .default = age),
    # Gesundheit: Missings raus (quasi-metrisch, 1-5, invers gepolt!)
    hs01 = dplyr::case_when(hs01 < 0 ~ NA, .default = hs01)
  )
# ggf. counts erneut durchführen um Änderungen zu prüfen

model_2 <- lm(ls01 ~ incc + age + sex_bi + hs01,
              data = allbus_c_2023,
              weights = wghtpew)

summary(model_2)

texreg::screenreg(list(model_1, model_2))

# schöner mit Beschriftungen:
texreg::screenreg(
  list(model_1, model_2),
  custom.coef.names = c("Intercept", "Einkommen (kat.)", "Alter",
                        "Geschlecht: Frau", "Gesundheit"),
  digits = 3
)

# Interpretation Modellvergleich:

# Modellgüte:
# R² steigt von 0.024 auf 0.182 - Modell 2 erklärt rund 18% der Varianz
# der Lebenszufriedenheit (statt 2,4%), vor allem dank Gesundheit.
# Noch nicht enthalten: Erwerbsstatus, Familienstand etc. - die hätten
# vermutlich ebenfalls Erklärungskraft.

# Effektgrößen:
# Einkommen: Effekt halbiert sich von 0.055 auf 0.027 unter Kontrolle
# der anderen UVs - ein Teil des ursprünglichen Effekts ging also auf
# Alter, Geschlecht und Gesundheit zurück.
# Alter: +0.021 Punkte Lebenszufriedenheit pro Lebensjahr, ceteris paribus.
# Geschlecht: Frauen im Schnitt 0.143 Punkte zufriedener als Männer -
# bei gleichem Einkommen, Alter und Gesundheit.
# Gesundheit: stärkster Prädiktor! Pro Stufe schlechterer Gesundheit
# sinkt die Zufriedenheit um 0.796 Punkte (Skala invers gepolt:
# 1 = sehr gut, 5 = schlecht).

# Signifikanz:
# Alle Prädiktoren sind signifikant (p < .01 bzw. p < .001).

# Fazit:
# Einkommen sagt Lebenszufriedenheit statistisch signifikant, aber
# praktisch kaum vorher - Gesundheit ist der mit Abstand wichtigste
# Prädiktor im Modell.
# NB: Ob Gesundheit wirklich "stärkster" Prädiktor ist, klärt erst
# der Vergleich standardisierter Koeffizienten (nächster Abschnitt)!



## standardisierte Koeffizienten --------------

# standardisierte Koeffizienten (beta): alle Variablen auf SD-Einheiten
# -> Effektstärken über UVs hinweg vergleichbar
# NB: bei Dummies (sex_bi) ist die Standardisierung schwer interpretierbar -
# dort besser beim unstandardisierten Koeffizienten bleiben.

# Variablen z-standardisieren und Modell neu schätzen
model_2_z <- lm(scale(ls01) ~ scale(incc) + scale(age) + sex_bi + scale(hs01),
                  data = allbus_c_2023,
                  weights = wghtpew)

summary(model_2_z)

texreg::screenreg(
  list(model_2, model_2_z),
  custom.model.names = c("M2 (b)", "M2 (beta)"),
  custom.coef.names = c("Intercept", "Einkommen (kat.)", "Alter",
                        "Geschlecht: Frau", "Gesundheit",
                        "Einkommen (kat.)", "Alter", "Gesundheit"),
  digits = 3
)
# Interpretation der standardisierten Koeffizienten (beta):
# "Steigt die UV um 1 Standardabweichung, ändert sich die AV um beta SDs"

# Gesundheit (beta = -0.42): mit Abstand stärkster Prädiktor -
# die Vermutung aus den unstandardisierten Werten bestätigt sich.

# Alter (beta = 0.20): zweitstärkster Prädiktor! Das war an den
# unstandardisierten Koeffizienten NICHT erkennbar (b = 0.021 wirkte
# winzig - aber Alter streut eben über ~80 Jahre).

# Einkommen (beta = 0.08): trotz unserer Forschungsfrage einer der
# schwächsten Prädiktoren im Modell.

# sex_bi (beta = 0.08): Vorsicht - Standardisierung bei Dummies ist
# schwer interpretierbar ("1 SD mehr Frau"?). Hier besser beim
# unstandardisierten Wert bleiben (0.143 Punkte Unterschied).

# Didaktischer Kernpunkt: Die Rangfolge der Effektstärken ändert sich
# durch Standardisierung! Unstandardisierte Koeffizienten sind NICHT
# über UVs hinweg vergleichbar.


# Schritt 4 Regressionsergebnisse darstellen! --------

# neben der Regressionstabelle, die immer berichtet werden sollte,
# gibt es zusätzliche, visuelle Darstellungsmöglichkeiten. Die häufigste
# und wahrscheinlich intuitivste iste der Koeffizientenplot (c)


# a) Regressionsgerade visualisieren
allbus_c_2023 %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = incc, y = ls01)) +
  ggplot2::geom_jitter(alpha = 0.3) +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::xlab("Einkommen (kat.)") + ggplot2::ylab("Lebenszufriedenheit")

# b) Interpretation in Worten (easystats)
model_2 %>% report::report()


# c) Koeffizientenplot: intuitivste Darstellung

# Schritt 1: broom::tidy() macht aus dem Modell einen Datensatz
broom::tidy(model_2, conf.int = TRUE)
# -> jede Zeile ein Koeffizient, mit KI-Grenzen (conf.low, conf.high)

# Schritt 2: diesen Datensatz plotten wie jeden anderen auch
broom::tidy(model_2, conf.int = TRUE) %>%
  dplyr::filter(term != "(Intercept)") %>%  # Intercept ausblenden
  dplyr::mutate(term = dplyr::case_when( # Variabelnenamen ausschreiben
    term == "incc"~ "Einkommen (kat.)",
    term == "age"~ "Alter",
    term == "sex_biFRAU" ~ "Geschlecht: Frau",
    term == "hs01"~ "Gesundheit"
  )) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = estimate, y = term)) + #
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  ggplot2::geom_errorbarh(mapping = ggplot2::aes(xmin = conf.low, xmax = conf.high),
                          height = 0.2) + # horizontalen Standardfehler
  ggplot2::geom_point() +
  ggplot2::scale_x_continuous(breaks = seq(-1, 0.5, by = 0.1)) +
  ggplot2::labs(
    title = "Was sagt Lebenszufriedenheit vorher?",
    x = "Koeffizient (b) mit 95%-KI",
    y = ""
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_line(color = "grey80"),
    panel.grid.minor.x = ggplot2::element_line(color = "grey90")
  )

# Interpretation:
# - Punkt = geschätzter Koeffizient, Balken = 95%-Konfidenzintervall
# - Schneidet der Balken die gestrichelte Nulllinie NICHT -> signifikant
# - Je weiter vom Nullpunkt entfernt, desto stärker der Effekt
#   (Achtung: nur bei standardisierten Koeffizienten direkt vergleichbar!)


# und hier einmal das standardisierte modell
broom::tidy(model_2_z, conf.int = TRUE) %>%
  dplyr::filter(term != "(Intercept)") %>%  # Intercept ausblenden
  dplyr::mutate(term = dplyr::case_when(
    term == "scale(incc)" ~ "Einkommen (kat.)",
    term == "scale(age)"  ~ "Alter",
    term == "sex_biFRAU"  ~ "Geschlecht: Frau",
    term == "scale(hs01)" ~ "Gesundheit"
  )) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = estimate, y = term)) + #
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  ggplot2::geom_errorbarh(mapping = ggplot2::aes(xmin = conf.low, xmax = conf.high),
                          height = 0.2) + # horizontalen Standardfehler
  ggplot2::geom_point() +
  ggplot2::scale_x_continuous(breaks = seq(-1, 0.5, by = 0.1)) +
  ggplot2::labs(
    title = "Was sagt Lebenszufriedenheit vorher?",
    x = "Koeffizient (b) mit 95%-KI",
    y = ""
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_line(color = "grey80"),
    panel.grid.minor.x = ggplot2::element_line(color = "grey90")
  )
# was hier deutlich wird: Alter hat den zweitgrößten Effekt



# Alternative: sjPlot - sehr beliebt in den Sozialwissenschaften
# install.packages("sjPlot")
sjPlot::plot_model(model_2, type = "std") + theme_linedraw()
# automatisch farbcodiert (positiv/negativ), mit vielen Optionen -
# z.B. type = "std" für standardisierte Koeffizienten direkt im Plot!

# Weitere Optionen (alle mit ähnlichem Ergebnis):
# - plot(parameters::model_parameters(model_2)) # easystats
# - modelsummary::modelplot(model_2) # flexibel, ggplot-basiert
# - jtools::plot_summs(model_1, model_2) # legt Modelle übereinander
# - dotwhisker::dwplot(list(model_1, model_2)) # der Klassiker

