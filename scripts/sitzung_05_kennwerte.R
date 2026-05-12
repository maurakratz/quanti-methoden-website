# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# SITZUNG 05 Kennwerte und deren Visualisierung         -----------
# ______________________________________________________-----------


# Nun, da wir gelernt haben: wie wir Datensätze einlesen; wie wir sie säubern,
# wie wir sie ggf. zusammenführen und uns einen Überblick verschaffen,
# Können sie uns bereits viele spannende, deskriptive, politikwissenschaftliche
# Fragen beantworten.

# Solche Fragen kreisen meistens entweder a) um die Verteilung von Merkmalen
# oder b) um den Zusammenhang zwischen Merkmalen (dazu mehr in Sitzung 08).

# Zum Beispiel:
 # Wie ungleich ist Arbeitslosigkeit über Wahlkreise verteilt?
 # ...



# 00 setup --------------------------------------

# Pakete
library(dplyr)
library(skimr)
library(summarytools)


# Daten
load("output/btw_2025_ergebnisse.RData")
load("output/btw_2025_strukturdaten.RData")
# siehe Sitzung 2 und Lösung zu Übung 1, falls du die .RData-Datensätze
# nicht finden kannst oder nicht weißt, wie du sie erstellen kannst.


# kurze Erinnerung: was war da nochmal drin
btw_2025_ergebnisse %>%
  dplyr::glimpse()

btw_2025_strukturdaten %>%
  dplyr::glimpse()



# 01 Kennwerte mit skimr::skim() ----------

# Frage: Wie ungleich ist Arbeitslosigkeit über Wahlkreise verteilt?


# dazu erstellen wir erst einen Subdatensatz nur mit den Wahlkreisen
wk <- btw_2025_strukturdaten %>%
  dplyr::filter(wahlkreis_nr < 900)

# Alle Kennwerte zur Arbeitslosenquote auf einmal ausgeben

# der spartanische baseR-summary() Befehl
wk %>%
  dplyr::select(alo_quote_insgesamt) %>%
  summary()

# alternativ der vollständigere summarytools::descr()
wk %>%
  dplyr::select(alo_quote_insgesamt) %>%
  summarytools::descr()

# oder aber mit skimr (was in pdfs oft besser aussieht, siehe unten)
wk %>%
  dplyr::select(alo_quote_insgesamt) %>%
  skimr::skim()



# 02 Kennwerte mit dplyr::summarize() -----------

# gezielt einzelne Kennwerte zur Arbeitslosigkeit ausgeben mit dplyr::summarise()
wk %>%
  dplyr::summarise(
    n = dplyr::n(), # NB: das tut dasselbe wie count()
    min = min(alo_quote_insgesamt, na.rm = TRUE),
    max = max(alo_quote_insgesamt, na.rm = TRUE),
    range = max(alo_quote_insgesamt, na.rm = TRUE) - min(alo_quote_insgesamt, na.rm = TRUE),
    mittelwert = mean(alo_quote_insgesamt, na.rm = TRUE),
    sd = sd(alo_quote_insgesamt, na.rm = TRUE),
    median = median(alo_quote_insgesamt, na.rm = TRUE),
    mad = mad(alo_quote_insgesamt, na.rm = TRUE),
    q25 = quantile(alo_quote_insgesamt, probs = 0.25, na.rm = TRUE),
    q75 = quantile(alo_quote_insgesamt, probs = 0.75, na.rm = TRUE),
    iqr = IQR(alo_quote_insgesamt, na.rm = TRUE)
  ) %>%
  dplyr::tibble() # stattdessen t() , also transpose, für Werte untereinander

# Was sagt uns das jetzt?
   # Die durchschn. Arbeitslosigkeit liegt bei etwa 6% in deutschen Wahlkreisen
   # im Durchschnitt weicht ein Wahlkreis ca. 2 PP vom Mittelwert ab => viel

   # Median ist etwas kleiner als der Mittelwert  => Ausreißer nach oben,
   # aber keine zu starke Verzerrung.

   #  zwischen dem Wahlkreis mit der niedrigsten (2.5%) und höchsten (14.8%)
   # Arbeitslosenquote liegen fast 12 Prozentpunkte => erhebliche Ungleichheit
   # die mittleren 50% der Wahlkreise liegen zwischen 4.3% und 7.3%
   # (IQR 3 Prozentpunkte) => Abweichungen sind Ausreißer, nicht die Regel


# Extremfälle identifzieren: Wer sind denn diese Ausreißer?
wk %>%
  dplyr::select(wahlkreis_name, land, alo_quote_insgesamt) %>%
  dplyr::arrange(alo_quote_insgesamt) %>%
  head(10)

wk %>%
  dplyr::select(wahlkreis_name, land, alo_quote_insgesamt) %>%
  dplyr::arrange(alo_quote_insgesamt) %>%
  tail(10)


# 03 Gruppieren mit dplyr::group_by() ----------

# Kennwerte zu Arbeitslosenquote pro Land

# mit skimr::skim() für den schnellen Überblick
wk %>%
  dplyr::group_by(land) %>%
  skimr::skim(alo_quote_insgesamt) %>%
  dplyr::select(-n_missing, -complete_rate) # missing und completion rate ausschließen


# mit dplyr::summarize() zum Weiterarbeiten
alo_bula <- wk %>%
  dplyr::group_by(land) %>%
  dplyr::summarise(
    n = dplyr::n(),
    mittelwert = mean(alo_quote_insgesamt, na.rm = TRUE),
    sd = sd(alo_quote_insgesamt, na.rm = TRUE),
  )



# 04 Visualisierung mit BaseR -------------------

# Wie kann ich Lage- und Streuungsmaße nun visuell darstellen?


# Boxplot: Wie ist die Arbeitslosigkeit über WK bundesweit verteilt?
boxplot(wk$alo_quote_insgesamt)

# Wie ist sie innerhalb der Bundesländer verteilt?
boxplot(alo_quote_insgesamt ~ land, data = wk)


#_________-----

# 05 R-Code in Quarto einbetten -------------------

# Quarto ist ein Dokumentformat, das R-Code und Text kombiniert.
# Aus einer solchen .qmd-Datei lassen sich PDF, HTML oder Word-Dokumente "rendern".
# Damit ihr allerdings PDFs erstellen könnt, müsst ihr eine LaTeX-Engine
# installiert haben. Das geht z.B. so:

## LaTeX-Engine installieren ------

# Einmalig installieren (nur einmal nötig, danach mit # auskommentieren):
install.packages("tinytex")
tinytex::install_tinytex()
# danach ggf. RStudio neu starten


## here-Paket installieren --------

# here::here() löst ein häufiges Problem in Quarto:
# Quarto sucht Dateien relativ zur .qmd-Datei, nicht zum Projektordner.
# here::here() verweist immer auf den Projektordner (.Rproj-Datei).
# Das bedeutet, dass ihr in euren .qmd-Dateien immer mit here::here() arbeiten
# solltet. Also, so:
   # load(here::here("output/btw_2025_strukturdaten.RData"))
# statt so
   # load("output/btw_2025_strukturdaten.RData")

# Einmalig installieren (nur einmal nötig, danach mit # auskommentieren):
install.packages("here")
library(here)


# Jetzt seid ihr dran:
# 1. Ladet die Beispiel-.qmd-Datei von der Kurswebseite herunter
# 2. Legt sie in euren Projektordner und öffnet sie im Kurs-RProject
# 3. Klickt auf Render
# 4. Im selben Ordner sollte nun eine .pdf-Datei liegen
# 5. Meldet euch, wenn etwas nicht klappt!
