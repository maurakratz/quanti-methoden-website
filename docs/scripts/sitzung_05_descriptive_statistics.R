# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# SITZUNG 05 Kennwerte und deren Visualisierung         -----------
# ______________________________________________________-----------


# Nun, da wir gelernt haben: wie wir Datensätze einlesen; wie wir sie säubern,
# wie wir sie ggf. zusammenführen und uns einen Überblick verschaffen,
# Können sie uns bereits viele spannende, deskriptive, politikwissenschaftliche
# Fragen beantworten.


# 1 setup --------------------------------------

# Pakete
library(rio)
library(labelled)
library(janitor)
library(summarytools)
library(dplyr)
library(skimr)


# Daten
load("output/btw_2025_ergebnisse.RData")
load("output/btw_2025_strukturdaten.RData")
# load("output/btw_25_rws.RData")

# kurze Erinnerung: was war da nochmal drin
btw_2025_ergebnisse %>%
  dplyr::glimpse()

btw_2025_strukturdaten %>%
  dplyr::glimpse()

btw_25_rws %>%
  dplyr::glimpse()



# Wie ungleich ist Arbeitslosigkeit ueber Wahlkreise verteilt? --------------


# dazu erstellen wir erst einen subdatensatz nur mit den wahlkreisen
wk <- btw_2025_strukturdaten %>%
  dplyr::filter(wahlkreis_nr < 900)


# Alle Kennwerte auf einmal ausgeben
wk %>%
  select(alo_quote_insgesamt) %>%
  summary()

# alternativ mit skimr
wk %>%
  select(alo_quote_insgesamt) %>%
  skimr::skim()


# gezielt einzelne Werte ausgeben im tidyverse
wk %>%
  dplyr::summarise(
    n = dplyr::n(),
    min = min(alo_quote_insgesamt, na.rm = TRUE),
    max = max(alo_quote_insgesamt, na.rm = TRUE),
    range = max(alo_quote_insgesamt, na.rm = TRUE) - min(alo_quote_insgesamt, na.rm = TRUE),
    mittelwert = mean(alo_quote_insgesamt, na.rm = TRUE),
    median = median(alo_quote_insgesamt, na.rm = TRUE),
    sd = sd(alo_quote_insgesamt, na.rm = TRUE),
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

# Wie kann ich das nun visuell darstellen?
hist(wk$alo_quote_insgesamt)
boxplot(wk$alo_quote_insgesamt)

# Extremfälle identifzieren
wk %>%
  dplyr::select(wahlkreis_name, land, alo_quote_insgesamt) %>%
  dplyr::arrange(alo_quote_insgesamt) %>%
  head(10)

wk %>%
  dplyr::select(wahlkreis_name, land, alo_quote_insgesamt) %>%
  dplyr::arrange(alo_quote_insgesamt) %>%
  tail(10)


# Jetzt ihr: Wo war die Wahlbeteiligung bei der Bundestagswahl 2025
# am höchsten und wo am niedrigsten?



# R-Code in Quarto einbetten -------------------
install.packages("tinytex")
tinytex::install_tinytex()

install.packages("here")
library(here)

# here::here() sorgt dafür, dass R auch in quarto immer im project root folder
# beginnt, unabhängig davon, wo die .qmd-Datei liegt.
# beispielsweise:
load(here::here("output/btw_2025_strukturdaten.RData"))
# statt
load("output/btw_2025_strukturdaten.RData")


# Lade dir die Beispiel-.qmd-file von der Kurswebseite herunter.
# Lege Sie in deinen slides-Ordner und öffne sie im Kurs-RProject.
# Klicke auf render
# Nun sollte in demselben Ordner eine .pdf-Datei liegen.
# Sieh sie dir an.
# Melde dich, wenn etwas nicht klappt!
