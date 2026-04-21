# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# ÜBUNG 2: Data Wrangling                               -----------
# ______________________________________________________-----------

# Aufgabe 1 ------------------
   # Lade die nötigen Pakete (rio, dplyr, tidyr)
library(rio)
library(dplyr)
library(tidyr)

load("output/btw_2025_strukturdaten.RData")


# Aufgabe 2 -----------
   # Benenne die folgenden Variablen um: land um in bula; wahlkreis_nr in wknr
   # und wahlkreis_name in wkname. Denk daran die Änderung zu speichern, also
   # den alten Datensatz zu überschreiben.

btw_2025_strukturdaten <- btw_2025_strukturdaten %>%
  dplyr::rename(
    bula = land,
    wknr = wahlkreis_nr,
    wkname = wahlkreis_name
  )


# Aufgabe 3 ------------------
   # Schließe alle wohnungsbezogenen Variablen sowie die Variable "fussnoten" aus.
   # Überschreibe dafür den Datensatz.

btw_2025_strukturdaten <- btw_2025_strukturdaten %>%
  dplyr::select (-starts_with("wohn"),
                 -fussnoten)

# Aufgabe 4 ------------------
   # Schau dir die Daten in einem RStudio-Tab an. Welche Nummern ("wknr") stehen
   # für die Bundesländer, welche für Gesamtdeutschland? Filtere diese Zeilen
   # heraus und speichere sie in einem neuen Datensatz namens "btw_2025_struk_lander".

btw_2025_struk_lander <- btw_2025_strukturdaten %>%
  dplyr::filter(wknr > 900)


# Aufgabe 5 ------------------
   # Lass dir für jedes Bundesland die Fläche, Einwohnerzahl und Bevölkerungsdichte
   # in der Console anzeigen.

btw_2025_struk_lander %>%
  dplyr::select(bula, wknr, flache_km2, bev_insgesamt_1000, bev_dichte_ew_km2)

   # Welches Bundesland ist am dichtesten besiedelt?
   # Welches ist am größten?
   # Welches hat die meisten Einwohner und wieviele?


