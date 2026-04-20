# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# ÜBUNG 2: Data Wrangling                                 -----------
# ______________________________________________________-----------

# load packages and data
library(rio)
library(dplyr)
library(tidyr)

load("output/btw_2025_strukturdaten.RData")


# 1) Benenne die folgenden Variablen um: land um in bula; wahlkreis_nr in wknr
   # und wahlkreis_name in wkname. Denk daran die Änderung zu speichern, also
   # den alten Datensatz zu überschreiben.

btw_2025_strukturdaten <- btw_2025_strukturdaten %>%
  dplyr::rename(
    bula = land,
    wknr = wahlkreis_nr,
    wkname = wahlkreis_name
  )


# Schließe alle wohnungsbezogenen Variablen aus

btw_2025_strukturdaten <- btw_2025_strukturdaten %>%
  dplyr::select (-starts_with("wohn"))


# schau dir fläche, bevölkerung und bevölkerungsdichte für alle Bundesländer an.
   # Tipp: Nutze dafür erst select(), dann filter (%in%).

btw_2025_strukturdaten %>%
  dplyr::select(bula, wknr, flache_km2, bev_insgesamt_1000, bev_dichte_ew_km2) %>%
  dplyr::filter(wknr > 900)


# Wie war die Wahlbeteiligung bei der Bundestagswahl 2025 in Bayern vs. in Berlin?
