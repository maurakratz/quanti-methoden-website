# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# SITZUNG 04 Variable Classes & Dezimaltrennzeichen     -----------
# ______________________________________________________-----------


load(file = "output/btw_2025_erg_struk.RData")


# 1. Variable Classes in R ---------------------------------------------

# varaible class herausfinden für alle variablen
sapply(btw_2025_erg_struk, class)


# variable class ändern
btw_2025_erg_struk <- btw_2025_erg_struk %>%
  dplyr::mutate(
    bev_dichte_ew_km2 = as.numeric(bev_dichte_ew_km2)
  )
# huch! alle wurden zu NA!
# Warum?
# weil die Variable bev_dichte_ew_km2 ursprünglich als character eingelesen wurde,
   # da sie ein Komma als Dezimaltrennzeichen enthielt. Das as.numeric() konnte
   # das Komma nicht interpretieren und hat daher alle Werte in NA umgewandelt.

# also muss ich zunächst das Komma durch einen Punkt ersetzen, damit R die Werte
   # als numerisch interpretieren kann.

# dazu erst einmal neu einlesen, um die nun fehlerhafte Version erneut zu überschreiben
load(file = "output/btw_2025_erg_struk.RData")


# Dezimaltrennzeichen von , zu .
library(readr)
btw_2025_erg_struk <- btw_2025_erg_struk %>%
  dplyr::mutate(
    dplyr::across(
      c(5:52), # POSITIONEN ÜBERPRÜFEN!!
      ~ readr::parse_number(., locale = readr::locale(decimal_mark = ","))
    )
  )

btw_2025_erg_struk <- btw_2025_erg_struk %>%
  dplyr::mutate(
    dplyr::across(
      c(5:52), # POSITIONEN ÜBERPRÜFEN
      ~ as.numeric(gsub(",", ".", .))
    )
  )
