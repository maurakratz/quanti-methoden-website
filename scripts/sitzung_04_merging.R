# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# SITZUNG 04 Data merging & Quarto                      -----------
# ______________________________________________________-----------


# 1 setup --------------------

# work directory überprüfen
getwd()

# Pakete installieren und laden
library(rio) # daten einlesen
library(dplyr) # %>%


# 2. Daten importieren --------------------

# Dazu müssen wir unsere bereits vorbereiteten Datensätze speichern und einlesen.
   # Führe das Skript aus letzter Sitzung komplett aus. Setze dann am unteren Ende
   # folgendes Befehl ein: save(btw_2025_ergebnisse, file = "data/btw_2025_ergebnisse.RData").
   # Öffne anschließend das Lösungsskript zur ersten Übung. Ergänze auch dort ganz
   # unten: save(btw_2025_strukturdaten, file = "data/btw_2025_strukturdaten.RData").
   # Führe dann das gesamte Skript aus.

# Nun können wir beide hier mit dem base R Befehl load() einlesen.
load("data/btw_2025_ergebnisse.RData")
load("data/btw_2025_strukturdaten.RData")


# 3. Daten mergen --------------------

# Beide Datensätze enthalten Angaben zur Wahlkreisnummer.

btw_2025_ergebnisse %>%
  distinct(gebietsnummer, gebietsart, gebietsname)
   # dabei steht 99 hier für das Bundesgebiet.

btw_2025_strukturdaten %>%
  distinct(wahlkreis_nr, wahlkreis_name)
   # Hier zeigen die Nummern über 900 Aggregatzahlen an, für die Bundesländer und
   # das gesamte Bundesgebiet. Wahlkreise tragen die Nummern 1-299.

# Insgesamt ist das Problem, dass zwar beide Datensätze Angaben zu a) Wahlkreisen, b) Bundes-
   # ländern und c) dem Bundesgebiet enthalten, diese Daten aber im Strukturdatensatz
   # in einer einzigen Variable stecken, im Ergebnisdatensatz hingegen in drei
   # verschiedenen: gebietsnummer, gebietsart, und gebietsname.

# Um also die Strukturdaten den Ergebnisdaten hinzufügen zu können, müssen wir
   # deren Struktur an jene des Ergebnisdatensatzes anpassen:

# Strukturdaten für Merge vorbereiten
btw_2025_strukturdaten_merge <- btw_2025_strukturdaten %>%
  dplyr::mutate(
    gebietsart = dplyr::case_when( # neue Variable Gebietsart erstellen
      wahlkreis_nr == 999 ~ "Bund",
      wahlkreis_nr >= 901 ~ "Land",
      TRUE ~ "Wahlkreis"
    ),
    wahlkreis_nr = dplyr::case_when( # die nummern an jene im ergebnisdatensatz anpassen
      wahlkreis_nr == 999 ~ 99,
      wahlkreis_nr == 901 ~ 1,
      wahlkreis_nr == 902 ~ 2,
      wahlkreis_nr == 903 ~ 3,
      wahlkreis_nr == 904 ~ 4,
      wahlkreis_nr == 905 ~ 5,
      wahlkreis_nr == 906 ~ 6,
      wahlkreis_nr == 907 ~ 7,
      wahlkreis_nr == 908 ~ 8,
      wahlkreis_nr == 909 ~ 9,
      wahlkreis_nr == 910 ~ 10,
      wahlkreis_nr == 911 ~ 11,
      wahlkreis_nr == 912 ~ 12,
      wahlkreis_nr == 913 ~ 13,
      wahlkreis_nr == 914 ~ 14,
      wahlkreis_nr == 915 ~ 15,
      wahlkreis_nr == 916 ~ 16,
      TRUE ~ wahlkreis_nr
    )
  )

# Merge durchführen
btw_2025_erg_struk <- btw_2025_ergebnisse %>%
  dplyr::left_join(btw_2025_strukturdaten_merge,
                   by = c("gebietsnummer" = "wahlkreis_nr", "gebietsart" = "gebietsart")
  )

# speichern
save(btw_2025_erg_struk, file = "data/btw_2025_erg_struk.RData")
