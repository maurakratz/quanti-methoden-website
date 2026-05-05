# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# ÜBUNG 3: Data Merging                                 -----------
# ______________________________________________________-----------


# Aufgabe 1 ------------------------
   # Lade dir die repräsentative Wahlstatisik herunter ("Stimmabgabe nach
   # Geschlecht und Geburtsjahresgruppen", btw25_rws_bst2.csv) und lege sie
   # in deinen .data-Unterordner. Siehe dafür auf der Kurswebseite unter
   # dem Tab "Daten" nach. Lies dir auch die allgemeinen methodischen Hinweise
   # durch und lege sie als pdf im selben Ordner ab.


# Aufgabe 2 --------------------------
   # Lade die nötigen Pakete: rio, janitor, labelled, summarytools und dplyr.
   # Lese die Daten mir dem rio-Paket ein. Achte darauf, ggf. überflüssige Zeilen
   # zu überspringen. Sieh dir das Ergebnis an.

library(rio)
library(labelled)
library(janitor)
library(summarytools)
library(dplyr)

btw_25_rws_raw <- rio::import("./data/btw25_rws_bst2.csv",
                              sep = ";",
                              dec = ",",
                              skip = 11)

# Aufgabe 3 ------------------------------
   # Bereinige die Variablennamen.
btw_25_rws <- btw_25_rws %>%
  janitor::clean_names(btw_25_rws_raw)

btw_25_rws %>%
  names()

# Aufgabe 4 ---------------------------
   # Füge mit labelled::set_variable_labels() den Variablennamen
   # entsprechende Labels hinzu.

# auch hier sind die alten Namen als Labels brauchbar (siehe Lösung zu Übung 1).
# also speichern wir sie in einem named vector
btw_25_rws_labels <- setNames(
  object = names(btw_25_rws_raw), # die alten Namen werden zu names
  nm = names(btw_25_rws) # neuen Spaltennamen werden zu values
)

btw_25_rws_labels

btw_25_rws <- btw_25_rws %>%
  labelled::set_variable_labels(.labels = btw_25_rws_labels,
                                .strict = FALSE)



# Aufgabe 5 ----------------------------
   # Lass dir mit summarytools eine Datensazübersicht ausgeben und
   # speichere sie im output-Ordner.

btw_25_rws %>%
  summarytools::dfSummary() %>%
  summarytools::view(file = "output/btw_25_rws_dfSummary.html")


# Aufgabe 6 --------------------
   # a) Lade nun ebenfalls den Strukturdatensatz ein.
     # Dazu kannst du entweder den entsprechenden Code-Abschnitt
     # (import, clean_names etc.) hierher kopieren, oder aber die .RData-Datei
     # mit load() direkt laden. Beachte, dass du sie für letzteres
     # zuvor mit save() gespeichert haben musst.

load("./output/btw_2025_strukturdaten.RData")


   # b) Füge nun der repräsentativen Wahlstatistik die Strukturvariablen aus dem
     #  Strukturdatensatz mit einem left-join()-Befehl hinzu.

     # Sieh dir beide Datensätze an: Die Strukturdaten liegen pro Wahlkreis,
     # pro Bundesland und für Gesamtdeutschland vor,
     # die repräsentative Wahlstatistik nur pro Bundesland und für Gesamtdeutschland.
     # Filtere also zunächst die Strukturdaten, sodass nur die Zeilen übrig bleiben,
     # die Angaben zu Bund und Ländern enthalten. Speichere diese Zeilen in einem
     # neuen Objekt, das du strukturdaten_merge nennst.

strukturdaten_merge <- btw_2025_strukturdaten %>%
  dplyr::filter(wahlkreis_nr > 900)


     # Auch sind die Bundesländer unterschiedlich kodiert: In der Variable land in
     # strukturdaten_merge sind sie ausgeschrieben, in btw_25_rws nicht.
     # Ändere also die Ausprägungen von land in btw_25_rws entsprechend um,
     # sodass sie mit den Angaben in strukturdaten_merge übereinstimmen.
     # (Das kannst du mit case_when() machen.)

btw_25_rws <- btw_25_rws %>%
  dplyr::mutate(
    land = dplyr::case_when(
      land == "Bund" ~ "Deutschland",
      land == "SH" ~ "Schleswig-Holstein",
      land == "MV" ~ "Mecklenburg-Vorpommern",
      land == "HH" ~ "Hamburg",
      land == "NI" ~ "Niedersachsen",
      land == "HB" ~ "Bremen",
      land == "BB" ~ "Brandenburg",
      land == "ST" ~ "Sachsen-Anhalt",
      land == "BE" ~ "Berlin",
      land == "NW" ~ "Nordrhein-Westfalen",
      land == "SN" ~ "Sachsen",
      land == "HE" ~ "Hessen",
      land == "TH" ~ "Thüringen",
      land == "RP" ~ "Rheinland-Pfalz",
      land == "BY" ~ "Bayern",
      land == "BW" ~ "Baden-Württemberg",
      land == "SL" ~ "Saarland"
    )
  )


     # Nun, da die Datenstrukturen zueinander passen: Führe sie auf Grundlage
     # der land-Variable in beiden Datensätzen mit einem join-Befehl zusammen.

btw_25_rws_struk <- btw_25_rws %>%
  dplyr::left_join(strukturdaten_merge,
                   by = "land")

     # Sieh dir das Ergebnis an. Erstelle und speichere eine Übersicht
     # des neuen Datensatzes. Speichere auch den Datensatz selbst als .RData-Datei.

