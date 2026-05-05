# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# ÜBUNG 3: Data Merging                                 -----------
# ______________________________________________________-----------


# Aufgabe 1 ------------------------
   # Lade dir die repräsentative Wahlstatisik herunter ("Stimmabgabe nach
   # Geschlecht und Geburtsjahresgruppen", btw25_rws_bst2.csv). Lege sie
   # in deinen .data-Unterordner. Siehe dafür auf der Kurswebseite unter
   # dem Tab "Daten" nach. Lies dir auch die allgemeinen methodischen Hinweise
   # durch und lege sie als pdf im selben Ordner ab.


# Aufgabe 2 --------------------------
   # Lade die nötigen Pakete: rio, labelled.
   # Lese die Daten mir dem rio-Paket ein. Achte darauf, ggf. überflüssige Zeilen
   # zu überspringen. Sieh dir das Ergebnis an.

library(rio)
library(labelled)

btw_25_rws_raw <- rio::import("./data/btw25_rws_bst2.csv",
                              sep = ";",
                              dec = ",",
                              skip = 11)

# Aufgabe 3 ------------------------------
   # Bereinige die Variablennamen.
btw_25_rws <- janitor::clean_names(btw_25_rws_raw)

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
   # Lass dir mit summarytools eine Datensazübersicht ausgeben.

btw_25_rws %>%
  summarytools::dfSummary() %>%
  summarytools::view(file = "output/btw_25_rws_dfSummary.html")


# Aufgabe 6 --------------------
   # a) Lade nun ebenfalls den Datensatz mit Strukturdaten ein.
     # Dazu kannst du entweder den entsprechenden Code (import, clean_names etc.)
     # hierher kopieren, oder aber die Daten mit load() direkt laden. Beachte,
     # dass du ihn für letzteres mit save() gespeichert haben musst.

load("./output/btw_2025_strukturdaten.RData")


   # b) Füge nun der repräsentativen Wahlstatistik die Strukturvariablen aus dem
     #  Strukturdatensatz mit einem left-join hinzu.

# Die Strukturdaten liegen ja pro Wahlkreis, Land oder Bund vor, die RWS nur pro Bundesland und Bund
# Also zunächst strukturdaten filtern, sodass nur die Zeiöen bleiben, die Angaben zu Bund und Ländern enthalten
# speichere das in einem subdatensatz den du strukturdaten merge nennst.
strukturdaten_merge <- btw_2025_strukturdaten %>%
  dplyr::filter(wahlkreis_nr > 900)


# Auch die Bundesländer sind unterschiedlich kodiert. in strukturdaten land sind
# die ausgeschriebne in rws nicht. schreibe sie also aus. Bennene auch Bund in Deutschland um.

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


# nun passen die daten zueinander und wir können sie auf grundlage der land variable zusammenführen

btw_25_rws_struk <- btw_25_rws %>%
  dplyr::left_join(strukturdaten_merge,
                   by = "land")



     # Sieh dir das Ergebnis an.
     # Erstelle und speichere eine Übersicht des neuen Datensatzes. Speichere auch den Datensatz selbst mit als .RData Datei

