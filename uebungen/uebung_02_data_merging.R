# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# ÜBUNG 2: Data Merging                                 -----------
# ______________________________________________________-----------


# Aufgabe 1 ------------------------
   # Lade dir die repräsentative Wahlstatisik herunter ("Wahlberechtigte, Wählende
   # und Wahlbeteiligung nach Geschlecht und Geburtsjahresgruppen", btw25_rws_bst2.csv).
   # Lege sie in deinen .data-Unterordner. Siehe dafür auf der Kurswebseite unter
   # dem Tab "Daten" nach. Lies dir auch die allgemeinen methodischen Hinweise
   # durch und lege sie als pdf im selben Ordner ab.

# Aufgabe 2
   # Lade die nötigen Pakete: rio, labelled.
   # Lese die Daten mir dem rio-Paket ein. Achte darauf, ggf. überflüssige Zeilen
   # zu überspringen. Sieh dir das Ergebnis an.

library(rio)
library(labelled)

btw_25_rws_raw <- rio::import("./data/btw25_rws_bst2.csv",
                              skip = 11)

# Aufgabe 3
   # Bereinige die Variablennamen.
btw_25_rws <- janitor::clean_names(btw_25_rws_raw)

btw_25_rws %>%
  names()

# Aufgabe 4
   # Füge mit labelled::set_variable_labels() den Variablennamen
   # entsprechende Labels hinzu.

# auch hier sind die alten Namen als Labels brauchbar
# also speichern wir sie in einem named vector
btw_25_rws_labels <- setNames(
  names(btw_25_rws_raw), # die alten Namen werden zu names
  names(btw_25_rws) # neuen Spaltennamen werden zu values
)

btw_25_rws_labels

btw_25_rws <- btw_25_rws %>%
  labelled::set_variable_labels(.labels = btw_25_rws_labels,
                                .strict = FALSE)

# Aufgabe 5
   # Lass dir mit summarytools eine Datensazübersicht ausgeben.

btw_25_rws %>%
  summarytools::dfSummary() %>%
  summarytools::view()

# Aufgabe 6 --------------------
   # a) Lade nun ebenfalls den vergangene Sitzung vorbereiteten Datensatz zu
     # den Wahlergebnissen plus Strukturdaten ein.
     # (Ggf. musst du sie im Skript zu Sitzung 3 noch als .RData speichern).

load("./data/btw_2025_erg_struk.RData")

   # b) Füge nun der repräsentativen Wahlstatistik folgende Daten aus dem
     #  Ergebnisse+Strukturdaten-Datensatz mit einem inner join hinzu:
     # Beachte: die Bundesländer sind unterschiedlich kodiert. Und du möchtest nur die
     # Strukturdaten für die 16 Bundesländer, nicht die für die 299 Wahlkreise.





     # Sieh dir das Ergebnis an.
     # Erstelle und speichere eine Übersicht des neuen Datensatzes. Speichere auch den Datensatz selbst mit als .RData Datei
