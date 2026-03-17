# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# LÖSUNG ZU ÜBUNG 2: Datenimport                        -----------
# ______________________________________________________-----------


# Aufgabe 1
   # Lade dir die Wahlkreisstrukturdaten herunter und lege sie in deinen .data-Unterordner.
   # Siehe dafür auf der Kurswebseite unter dem Tab "Daten" nach.

# Aufgabe 2
   # Lade die nötigen Pakete: rio, labelled.
   # Lese die daten mir dem rio-Paket ein. Achte darauf, ggf. überflüssige Zeilen
   # zu überspringen. Sieh dir das Ergebnis an.

library(rio)
library(labelled)

btw_25_rws_raw <- rio::import("./data/btw25_rws_bst2-kombi.csv",
                              skip = 11)


# Aufgabe 3
   # Bereinige die Variablennamen.
btw_25_rws <- janitor::clean_names(btw_25_rws_raw)


# Aufgabe 4
   # Lass dir die Variablennamen ausgeben. Ändere sie ggf.
   # Füge den neuen Namen entsprechende Labels hinzu.

# variablennamen ausgeben lassen zum Kopieren
btw_25_rws %>%
  names()

# Liste bauen, in der wir den Namen ein Label zuordnen
labels_btw_25_rws <- list(
  land = "Bundesland",
  geschlecht = "Geschlecht der Person",
  geburtsjahresgruppe = "Geburtsjahresgruppe",
  partei_zweitstimme = "Gewählte Partei (Zweitstimme)",
  erststimme_summe = "Anzahl der abgegebenen Erststimmen",
  erststimme_ungultig = "Anzahl der ungültigen Erststimmen",
  erststimme_spd = "Anzahl der Erststimmen für die SPD",
  erststimme_cdu = "Anzahl der Erststimmen für die CDU",
  erststimme_grune = "Anzahl der Erststimmen für die Grünen",
  erststimme_fdp = "Anzahl der Erststimmen für die FDP",
  erststimme_af_d = "Anzahl der Erststimmen für die AfD",
  erststimme_csu = "Anzahl der Erststimmen für die CSU",
  erststimme_die_linke = "Anzahl der Erststimmen für die Linke",
  erststimme_sonstige = "Anzahl der Erststimmen für sonstige Parteien",
  erststimme_dar_freie_wahler = "Anzahl der Erststimmen für die Freien Wähler",
  erststimme_dar_bsw = "Anzahl der Erststimmen für das BSW"
)

# in der Liste definierte labels ankleben
btw_25_rws <- btw_25_rws %>%
  labelled::set_variable_labels(.labels = labels_btw_25_rws,
                                .strict = FALSE)



# Aufgabe 5 -------------------------------------------------------
   # Lass dir mit summarytools eine Datensazübersicht ausgeben.

btw_25_rws %>%
  summarytools::dfSummary(na.col = FALSE) %>%
  summarytools::view(file = "./data/btw_25_rws_summary.html")


