# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# SITZUNG 02                                            -----------
# ______________________________________________________-----------

# klassischer workflow:
   # 1. Pakete laden
   # 2. Daten importieren mit rio::import()
   # 3. Variablennamen anpassen mit janitor::clean_names()
   # 4. OPTIONAL  Falls es variable labels gibt, diese  einmal in eine Liste
     # extrahieren mit labelled:var_label()
     # dann mit labelled::set_variable_labels() wieder ankleben
     # alternativ selber einzen definieren mit labelled::set_variable_label(varname = "varlabel")
   # 5. Daten anschauen mit summarytools::dfSummary() %>% view()


# 1 setup --------------------

# work directory überprüfen
getwd()

# Pakete installieren und laden
library(rio) # Daten einlesen
library(janitor) # Variablennamen bereinigen
library(summarytools) # Überblick über Datensätze
library(dplyr) # %>%
library(labelled) # labels setzen und ankleben

# denke daran, die Pakete ggf. vorher zu installieren: install.packages("rio")
# um dich mit den Funktionen vertraut zu machen, kannst du deren CRAN oder
   # RDocumentation-pages über deine Suchmaschine im Browser finden
# oder du nutzt die eingebaute Dokumentation
?rio


# 2. Daten importieren --------------------

# Daten können in unterschiedlichebn Formaten vorliegen, z.B.
   # .xlsx (Excel)
   # .csv (comma-separated-values)
   # .txt-Format(text file)
   # .sav (SPSS)
   # .dta-Format (Stata)
# das Tolle am rio-package ist, dass es all die unterschiedlichen Formate (für
   # die sonst unterschiedliche Pakete gebraucht werden) mit einem einzigen
   # Befehl importieren kann.
# Daten, die mit R selbst erstellt wurden (.RData) kann R-Studio mit base R,
   # also ohne Zusatzpakete einlesen mit load()


# 2 Einlesen der Wahlergebnisse der BTW 2025 ----------

  btw_2025_ergebnisse_raw <- rio::import("./data/kerg2.csv")
  # Da ist etwas schief gelaufen. Deutsche .csv-Dateien sind oft mit ";" getrennt
  # und englische mit ",". Das können wir mit dem "sep"-Argument anpassen.
  # Auch den Dezimaltrenner müssen wir auf's Deutsche System auslegen:

  btw_2025_ergebnisse_raw <- rio::import("./data/kerg2.csv",
                                         sep = ";",
                                         dec = ",")
  # Hier ist noch der header drin, den wir überspringen müssen.

  btw_2025_ergebnisse_raw <- rio::import("./data/kerg2.csv",
                                         sep = ";",
                                         dec = ",",
                                         skip = 9)
    # mit dem skip-Argument haben wir nun die ersten 9 Zeilen abgeschnitten.
    # TIPP: Bei der allerersten Version ein _raw dahintersetzen, damit klar ist,
    # dass das die Rohdaten sind.


# 3 Variablennamen bereinigen mit janitor ---------------------------

  btw_2025_ergebnisse <- btw_2025_ergebnisse_raw %>%
    janitor::clean_names()
    # die Funktion clean_names() bereinigt die Variablennamen, indem sie z.B
    # Sonderzeichen entfernt, Leerzeichen durch Unterstriche ersetzt und alle
    # Buchstaben in Kleinbuchstaben umwandelt.


  # SNEAK PREVIEW: Ist in Bemerkung etwas wichtiges vermerkt?
  btw_2025_ergebnisse %>%
    dplyr::distinct(bemerkung)

  # Nein ,also weg damit:
  btw_2025_ergebnisse <- btw_2025_ergebnisse %>%
    dplyr::select (-bemerkung)


# 4 Umgang mit Labels -------------------------

  # variablennamen ausgeben lassen zum Kopieren
  btw_2025_ergebnisse %>%
      names()

  # Liste mit Labels bauen
  labels_btw_2025_ergebnisse <- list(
    wahlart = "Wahlart: Bundes, -Landes, -Kommunalwahl ",
    wahltag = "Datum des Wahltags",
    gebietsart = "Gebietsart: Bundesland, Wahlkreis, oder gesamtes Staatsgebiet",
    gebietsnummer = "Gebietsnummer",
    gebietsname = "Gebietsname",
    ueg_gebietsart = "Übergeordnete Gebietsart",
    ueg_gebietsnummer = "Übergeordnete Gebietsnummer",
    gruppenart = "Gruppenart: Partei, Einzelbewerber, System",
    gruppenname = "Gruppenname: z.B. Parteiname",
    gruppenreihenfolge = "Reihenfolge der Gruppe",
    stimme = "Stimmart (Erst-/Zweitstimme)",
    anzahl = "Anzahl Stimmen",
    prozent = "Stimmenanteil in Prozent",
    vorp_anzahl = "Anzahl Stimmen bei vorherangegangener Wahl",
    vorp_prozent = "Stimmenanteil bei vorherangegangener Wahl in Prozent",
    diff_prozent = "Veränderung zur vorherangegangenen Wahl in Prozent",
    diff_prozent_pkt = "Veränderung zur vorherangegangenen Wahl in Prozentpunkten",
    gewahlt = "Gewählt: Pateiname"
  )


  # Labels anbringen
  btw_2025_ergebnisse <- btw_2025_ergebnisse %>%
    labelled::set_variable_labels(.labels = labels_btw_2025_ergebnisse,
                                  .strict = FALSE)


# 3. Überblick über die Daten ----------------------------

  # Ausprägungen einer Variable anzeigen
  btw_2025_ergebnisse %>%
    distinct(gewahlt)

  btw_2025_ergebnisse %>%
    count(gewahlt)

  # Überblick über den gesamten Datensatz
  btw_2025_ergebnisse %>%
    summarytools::dfSummary() %>%
    summarytools::view()

  # Da "missing" und "valid" redundante Spalten sind, können wir eine davon ausschalten
  btw_2025_ergebnisse %>%
    summarytools::dfSummary(missing = FALSE) %>%
    summarytools::view()

  # Huch! Sie ist ja immernoch da. Da schauen wir doch mal nach,
  # warum das nicht funktioniert hat:
  ?summarytools::dfSummary

  # Aha, da steht, dass wir das Argument "na.col" auf FALSE setzen müssen,
  # um die Spalte mit den fehlenden werten auszublenden, nicht "missing".

  btw_2025_ergebnisse %>%
    summarytools::dfSummary(na.col = FALSE) %>%
    summarytools::view()

  # das speichere ich mir in meinem Data-Ordner ab:
  btw_2025_ergebnisse %>%
    summarytools::dfSummary(na.col = FALSE) %>%
    summarytools::view(file = "./data/btw_2025_ergebnisse_uebersicht.html")






