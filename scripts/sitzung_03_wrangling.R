# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# SITZUNG 03 Data wrangling                             -----------
# ______________________________________________________-----------


# 1 setup --------------------

# work directory überprüfen
getwd()

# Pakete installieren und laden
library(dplyr)
library(tidyr)



# 2. Daten importieren --------------------

# Dazu müssen wir unsere bereits vorbereiteten Datensätze speichern und einlesen.
# Führe das Skript aus letzter Sitzung komplett aus. Setze dann am unteren Ende
# folgendes Befehl ein: save(btw_2025_ergebnisse, file = "data/btw_2025_ergebnisse.RData").
# Öffne anschließend das Lösungsskript zur ersten Übung. Ergänze auch dort ganz
# unten: save(btw_2025_strukturdaten, file = "data/btw_2025_strukturdaten.RData").

# Nun können wir beide hier mit dem base R Befehl load() einlesen.
load("output/btw_2025_ergebnisse.RData")
# load("data/btw_2025_strukturdaten.RData")


# 3. Daten Bereinigen --------------------

## dplyr::rename() - Variablennamen umbenennen  --------------

# Variablennamen ansehen
names(btw_2025_ergebnisse)

# ändern
btw_2025_ergebnisse <- btw_2025_ergebnisse %>%
  dplyr::rename(
    uberg_gebietsart = ueg_gebietsart, # neuer_name = alter_name
    uberg_gebietsnr = ueg_gebietsnummer)




## tidyr::drop_na() - NAs herausfiltern -------------------------------

# Umgang mit fehlenden Werten:
# zunächst schauen, welche Werte die Variable "gewahlt" enthält

btw_2025_ergebnisse %>%
  dplyr::distinct(gewahlt)

btw_2025_ergebnisse %>%
  dplyr::count(gewahlt)


# drop_na() entfernt alle Zeilen, in denen die angegebene Variable NA ist
btw_2025_ergebnisse %>%
  dplyr::count(stimme)

btw_2025_ergebnisse %>%
  tidyr::drop_na(stimme) %>%
  dplyr::count(stimme)


# Zum Vergleich mit filter(): dasselbe Ergebnis, aber drop_na() ist lesbarer
btw_2025_ergebnisse %>%
  dplyr::filter(!is.na(stimme))

# WICHTIG: Je nach Datenformat kann es sein, dass R fehlende Werte (wie z.B. -99)
   # nicht als solche erkennt. Dann müssen diese explizit in NA umgewandelt werden.



# 4 Daten Zuschneiden ----------------------


## dplyr::select - Variablen auswählen (oder ausschließen) ---------------

# Nur bestimmte Variablen behalten
btw_2025_ergebnisse %>%
  dplyr::select(gebietsart, gebietsnummer, gruppenname, stimme, prozent, gewahlt) %>%
  head(10)
# NB: Ich speichere die Änderungen hier nicht, da ich die Variablen alle behalten
# möchte. Daher kein btw_2025_ergebnisse <- davor.

# Hilfreich: starts_with(), ends_with(), contains()
btw_2025_ergebnisse %>%
  dplyr::select(gebietsart, gebietsnummer, gruppenname, stimme,
                starts_with("vorp"), starts_with("diff")) %>%
  head(10)


# Variablen ausschließen mit -
btw_2025_ergebnisse <- btw_2025_ergebnisse %>%
  dplyr::select(-wahlart, -wahltag, -gruppenreihenfolge, -bemerkung)
# NB: Das habe ich gespeichert!



## dplyr::filter() - Zeilen auswählen -----------

   # Während wir mit dplyr::slide() Zeilen nach Position auswählen, erlaubt und
   # dplyr::filter() Zeilen nach Bedingungen auswählen.

# Nur Ergebnisse in Berlin
btw_2025_ergebnisse_be <-  btw_2025_ergebnisse %>%
  dplyr::filter(gebietsname == "Berlin")

# Nur Zweitstimmenergebnisse
btw_2025_ergebnisse_zweitst <-  btw_2025_ergebnisse %>%
  dplyr::filter(stimme == 2)




# frequency counts aller vorkommenden Ausprägungen der Variable gruppenart
btw_2025_ergebnisse %>%
  count(gruppenart)

# Einzelbewerber*innen nicht anzeigen
btw_2025_ergebnisse %>%
  dplyr::filter(gebietsart != "Einzelbewerber/Wählergruppe") %>%
  View()

# Kombination: Bundesebene UND Zweitstimmen UND nur Parteien
btw_2025_ergebnisse_partei <-  btw_2025_ergebnisse %>%
  dplyr::filter(
    gebietsart == "Bund",
    stimme == 2,
    gruppenart == "Partei"
  )

# Mehrere Ausprägungen mit %in%
btw_2025_ergebnisse_partei %>%
  dplyr::filter(gruppenname %in% c("SPD", "CDU", "CSU", "GRÜNE", "Die Linke", "AfD", "FDP", "BSW")) %>%
  View()

btw_2025_ergebnisse_partei %>%
  count(gruppenname)


# 5 Daten umstrukturieren ---------------------------


## dplyr::relocate() - Variablenpositionen ändern -------------
# Eine Variable verschieben
btw_2025_ergebnisse <- btw_2025_ergebnisse %>%
  dplyr::relocate(gewahlt, .before = vorp_anzahl)

# Eine Variable nach hinten schieben
btw_2025_ergebnisse <- btw_2025_ergebnisse %>%
  dplyr::relocate(gebietsart, .after = gebietsname)


## tidyr::pivot_wider() & tidyr::pivot_longer() ------------------------

# Aktuell ist der Datensatz im Long-Format:
   # jede Stimme (Erst-/Zweitstimme) ist eine eigene Zeile pro Partei und Gebiet.
   # Das ist für manche Analysen unpraktisch.

# durch pivot_wider() werden Erst- und Zweitstimmen zu eigenen Spalten
btw_2025_ergebnisse_wide <- btw_2025_ergebnisse %>%
  tidyr::pivot_wider(
    names_from  = stimme, # Werte dieser Variable werden Spaltennamen
    values_from = anzahl, # diese Werte füllen die neuen Spalten
    names_prefix = "stimme_" # Präfix für neue Spaltennamen (stimme_1)
  )


## dplyr::mutate() und dplyr::case_when() - Variable hinzufügen ---------

# case_when(): kategoriale Variable auf Basis von Bedingungen
# Syntax: case_when(Bedingung ~ Wert, Bedingung ~ Wert, .default = Wert)
btw_2025_ergebnisse %>%
  dplyr::filter(stimme == 1,
                gruppenname %in% c("SPD", "CDU", "CSU", "GRÜNE", "Die Linke",
                                   "AfD", "FDP", "BSW")) %>%
  dplyr::mutate(
    ergebnis_kat = dplyr::case_when(
      prozent >= 30 ~ "stark",
      prozent >= 15 ~ "mittel",
      prozent >= 5  ~ "schwach",
      .default     = "sehr schwach"
    )
  ) %>%
  relocate(ergebnis_kat, .after = prozent) %>%
  tidyr::drop_na(ergebnis_kat, prozent) %>%
  View()



# zum Beispiel eine Altersvariable zu der rep. Wahlstatistik hinzufügen! Angaben sind den
   # allgemeinen methodsichen Hinweisen entnommen.
