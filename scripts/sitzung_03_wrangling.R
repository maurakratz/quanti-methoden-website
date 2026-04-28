# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# SITZUNG 03 Data wrangling                             -----------
# ______________________________________________________-----------


# 1. setup --------------------

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

# Nun können wir beide hier mit dem baseR-Befehl load() einlesen.
load("output/btw_2025_ergebnisse.RData")
load("output/btw_2025_strukturdaten.RData")


# 3. Daten Bereinigen --------------------



## Umgang mit fehlenden Werten ---------------------

# Sicherstellen, dass alles, was NA sein sollte, auch NA ist!
   # Je nach Datenformat kann es sein, dass R fehlende Werte (wie z.B. -99)
   # nicht als solche erkennt. Dann müssen diese explizit in NA umgewandelt werden.
# So können fehlende Werte bei zukünftigen Operationen ignoriert werden.


# zunächst schauen, welche Werte die Variable "gewahlt" enthält
btw_2025_ergebnisse %>%
  dplyr::distinct(gewahlt)

btw_2025_ergebnisse %>%
  dplyr::count(gewahlt)
# hier fällt auf, dass einige Zellen einfach leer sind, andere als "-". Warum?

  # welche Zellen sind leer?
  btw_2025_ergebnisse %>%
    dplyr::filter(gewahlt == "") %>%
    View()

  # welche Zellen sind – ?
  btw_2025_ergebnisse %>%
    dplyr::filter(gewahlt == "–") %>%
    View()

  btw_2025_ergebnisse %>%
    dplyr::filter(gewahlt == "–") %>%
    dplyr::distinct(gebietsnummer)
  # kleiner Tipp: https://www.bundeswahlleiterin.de/bundestagswahlen/2025/ergebnisse/wahlatlas.html#(wahlatlas-4f511b16-bb62-403b-860c-68eb306c5540/thema_GEWAEHLTE_IN_WAHLKREISEN_/haeufigkeiten/wahl%C2%ADkreis/)


# Wenn die Zelle leer ist, bedeutet das also fehlender Wert
# Also wandeln wir leere Zellen der var "gewahlt" in NAs um:
btw_2025_ergebnisse <- btw_2025_ergebnisse %>%
  dplyr::mutate(
    gewahlt = dplyr::case_when(
      gewahlt == "" ~ NA,
      .default = gewahlt
    )
  )

# nun können fehlende Werte gezielt ignoriert werden
btw_2025_ergebnisse %>%
  dplyr::filter(!is.na(gewahlt)) %>% # alternative: drop_na()
  count(gewahlt)


## Variablen umbenennen  --------------

# Variablennamen ansehen
names(btw_2025_ergebnisse)

# ändern
btw_2025_ergebnisse <- btw_2025_ergebnisse %>%
  dplyr::rename(
    uberg_gebietsart = ueg_gebietsart, # neuer_name = alter_name
    uberg_gebietsnr = ueg_gebietsnummer)



# 4. Daten Zuschneiden ----------------------


## Variablen auswählen (oder ausschließen) ---------------

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



## Zeilen auswählen -----------

   # Während wir mit dplyr::slide() Zeilen nach Position auswählen, erlaubt und
   # dplyr::filter() Zeilen nach Bedingungen auswählen.
   # (siehe oben unter Umgang mit fehlenden Werten)

# Häufigkeitszählung aller vorkommenden Ausprägungen der Variable gruppenart
btw_2025_ergebnisse %>%
  count(gruppenart)

# Mehrere Ausprägungen mit %in% filtern
btw_2025_ergebnisse %>%
  dplyr::filter(gebietsart == "Bund") %>%
  dplyr::filter(gruppenname %in% c("SPD", "CDU", "CSU", "GRÜNE", "Die Linke", "AfD", "FDP", "BSW")) %>%
  View()


# was habe ich hier gemacht?
btw_2025_ergebnisse %>%
  dplyr::filter(gebietsart == "Land",
                gruppenart == "Partei",
                diff_prozent_pkt > 5) %>%
  dplyr::select(- uberg_gebietsart,
                - uberg_gebietsnr,
                - gruppenart)


# 5. Daten umstrukturieren ---------------------------


## Variablenpositionen ändern -------------
# Eine Variable verschieben
btw_2025_ergebnisse <- btw_2025_ergebnisse %>%
  dplyr::relocate(gewahlt, .before = vorp_anzahl)

# Eine Variable nach hinten schieben
btw_2025_ergebnisse <- btw_2025_ergebnisse %>%
  dplyr::relocate(gebietsart, .after = gebietsname)


## long vs. wide format ------------------------

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

btw_2025_ergebnisse_wide <- btw_2025_ergebnisse %>%
  dplyr::filter(gruppenart == "Partei",
                !is.na(stimme),
                gruppenname %in% c("SPD", "CDU", "CSU", "GRÜNE", "Die Linke", "AfD", "FDP", "BSW")
                ) %>%
  dplyr::select(gebietsnummer, gebietsname, gebietsart, gruppenname, stimme, prozent) %>%
  tidyr::pivot_wider(
    names_from = stimme,
    values_from = prozent,
    names_prefix = "prozent_stimme_"
  )

