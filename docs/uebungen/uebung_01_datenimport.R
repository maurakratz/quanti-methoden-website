# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# ÜBUNG 1: Datenimport                                  -----------
# ______________________________________________________-----------


# Aufgabe 1 -------------
   # Lade dir die Wahlkreisstrukturdaten herunter und lege sie in deinen .data-Unterordner.
   # Siehe dafür auf der Kurswebseite unter dem Tab "Daten" nach.

# Aufgabe 2 -----------------
   # Lade die nötigen Pakete: rio, labelled.
   # Lese die Daten mir dem rio-Paket ein. Achte darauf, ggf. überflüssige Zeilen
   # zu überspringen. Sieh dir das Ergebnis an.

# Aufgabe 3 -----------------
   # Bereinige die Variablennamen mit janitor::clean_names().

# Aufgabe 4 -------------------
   # a) Lass dir die Variablennamen ausgeben.
   # b) Ändere sie zu kurzen, sinnvollen Namen um.
     # Da wir das noch nicht durchgenommen haben bekommst du unetnn ein Beispiel,
     # das den rename-Befehl illustriert. Vervollständige den Befehl, indem du
     # auch die übrigen Varaiblen umbenennst.
   # c) Füge mit labelled::set_variable_labels() den neuen Namen
      # entsprechende Labels hinzu.

# b) Zu lange Namen umbenennen
btw_2025_strukturdaten <- btw_2025_strukturdaten %>%
  dplyr::rename(
    gemeinden = gemeinden_am_31_12_2023_anzahl,
    flache_km2 = flache_am_31_12_2023_km2,
    bev_insgesamt_1000 = bevolkerung_am_31_12_2023_insgesamt_in_1000,
    bev_deutsche_1000 = bevolkerung_am_31_12_2023_deutsche_in_1000,
    bev_auslander_pct = bevolkerung_am_31_12_2023_auslander_innen_percent,
    bev_dichte_ew_km2 = bevolkerungsdichte_am_31_12_2023_ew_je_km2
  )

# Aufgabe 5 -------------------
   # Lass dir mit summarytools eine Datensazübersicht ausgeben.
