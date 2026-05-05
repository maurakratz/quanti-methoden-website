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


# Aufgabe 3 ------------------------------
   # Bereinige die Variablennamen.


# Aufgabe 4 ---------------------------
   # Füge mit labelled::set_variable_labels() den Variablennamen
   # entsprechende Labels hinzu.


# Aufgabe 5 ----------------------------
   # Lass dir mit summarytools eine Datensazübersicht ausgeben und
   # speichere sie im output-Ordner.


# Aufgabe 6 --------------------
   # a) Lade nun ebenfalls den Strukturdatensatz ein.
     # Dazu kannst du entweder den entsprechenden Code-Abschnitt
     # (import, clean_names etc.) hierher kopieren, oder aber die .RData-Datei
     # mit load() direkt laden. Beachte, dass du sie für letzteres
     # zuvor mit save() gespeichert haben musst.


   # b) Füge nun der repräsentativen Wahlstatistik die Strukturvariablen aus dem
     #  Strukturdatensatz mit einem left-join()-Befehl hinzu.

     # Sieh dir beide Datensätze an: Die Strukturdaten liegen pro Wahlkreis,
     # pro Bundesland und für Gesamtdeutschland vor, die repräsentative
     # Wahlstatistik nur pro Bundesland und für Gesamtdeutschland.
     # Filtere also zunächst die Strukturdaten, sodass nur die Zeilen übrig
     # bleiben, die Angaben zu Bund und Ländern enthalten. Speichere diese
     # Zeilen in einem neuen Objekt, das du strukturdaten_merge nennst.

     # Auch sind die Bundesländer in beiden datensätzen unterschiedlich kodiert:
     # In der Variable land in strukturdaten_merge sind sie ausgeschrieben,
     # in btw_25_rws nicht.
     # Ändere also die Ausprägungen von land in btw_25_rws entsprechend um,
     # sodass sie mit den Angaben in strukturdaten_merge übereinstimmen.
     # (Das kannst du mit case_when() machen.)

     # Nun, da die Datenstrukturen zueinander passen: Führe sie auf Grundlage
     # der land-Variable in beiden Datensätzen mit einem join-Befehl zusammen.

     # Sieh dir das Ergebnis an. Erstelle und speichere eine Übersicht
     # des neuen Datensatzes mit summarytools. Speichere auch den Datensatz
     # selbst als .RData-Datei.

