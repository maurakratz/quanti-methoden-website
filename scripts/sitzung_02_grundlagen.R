# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# SITZUNG 02                                            -----------
# ______________________________________________________-----------


# 1 Benutzeroberfläche -------------------

# RStudio anpassen
   # Tools => global options => bspw. appearance, pane layout etc.
   # die Option Save workspace ausschalten
# R Projekt anlegen zum sauberen Arbeiten: File –> New Project
   # das Arbeitsverzeichnis wird so direkt auf den Projektordner festlegt
   # hier werden dann standartmäßig alle Daten, Skripte, Abbildungen usw. abgelegt:
     #.R-Dateien (R Skripte),
     #.RData-Dateien (R Datensätze),
     #.Rproj-Dateien (Platzhalter, der den Pfad zum R Projekt weist),
     #.Rproj.user-Dateien (versteckt, temporäre Datei zur R-Sitzung),
     # ggf temporäre Dateien wie .Rhist-Dateien
# alternativ: immer zu Beginn jedes Scripts den work directory definieren
   getwd() # zeigt den work direktory, also das aktuelle Arbeitsverzeichnis an
   # umlegbar mit setwd("Pfad-des-gewünschten-workdirectory")
   setwd("C:/Forschung_lokal/Quanti_Methoden_R")
   setwd(r"(C:\Forschung_lokal\Quanti_Methoden_R)")


# RStudio Arbeitsumfeld:
   # links oben: Das Skript
     # Hier schreiben und speichern wir unseren Code
     # Wir schreiben nicht direkt in der Konsole (auch wenn das auch funktionieren würde),
     # sondern in Skripten (diese kann man speichern)
     # Befehl im Skript ausführen:
       # den Teil des Skripts der ausgeführt werden soll markieren
       # oder den Cursor irgendwo in dem Block platzieren, der ausgeführt werden soll
       # dann Strg + Enter (oder Run drücken)
       # alternativ: den ganzen Skript mit dem "Source"-Button ausführen

   # links unten: Die Konsole
     # Hier werden unsere Operationen ausgeführt und Ergebnisse ausgegeben

   # oben rechts: Environment, History, Connections, Build, Git, etc.
     # Im Environment werden uns alle erstellten R-Objekte anzeigt
     # z.B. Datensätze, Variablen, Werte, Funktionen

   # unten rechts: Files, Plots, Packages, Help, Viewer etc.
     # Files: Hier sehen wir alle Dateien in unserem Arbeitsverzeichnis
     # Plots: Hier werden alle Grafiken angezeigt, die wir erstellen
     # (Packages: Hier können wir Pakete installieren und laden)
     # (Help: Hier können wir die Hilfeseiten zu Funktionen und Paketen aufrufen)
     # Viewer: Hier können wir z.B. HTML-Dateien anzeigen lassen



# 2 Stilregeln ----------------------

# Was macht ein gutes Skript aus?
   # Intersubjektiv nachvollziehbar: Abschnitte und Unterabschnitte, Kommentare etc.
   # Lässt sich von oben bis unten durchlaufen (mit dem "Source"-button oder händisch)
   # TIPP: regelmäßig speichern: Strg + S bzw. File => Save As

# a) Kommentare:
   # Das Einfügen einer Raute # vor oder nach Befehlen erlaubt es, kurze Kommentare und Erklärungen beizufügen, ohne dass diese als Code gelesen werden
   # alles nach der Raute wird dann beim Ausführen übersprungen

# b) Outline/ Abschnitte
   # Ein Kommentar der Form Raute, Text, Minuszeichen (# ...-------) wird als Gliederungspunkt in der document outline agezeigt
   # alternativ können solche Abschnitte auch mit Strg + Shift + R erstellt werden

# c) Zeichen
   # grundsätzlich Sonderzeichen im Code vermeiden: ä, ö, ß usw.
   # Großbuchstaben weitgehend vermeiden
   # Leerzeichen vor und nach Operatoren sowie nach Kommata
   # Keine Leerzeichen vor oder nach () oder „“

# d) Ordnung
   # Lange Befehle durch Zeilenumbrüche unterbrechen
   # Einrücken mit Leerzeichen
   # Konsole regelmäßig "aufräumen": Strg + L
   # detaillierte Stilregeln unter (http://adv-r.had.co.nz/Style.html)



# 3 Befehle/ Funktionen --------------

# Funktionsname + geschlossene Klammer
print("Hallo!") # "drucke" das Wort Hallo!
sqrt(16) # berechne die Wurzel von 16

#	R als Taschenrechner (Punkt vor Strich beachten)
   # Addieren: +
   # Subtrahieren:	–
   # Multiplizieren:	*
   # Dividieren: /
   # Potenzieren:	**
   # Wurzel ziehen: sqrt()


# 4 Objekte -----------------------------------------------------------------

# Zuweisungen mit: <- (also "kleiner als" und "minus"); shortcut Alt + minus)

a <- 25 # 25 wird im Objekt a gespeichert
b <- 19 # 19 wird im Objekt b gespeichert
ab <- a * b # das Ergebnis von a*b, also 25*19, wird im Objekt ab gespeichert
c <- "Willkommen in R!"

print(ab) # "drucke" ab
ab # ab anzeigen lassen (Alternative zu print)

# Arbeitsumfeld ("environmanet") anschauen/aufräumen
ls() # alles anzeigen lassen
rm(a) # etwas (in diesem Falle das Objekt a) entfernen





