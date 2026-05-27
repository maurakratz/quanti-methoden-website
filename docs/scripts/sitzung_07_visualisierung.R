# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# SITZUNG 07 Visualisierungen mit ggplot2               -----------
# ______________________________________________________-----------


# 00 setup --------------------------------------

library(dplyr)
library(ggplot2)

load("output/btw_2025_strukturdaten.RData")

wk_btw_2025_strukt <- btw_2025_strukturdaten %>%
  dplyr::filter(wahlkreis_nr < 900)


# 01 unzureichende baseR-Visualisierungen -------------

# in den letzten beiden Sitzungen haben wir gesehen, wie unbefriedigend
# die Darstellungsmöglichkeiten mit baseR sind!

# Zur Erinnerung. Die sahen etwa so aus

  # Säulendiagramm mit baseR
  # Wahlkreise pro Bundesland
  wk_btw_2025_strukt %>%
    dplyr::count(land, sort = TRUE) %>%
    with(barplot(n))

  # Boxplot mit baseR
  # Arbeitslosenquote über Wahlkreise hinweg
  boxplot(wk_btw_2025_strukt$alo_quote_insgesamt)

  # Arbeitslosenquote über Wahlkreise hinweg nach Bundesland
  boxplot(alo_quote_insgesamt ~ land, data = wk_btw_2025_strukt)

  # Insgesamt also: UNZUREICHEND!


# Heute lernen wir schöne, maßgeschneiderte Grafiken mit ggplot2 zu erstellen!


# 02 schöne ggplot2()-Visualisierungen ------------

# ggplot2 ist ein Paket, das von Hadley Wickham entwickelt wurde. Es basiert
# auf der"Grammar of Graphics", die das die Erstellung von Grafiken in
# verschiedene Komponenten unterteilt.

# 1. Daten: Welche Daten sollen dargestellt werden?
# 2. Ästhetik: Welche Variablen sollen auf welchen Achsen dargestellt
# 3. Geometrie: Welche Art von Grafik soll es sein? (z.B. Punkte, Linien, Balken)
# 4. Facetten: Sollen mehrere Grafiken erstellt werden, z.B. für
#    verschiedene Gruppen?
# 5. Anpassungen: Wie soll die Grafik aussehen? (z.B. Farben, Achsenbeschriftungen, Titel)


# 03 Säulendiagramm: wahlkreise pro Bundesland ----------
wk_btw_2025_strukt %>%
  dplyr::count(land, sort = TRUE) %>%
  ggplot2::ggplot(aes(x = reorder(land, n), y = n)) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Wahlkreise pro Bundesland",
    x = NULL,
    y = "Anzahl Wahlkreise"
  ) +
  ggplot2::theme_minimal()


# wie habe ich das jetzt gemacht?!


# schritt 1: daten, achsen und geometrie
  wk_btw_2025_strukt %>%
    dplyr::count(land) %>%  # vorbereitende Rechnung
    ggplot2::ggplot(aes(x = land, y = n)) + # x und y
    ggplot2::geom_col() # Säulen

# schritt 2: sortieren und tauschen
  wk_btw_2025_strukt %>%
    dplyr::count(land) %>%
    ggplot2::ggplot(aes(x = reorder(land, n), y = n)) + # reorder sortiert K
    # Kategorien einer Variable x nach den Werten von y
    ggplot2::geom_col() +
    ggplot2::coord_flip() # x- und y-Achse tauschen

  # alternativ zum flip können wir auch die Beschriftung der x-achse um
  # 90 Grad drehen, sodass sie sich nicht mehr überschneidet:
  # ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))


  # schritt 3: Titel, Achsenbeschriftungen und Design
  wk_bar <- wk_btw_2025_strukt %>% # in einem Objekt speichern
    dplyr::count(land, sort = TRUE) %>%
    ggplot2::ggplot(aes(x = reorder(land, n), y = n)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Wahlkreise pro Bundesland", # Titel
      x = NULL, # keine x-Achsenbeschriftung (Achtung, Flip!)
      y = "Anzahl Wahlkreise"
    )

  wk_bar # ansehen

  wk_bar + theme_bw() # Theme ändern

  # Exportieren (wird nicht gebraucht, wenn wir in Quarto rendern!)
  ggplot2::ggsave("./output/wk_bar_plot.png",
                  plot = wk_bar + theme_bw(),
                  width = 8, height = 4, dpi = 300)

  # svg ist das beste Format für Word, weil verlustfrei
  # das heißt man kann reinzoomen, ohne dass es pixelig wird
  ggplot2::ggsave("./output/wk_bar_plot.svg",
                  plot = wk_bar + theme_bw(),
                  width = 8, height = 4, dpi = 300)


# 04 Boxplot: Arbeitslosenquote -----------
  wk_btw_2025_strukt %>%
    ggplot2::ggplot(aes(y = alo_quote_insgesamt)) +
    ggplot2::geom_boxplot(fill = "steelblue", # Farbe
                          alpha = 0.7) + # Transparenz
    ggplot2::labs(title = "Arbeitslosenquote über alle Wahlkreise",
                  y = "Arbeitslosenquote (%)",
                  x = NULL) +
    ggplot2::theme(axis.text.x = element_blank())


# Achsen anpassen
  wk_btw_2025_strukt %>%
    ggplot2::ggplot(aes(y = alo_quote_insgesamt)) +
    ggplot2::geom_boxplot(fill = "steelblue",
                          alpha = 0.7) +
    # y-Achse von 0 bis 20 mit Schritten von 1
    ggplot2::scale_y_continuous(breaks = seq(0, 20, by =  1)) +
    ggplot2::labs(title = NULL,
                  y = "Arbeitslosenquote (%)",
                  x = NULL) +
    ggplot2::theme(axis.text.x = element_blank()) +
    theme_minimal()

  wk_btw_2025_strukt %>%
    ggplot2::ggplot(mapping = aes(x = "", y = alo_quote_insgesamt)) +
    ggplot2::geom_boxplot(fill = "steelblue", alpha = 0.5) +
    ggplot2::stat_summary(fun = mean,
                          geom = "point",
                          shape = 4,
                          size = 3) +
    ggplot2::scale_y_continuous(breaks = seq(0, 20, by = 1)) +
    ggplot2::labs(y = "Arbeitslosenquote (%)", x = NULL) +
    ggplot2::theme_minimal()


## BONUS: Punkt-/ Liniendiagramm -----------------------------------------------

  # Lade zunächst den Übungsdatensatz namens Quoten von der Webseite herunter und
  # lege ihn in den Datenordner.

  quoten <- readxl::read_excel("./data/Quoten.xlsx")

  # Schritt 1: Definition der Datengrundlage + Koordinatensystem
  ggplot2::ggplot(quoten, # Datensatz definieren
                  aes(x = Folge, # aes = definieren, welche Daten auf X- und Y-Achse sollen
                      y = alleMio))


  # Schritt 2: Definition der Darstellungsweise
  ggplot(quoten, # Datensatz definieren
         aes(x = Folge, # aes = definieren, welche Daten auf X- und Y-Achse sollen
             y = alleMio)) +
    geom_point() # Datenpunkte hinzufügen


  ggplot(quoten, # Datensatz definieren
         aes(x = Folge, # aes = definieren, welche Daten auf X- und Y-Achse sollen
             y = alleMio,
             color = Sendung) # Gruppenvergleich: jede Sendung bekommt eine eigene Farbe
  ) +
    geom_point() # Datenpunkte hinzufügen


  ggplot(quoten, # Datensatz definieren
         aes(x = Folge, # aes = definieren, welche Daten auf X- und Y-Achse sollen
             y = alleMio,
             color = Sendung)) +
    geom_point() +  # Datenpunkte hinzufügen
    geom_line() # Datenpunkte miteinander verbinden/Linien hinzufügen

  ggplot(quoten, # Datensatz definieren
         aes(x = Folge, # aes = definieren, welche Daten auf X- und Y-Achse sollen
             y = alleMio,
             color = Sendung)) +
    geom_point() + # Datenpunkte hinzufügen
    geom_line() + # Datenpunkte miteinander verbinden/Linien hinzufügen
    scale_y_continuous(limits = c(0,3)) + # y-Achse definieren
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) # x-Achse definieren

  ggplot(quoten, # Datensatz definieren
         aes(x = Folge, # aes = definieren, welche Daten auf X- und Y-Achse sollen
             y = alleMio,
             color = Sendung)) +
    geom_point() + # Datenpunkte hinzufügen
    geom_line() + # Datenpunkte miteinander verbinden/Linien hinzufügen
    scale_y_continuous(limits = c(0,3)) +  # y-Achse definieren
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) + # x-Achse definieren
    labs(x = "Folge", # x-Achse beschriften
         y = "Anzahl der Zuschauenden in Mio.", # y-Achse beschriften
         title = "Bachelorette teilweise mehr Zuschauende als Bachelor", # Titel geben
         subtitle = "Zuschauende in Mio. nach Staffel und Folgen") # Untertitel gegeben


  # In einem Objekt speichern
  zuschauende_plot <- ggplot(quoten, # Datensatz definieren
                             aes(x = Folge, # aes = definieren Sie, welche Daten verwendet werden (X- und Y-Achse)
                                 y = alleMio,
                                 color = Sendung)) + # x-Achse definieren
    geom_point() + # Datenpunkte hinzufügen
    geom_line() + # Datenpunkte miteinander verbinden/Linien hinzufügen
    scale_y_continuous(limits = c(0,3)) + # y-Achse definieren
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
    labs(x = "Folge", # x-Achse beschriftet
         y = "Anzahl der Zuschauenden in Mio.", # y-Achse beschriftet
         title = "Bachelorette teilweise mehr Zuschauende als Bachelor", # Titel gegeben
         subtitle = "Zuschauende in Mio. nach Staffel und Folgen") # Untertitel gegeben


  # Jetzt können wir das Objekt nutzen, um weitere Ebenen hinzuzufügen
  zuschauende_plot + theme_bw() # theme hinzufügen


  # Objekt als svg oder jpg datei herunterladen

  ggplot2::ggsave("./output/zuschauende_plot.jpg",
                  plot = zuschauende_plot + theme_bw(),
                  width = 8, height = 4, dpi = 300)

  ggplot2::ggsave("./output/zuschauende_plot.svg",
                  plot = zuschauende_plot + theme_bw(),
                  width = 8, height = 4, dpi = 300)
