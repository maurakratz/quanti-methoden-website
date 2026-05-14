# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# SITZUNG 07 Visualisierungen mit ggplot2         -----------
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

  # Insgesamt also: UGLY!


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


# saeulendiagramm: wahlkreise pro bundesland
wk_btw_2025_strukt %>%
  dplyr::count(land, sort = TRUE) %>%
  ggplot2::ggplot(aes(x = reorder(land, n), y = n)) +
  ggplot2::geom_col(fill = "steelblue") +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Wahlkreise pro Bundesland",
    x = NULL,
    y = "Anzahl Wahlkreise"
  ) +
  ggplot2::theme_minimal()


# wie habe ich das jetzt gemacht?!


# 03 layer by layer --------------

# schritt 1: daten, achsen und geometrie
  wk_btw_2025_strukt %>%
    dplyr::count(land, sort = TRUE) %>%  # vorbereitende Rechnung
  ggplot2::ggplot(aes(x = land, y = n)) + # x und y
    ggplot2::geom_col() # Säulen

# schritt 2: sortieren und tauschen
  wk_btw_2025_strukt %>%
    dplyr::count(land, sort = TRUE) %>%
    ggplot2::ggplot(aes(x = reorder(land, n), y = n)) + # reorder sortiert
    ggplot2::geom_col() +
    ggplot2::coord_flip() # x- und y-Achse tauschen,
  # Kategorien einer Variable x nach den Werten von y sortieren

  # schritt 3: Titel, Achsenbeschriftungen und Design
  wk_btw_2025_strukt %>%
    dplyr::count(land, sort = TRUE) %>%
    ggplot2::ggplot(aes(x = reorder(land, n), y = n)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Wahlkreise pro Bundesland", # Titel
      x = NULL, # keine x-Achsenbeschriftung (Achtung, Flip!)
      y = "Anzahl Wahlkreise"
    ) +
    ggplot2::theme_minimal()

  # alternativ zum flip können wir auch die Beschriftung der x-achse um
  # 90 Grad drehen, sodass sie sich nicht mehr überschneidet:
  # ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))


# boxplot: arbeitslosenquote
  wk_btw_2025_strukt %>%
    ggplot2::ggplot(aes(y = alo_quote_insgesamt)) +
    ggplot2::geom_boxplot(fill = "steelblue", # Farbe
                          alpha = 0.7) + # Transparenz
    ggplot2::labs(title = "Arbeitslosenquote über alle Wahlkreise",
                  y = "Arbeitslosenquote (%)",
                  x = NULL) +
    ggplot2::theme_minimal() +
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
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = element_blank())

