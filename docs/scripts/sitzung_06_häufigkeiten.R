# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# SITZUNG 06 Häufigkeiten und deren Visualisierung         -----------
# ______________________________________________________-----------


# 00 setup --------------------------------------

# Pakete
library(dplyr)
library(skimr)
library(questionr)


# Daten
load("output/btw_2025_ergebnisse.RData")
load("output/btw_2025_strukturdaten.RData")
load("output/btw_25_rws.RData")
# siehe Sitzung 2 und Lösung zu Übung 1, falls du die .RData-Datensätze
# nicht finden kannst oder nicht weißt, wie du sie erstellen kannst.

btw_2025_strukturdaten %>%
  dplyr::glimpse()

btw_2025_ergebnisse %>%
  dplyr::glimpse()

btw_25_rws %>%
  dplyr::glimpse()

# nur Wahlkreise behalten (Länder und Bund rausfiltern)
wk_btw_2025_strukt <- btw_2025_strukturdaten %>%
  dplyr::filter(wahlkreis_nr < 900)



# 01 Häufigkeitstabellen -----------------------


## mit dplyr::count() --------------

# Wahlkreise pro Bundesland
wk_btw_2025_strukt %>%
  dplyr::count(land, sort = TRUE)

# mit prozentualen Anteilen
wk_btw_2025_strukt %>%
  dplyr::count(land, sort = TRUE) %>%
  dplyr::mutate(pct = n / sum(n) * 100)

# auf zwei nachkommastellen gerundet:
wk_btw_2025_strukt %>%
  dplyr::count(land, sort = TRUE) %>%
  dplyr::mutate(pct = round(n / sum(n) * 100, 2))


## mit questionr::freq() ------------------

# klassischerweise enthalten Häufigkeitstabellen Angaben zu:
# - absoluten Häufigkeiten (n)
# - Prozentualen Anteilen (%)
# - kumulierten Anteilen (cum_pct)
# Je nach Vollständigkeit kann es sinnvoll sein, diese Angaben
# nur auf die validen Werte (ohne missings) auszugeben.

questionr::freq(wk_btw_2025_strukt$land) # absolute Häufigkeiten

questionr::freq(wk_btw_2025_strukt$land,
                valid = FALSE)  # val% ausblenden

questionr::freq(wk_btw_2025_strukt$land,
                valid = FALSE,
                cum = TRUE) # mit kumulierten Häufigkeiten

questionr::freq(
  wk_btw_2025_strukt$land,
  valid = FALSE,
  cum = TRUE,
  total = TRUE # mit Gesamtzeile
)


## (Optionale Alternativen zu dplyr und questionr) ---------

# mit baseR
table(wk_btw_2025_strukt$land)

# mit %
prop.table(table(wk_btw_2025_strukt$land)) * 100

# mit janitor - für den schnellen Überblick
wk_btw_2025_strukt %>%
  janitor::tabyl(land) %>% # freq tab mit n und pct
  dplyr::arrange(desc(n)) %>% # absteigend nach n sortieren
  janitor::adorn_totals("row") %>%  # Gesamtzeile hinzufügen
  janitor::adorn_pct_formatting()  # mit %-Zeichen ausstatten



# 02 kontinuierliche Variablen klassieren -------------------

# Manchmal ist es sinnvoll, metrisch skalierte Variablen in Intervalle zu
# unterteilen, um sie besser beschreiben oder visualisieren zu können.
# Denn übersichtliche Häufigkeitstabellen kann es, logischerweise,
# nur für kategoriale Variablen geben.

# Beispiel Arbeitslosenquote in Gruppen einteilen
wk_btw_2025_strukt <- wk_btw_2025_strukt %>%
  dplyr::mutate(alo_quote_insg_kat = dplyr::case_when(
    alo_quote_insgesamt < 4 ~ "< 4%",
    alo_quote_insgesamt >= 4 & alo_quote_insgesamt < 6 ~ "4–5%",
    alo_quote_insgesamt >= 6 & alo_quote_insgesamt < 8 ~ "6–7%",
    alo_quote_insgesamt >= 8 & alo_quote_insgesamt < 10 ~ "8–9%",
    alo_quote_insgesamt >= 10 ~ ">= 10%"
  )) %>%
  relocate(alo_quote_insg_kat , .after = alo_quote_insgesamt)
# Man könnte stattdessen natürlich auch so was wie niedrig, mittel, hoch als
# Kategorien verwenden

# Häufigkeitstabelle der neuen, ordinalen Variable zur Arbeitslosenquote
wk_btw_2025_strukt %>%
  dplyr::count(alo_quote_insg_kat) %>%
  dplyr::mutate(pct = round(n / sum(n) * 100, 2),
                cum_pct = cumsum(pct))

# Das ist nicht sehr sinnvoll angeordnet!


# 03 Exkurs Faktoren ---------------

# Das Problemn ist, dass R die neue Variable als character-Klasse interpretiert,
# also als Textvariable, und diese standardmäßig alphabetisch sortiert.
class(wk_btw_2025_strukt$alo_quote_insg_kat)


# Das ist hier nicht sinnvoll, da die Kategorien ja eine natürliche Rangfolge
# haben. Deshalb müssen wir R mitteilen, dass es sich um eine kategoriale
# Variable handelt, die in einer bestimmten Reihenfolge sortiert werden soll.
# Dafür gibt es die Klasse "factor".

# Ein factor ist ein Datentyp in R für kategoriale Variablen
# Im Kern ein Integer-Vektor, dem R im Hintergrund Kategorien (levels) zuordnet
wk_btw_2025_strukt <- wk_btw_2025_strukt %>%
  dplyr::mutate(alo_quote_insg_kat = factor(alo_quote_insg_kat,
                                            levels = c("< 4%", # das hier zuerst
                                                       "4–5%", # dann das
                                                       "6–7%", # dann das
                                                       "8–9%", # usw.
                                                       ">= 10%")))

attributes(wk_btw_2025_strukt$alo_quote_insg_kat)


# hier nochmal dieselbe tabelle - diesmal schön geordnet
wk_btw_2025_strukt %>%
  dplyr::count(alo_quote_insg_kat) %>%
  dplyr::mutate(pct = round(n / sum(n) * 100, 2),
                cum_pct = cumsum(pct))




# 04 Häufigkeiten visualisieren ---------------------

# Nominal oder ordinal skalierte Variablen (also kategoriale Variablen)
# können mit Säulen- oder Balkendiagrammen visualisiert werden.
# Für metrisch skalierte Variablen eigent sich ein Histogramm. Der Boxplot,
# den wir letzte Sitzung kennengelert haben, visualisiert nicht Häufigkeiten,
# sondern die Lage und Streuung der Werte. Er eignet sich gut zur Ergänzung
# des Histogramms.


# Säulendiagramm mit baseR
wk_btw_2025_strukt %>%
  dplyr::count(land, sort = TRUE) %>%
  with(barplot(n))
# optional names.arg = land, damit die Ländernamen (teils) angezeigt werden



# Histogramme für kontinuierliche Variablen
hist(wk_btw_2025_strukt$alo_quote_insgesamt)
# hier wurde also eine eigentlich kontinuierliche Variable in Intervalle
# unterteilt, um sie darstellen zu können.


