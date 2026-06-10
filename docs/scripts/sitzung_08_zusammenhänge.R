# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# SITZUNG 08 Zusammenhänge                              -----------
# ______________________________________________________-----------


# 01 setup ---------------------

# das Übliche
library(haven) # für .dta-Formate
library(dplyr)
library(forcats) # für Faktoren
library(labelled)
library(ggplot2)

# für Kreuztabellen
library(gmodels)
library(gtsummary) # ggf. vorher installieren!
library(janitor)
library(questionr)

# für Korrelationen
library(correlation)
library(report)


# 02 data ------------------------

# Lade den ALLBUS compact herunter (siehe Webseite => Daten)
# Lege ihn in deinen data-Ordner.
# Einlesen tun wir ihn mit dem haven-Paket:


allbus_c_2023_raw <- haven::read_dta("./data/ZA8831_v1-3-0.dta")

# View(allbus_c_2023_raw)



# 03 Exkurs Datenformate -------------------------

# labelled-Vektoren und Faktoren

# In Stata haben die Ausprägungen von Variablen zwei Ebenen:
# den numerischen Rohwert bzw. Value (z.B. 1, 2, 3)
# und ein Value Label (z.B. "stimme zu", "teils/teils", "stimme nicht zu").
# In R gibt es diese Doppelstruktur nicht.

# haven::read_dta() liest Value und Label als haven_labelled-Vektor ein
# Im Anschluss kann dann, je nach benötigtem Skalenniveau,
# entweder mit dem Rohwert, oder mit dem Label gearbeitet werden

# Für kategoriale Variablen brauchen wir in R Faktoren
# haven::as_factor() überführt die Stata-Labels in Faktorlevels.

# Für metrische Variablen (z.B. Einkommen) wäre haven::as_factor() falsch. Hier
# werden wir stattdessen die Rohwert mit as.double() oder as.integer()
# aus dem haven_labelled-Vektor extrahieren.


# Alternativ für englischen ALLBUS

if(FALSE){
  # eng
  allbus_c_2023_eng_raw <- read_dta("data/ZA8833_v1-0-0.dta")

  View(ZA8833_v1_0_0)
}



# 04 Umgang mit haven-labelled-Vektoren (wie im ALLBUS) ----------------------

# Überblick bestimmte Variablen:
# Position, Name, Label, Klasse, Values
allbus_c_2023_raw %>%
  labelled::look_for("wirtschaftslage")

# das gibt mir darüber Auskunft a) was zu NA gemacht werden muss
# und b) welche Labels zu den Werten gehören, damit ich entscheiden kann,
# ob ich die Variable als Faktor oder metrisch weiterverarbeite (Stich-
# wort Skalenniveau).

# dann kann ich, je nach dem was ich tun will, die Variable weiterverarbeiten.
# Bsp:

allbus_c_2023_raw %>%
  dplyr::mutate(
    ep01_f = haven::as_factor(ep01), # ordinal/kategorial -> Faktor
    age_d  = as.double(age) # metrisch -> double
  ) %>%
  select(ep01, ep01_f, age, age_d)
# Entscheidungshilfe:
## kategorial -> as_factor(),
## metrisch -> as.integer()/as.double()


# ERGÄNZUNG VISUALISIERUNG ------------------------------

# 05 Staked bar charts ----------------

# anknüpfend an Übung 05 interessiert uns vielleicht, inwiefern sich die
# Einschätzung, eine vollzeit arbeitende Frau könne eine gute Mutter sein,
# nach Gender der Befragten unterscheidet:

# Zur Erinnerung: so sah unser ursprünglicher plot aus
# ohne Differenzierung nach Gender):
allbus_c_2023_raw %>%
  dplyr::mutate(fr07 = haven::as_factor(
    dplyr::case_when(fr07 < 0 ~ NA,
                     .default = fr07)
  )) %>%
  tidyr::drop_na(fr07) %>% # oder dplyr::filter(!is.na(fr07))
  ggplot2::ggplot(mapping = aes(x = fr07)) +
  ggplot2::geom_bar()

# nun muss ich sex als fill-Variable auf der aestetics-Layer hinzufügen:
allbus_c_2023_raw %>%
  dplyr::mutate(fr07 = haven::as_factor(
    dplyr::case_when(fr07 < 0 ~ NA, .default = fr07))) %>%
  dplyr::filter(!is.na(fr07)) %>%
  ggplot2::ggplot(mapping = aes(x = fr07, fill = haven::as_factor(sex))) +
  ggplot2::geom_bar()
# siehe Legende: das passiert, wenn ich die variable vorher nicht bereinige
# können wir so lassen, sauberer wäre aber...

# look for sex
allbus_c_2023_raw %>%
  labelled::look_for("sex")

# so ist es korrekt
allbus_c_2023_raw %>%
  dplyr::mutate(
    fr07 = haven::as_factor(dplyr::case_when(fr07 < 0 ~ NA, .default = fr07)),
    sex = haven::as_factor(dplyr::case_when(sex < 0 ~ NA, .default = sex))
  ) %>%
  dplyr::filter(!is.na(fr07),
                !is.na(sex)) %>%
  ggplot2::ggplot(mapping = aes(x = fr07, fill = sex)) +
  ggplot2::geom_bar()

# und noch der letzte Feinschliff
allbus_c_2023_raw %>%
  dplyr::mutate(
    fr07 = haven::as_factor(dplyr::case_when(fr07 < 0 ~ NA, .default = fr07)),
    sex  = haven::as_factor(dplyr::case_when(sex  < 0 ~ NA, .default = sex))
  ) %>%
  dplyr::filter(!is.na(fr07), !is.na(sex)) %>%
  ggplot2::ggplot(aes(x = fr07, fill = sex)) +
  ggplot2::geom_bar(color = "black") +
  ggplot2::scale_y_continuous(breaks = seq(0, 5000, by = 100)) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    axis.title.y = element_blank()
  )

# in %
allbus_c_2023_raw %>%
  dplyr::mutate(
    fr07 = haven::as_factor(dplyr::case_when(fr07 < 0 ~ NA, .default = fr07)),
    sex  = haven::as_factor(dplyr::case_when(sex  < 0 ~ NA, .default = sex))
  ) %>%
  dplyr::filter(!is.na(fr07), !is.na(sex)) %>%
  ggplot2::ggplot(aes(x = fr07, fill = sex)) +
  ggplot2::geom_bar(color = "black", position = "fill") + # stapelt auf 100%
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  )

# Allerdings ist im Allbus Ostdeutschland "oversampled",
# d.h. es gibt mehr Ostdeutsche in der Stichprobe als in der Bevölkerung.
# Das könnte die Ergebnisse verzerren.
# Deshalb müssen wir die Daten gewichten, um die Verzerrung zu korrigieren.


# 06 Gewichte --------------------------------
allbus_c_2023_raw %>%
  dplyr::mutate(
    fr07 = haven::as_factor(dplyr::case_when(fr07 < 0 ~ NA, .default = fr07)),
    sex  = haven::as_factor(dplyr::case_when(sex  < 0 ~ NA, .default = sex))
  ) %>%
  dplyr::filter(!is.na(fr07), !is.na(sex)) %>%
  ggplot2::ggplot(aes(x = fr07, fill = sex, weight = wghtpew)) + #Gewichtung
  ggplot2::geom_bar(color = "black", position = "fill") + # stapelt auf 100%
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  )
# Änderungen sind in diesem Fall marginal



# ZUSAMMENHÄNGE --------------------

# Je nach Skalenniveau:
# kategorial × kategorial => Kreuztabelle, gruppierter/gestapelter Barplot
# metrisch × metrisch => Scatterplot, Korrelation (Pearson)
# kategorial x metisch => Verteilung der metrischen Variable je Gruppe
  # dargestellt als  Violin-Plot, Dichteplot, Boxplot, gruppiertes Histogramm
  # und/ oder Spearman Korrelation


# 07 Kreuztabellen -----------------------

# Die Frage nach dem Zusammenhang zwischen angegebenem Geschlecht und
# Berufstätigkeit könnte ebenfalls interessant sein:
allbus_c_2023_raw %>%
  labelled::look_for("work")

allbus_c_2023_raw %>%
  count(work)

allbus_c_2023_raw %>%
  count(sex)

# zunächst Bereinigung:
  # negative Codes -> NA
  # haven_labelled -> Faktor
  # leere Faktorlevels entfernen
allbus_c_2023 <- allbus_c_2023_raw %>%
  dplyr::mutate(
    sex_f  = haven::as_factor(dplyr::case_when(sex  < 0 ~ NA, .default = sex)) %>%
      forcats::fct_drop(),
    work_f = haven::as_factor(dplyr::case_when(work < 0 ~ NA, .default = work)) %>%
      forcats::fct_drop() #  entfernt leere Faktorlevels
  )

allbus_c_2023 %>%
  count(work_f)

allbus_c_2023 %>%
  count(sex_f)


# Kreuztabelle

# für den schnellen überblick
gmodels::CrossTable(allbus_c_2023$sex_f, allbus_c_2023$work_f,
                    prop.r = TRUE,  # Zeilenprozente
                    prop.c = FALSE, # keine Spaltenprozente
                    prop.t = FALSE, # kein Gesamtanteil
                    prop.chisq = FALSE # kein Chi-Quadrat-Anteil
)
# merke: Gewichten kann das nicht!


# etwas umständlich mit dplyr
allbus_c_2023 %>%
  dplyr::count(sex_f, work_f)

allbus_c_2023 %>%
  dplyr::filter(!is.na(sex_f), !is.na(work_f)) %>%
  dplyr::count(sex_f, work_f) %>%
  tidyr::pivot_wider(names_from = work_f, values_from = n)

# mit janitor für Zeilenprozent
allbus_c_2023 %>%
  dplyr::filter(!is.na(sex_f), !is.na(work_f)) %>%
  janitor::tabyl(sex_f, work_f, show_missing_levels = FALSE) %>%
  janitor::adorn_totals(where = c("row", "col")) %>%
  janitor::adorn_percentages(denominator = "row") %>%
  janitor::adorn_pct_formatting(digits = 1) %>%
  janitor::adorn_ns(position = "front") # %>% knitr::kable() # für quarto


# mit gtsummary if you want it to look really fancy
allbus_c_2023 %>%
  gtsummary::tbl_cross(
    row = sex_f,
    col = work_f,
    percent = "row",
    missing = "no",
    margin  = "row"  # "row", "col", oder c("row", "col")
  )
# Von den Befragten Männern ist über die Hälfte Vollzeitbeschäftigt,
# Von den befragten Frauen nur 30%


# BONUS: gewichtete Kreuztabellen mit gtsummary
# wenn man n und % zugleich darstellen will
allbus_c_2023 %>%
  dplyr::filter(!is.na(sex_f), !is.na(work_f)) %>%
  survey::svydesign(~1, data = ., weights = ~wghtpew) %>%
  gtsummary::tbl_svysummary(
    by = work_f, # spalten
    include = sex_f, # zeilen
    percent  = "row",
    missing  = "no"
  ) %>%
  gtsummary::add_overall(last = TRUE)



# alternativ: mit questionr für Gewichtungen
questionr::wtd.table(allbus_c_2023$sex_f, allbus_c_2023$work_f,
                     weights = allbus_c_2023$wghtpew)





# 08 Korrelationskoeffizienten ----------------

# den aus der Kreuztabelle bereits vermuteten Zusammenhang können
# wir auch berechenen:
# zwei kategoriale Variablen => Chi-Quadrat-Test + Cramér's V


# Wie sind die Variablen kodiert?

# mit count
allbus_c_2023 %>%
  dplyr::count(sex_f)
allbus_c_2023 %>%
  dplyr::count(work_f)

# oder mit levels
levels(allbus_c_2023$sex_f) %>%
  tibble::enframe()
levels(allbus_c_2023$work_f) %>%
  tibble::enframe()
# hier ist die Variable wie folgt codiert:
# Geschlecht männlich => weiblich
# Erwerbsstatus: Vollzeit => Nicht erwerbstätig
# WICHTIG FÜR INTERPRETATION!

# sex_f: nominal-polytom (3 Kategorien)
# work_f: ordinal-kategorial (4 Kategorien mit Reihenfolge)

# also: Cramers V und Chi²-Test
effectsize::cramers_v(
  stats::chisq.test(allbus_c_2023$sex_f, allbus_c_2023$work_f),
  ci = 0.95,
  alternative = "two.sided"
)
# In diesem Fall ist der Zusammenhang zwischen Geschlecht und Erwerbsstatus
# schwach bis moderat (Cramér's V von 0.22)

sum(stats::chisq.test(allbus_c_2023$sex_f, allbus_c_2023$work_f)$observed)

# alternativ wäre auch dichotom und ordinal vertretbar
# dann spearman
# da können wir dann auch ablesen, ob der Zusammenhang positiv oder negativ ist
allbus_c_2023 %>%
  dplyr::filter(sex_f %in% c("MANN", "FRAU")) %>%
  dplyr::mutate(
    sex_d  = as.integer(sex_f),
    work_d = as.integer(work_f)
  ) %>%
  correlation::correlation(
    select = "sex_d",
    select2 = "work_d",
    method= "spearman"
  )
# In diesem Fall hat Frausein einen schwachen, positiven Zusammenhang
# (rho: 0.15) mit einem geringeren Erwerbsstatus.



## Beispiel politisches Vertrauen: ----------------------
# Hängen Vertrauen zu Parteien und Vertrauen in den Bundestag zusammen?

allbus_c_2023 %>%
  labelled::look_for("Vertrauen")

# Missings ansehen und bereinigen
allbus_c_2023 <- allbus_c_2023 %>%
  dplyr::mutate(
    pt03_d = dplyr::case_when(pt03 < 0 ~ NA_real_, .default = as.double(pt03)),
    pt15_d = dplyr::case_when(pt15 < 0 ~ NA_real_, .default = as.double(pt15))
  )


# checks
allbus_c_2023 %>%
  count(pt03_d)

allbus_c_2023 %>%
  tidyr::drop_na(pt03_d) %>%
  count()

allbus_c_2023 %>%
  count(pt15_d)


# Korrelation berechnen:

# Option a) schick mit correlation package
cor_01 <- allbus_c_2023 %>%
  dplyr::select(pt03_d, pt15_d) %>%
  correlation::correlation()
# correlation nutzt standardmäßig den Pearson-Korrelationskoeffizienten
# In diesem Fall ist das vertretbar, weil beide Variablen quasi-metrisch sind.

summary(cor_01)


# Option b) mit baseR cor() und report::report
stats::cor.test(allbus_c_2023$pt03_d, allbus_c_2023$pt15_d)

stats::cor.test(allbus_c_2023$pt03_d, allbus_c_2023$pt15_d) %>%
  report::report()


# scatter plot für einen ersten Überblick
allbus_c_2023 %>%
  ggplot(aes(x = pt15_d, y = pt03_d)) +
  geom_point(position = "jitter", alpha = 0.3) +
  labs(x = "Vertrauen in politische Parteien",
       y = "Vertrauen in den Bundestag") +
  theme_minimal()


# Korrelationsmatrix
# Na-Bereinigung für alle Vertrauensvariablen
allbus_c_2023 <- allbus_c_2023 %>%
  dplyr::mutate(
    across(
      c(pt01, pt02, pt03, pt04, pt06, pt07, pt08, pt09, pt10, pt11, pt12, pt14, pt15, pt19, pt20),
      ~ dplyr::case_when(.x < 0 ~ NA_real_, .default = as.double(.x)),
      .names = "{.col}_d"
    )
  )

allbus_c_2023 %>%
  select(ends_with("_d")) %>%
  correlation::correlation()

# Hinweis: mit dem method-Argument kannst du innerhalb des correlation-Pakets
  # und auch in stats::cor.test() das Korrelationsmaß ändern,
  # beispielsweise mit method = "spearman" für Spearman's rho bei ordinal
  # skalierten Variablen.

# Und nächste Woche ... Einfache und multiple OLS-Regressionen!

