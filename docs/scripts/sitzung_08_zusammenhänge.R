# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# SITZUNG 08 Zusammenhänge                              -----------
# ______________________________________________________-----------


# setup ---------------------
library(haven)
library(dplyr)



# data ------------------------

# Lade den ALLBUS compact herunter (siehe Webseite => Daten)
# Lege ihn in deinen data-Ordner.
# Einlesen tun wir ihn mit dem haven-Paket:


allbus_c_2023_raw <- read_dta("data/ZA8831_v1-3-0.dta")
View(ZA8831_v1_3_0)



# Exkurs Datenformate: labelled-Vektoren und Faktoren

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

  allbus_c_2023_eng_raw <- read_dta("data/ZA8833_v1-0-0.dta") %>%
    haven::as_factor()


  View(ZA8833_v1_0_0)
}




# Skalenniveaus ----------------------

# Überblick alle Variablen: Position, Name, Label, Klasse, Values
allbus_c_2023_raw %>%
labelled::look_for()

# bestimmte Variablen
allbus_c_2023_raw %>%
  labelled::look_for("wirtschaftslage")

# das gibt mir darüber Auskunft a) was zu NA gemacht werden muss
# und b) welche Labels zu den Werten gehören, damit ich entscheiden kann,
# ob ich die Variable als Faktor oder metrisch weiterverarbeite.


# Es kombiniert also, was wir sonst in 3 versch. Befehlen tun müssten:
attr(allbus_c_2023_raw$ep01, "label")   # Variable Label
attr(allbus_c_2023_raw$ep01, "labels")  # Value Labels mit Rohwerten
class(allbus_c_2023_raw$ep01)           # -> "haven_labelled"


# dann kann ich, je nach dem was ich tun will, die Variable weiterverarbeiten.
# Bsp:

allbus_c_2023_raw %>%
  dplyr::mutate(
    ep01_f = haven::as_factor(ep01),    # ordinal/kategorial -> Faktor
    age_d  = as.double(age)             # metrisch -> double
  ) %>%
  select(ep01, ep01_f, age, age_d)
# Entscheidungshilfe:
## kategorial -> as_factor(),
## metrisch -> as.integer()/as.double()


# Kreuztabellen -----------------------

glimpse(allbus_c_2023_raw)

allbus_c_2023_raw %>%
  count(work)

allbus_c_2023_raw %>%
  count(sex)

# Bereinigung: negative Codes -> NA, haven_labelled -> Faktor
allbus_c_2023 <- allbus_c_2023_raw %>%
  dplyr::mutate(
    sex_f  = haven::as_factor(dplyr::case_when(sex  < 0 ~ NA, .default = sex)),
    work_f = haven::as_factor(dplyr::case_when(work < 0 ~ NA, .default = work))
  )

allbus_c_2023 %>%
  count(work_f)

# Kreuztabelle

# für den schnellen überblick
library(gmodels)
gmodels::CrossTable(allbus_c_2023$sex_f, allbus_c_2023$work_f,
                    prop.r    = TRUE,  # Zeilenprozente
                    prop.c    = FALSE, # keine Spaltenprozente
                    prop.t    = FALSE, # kein Gesamtanteil
                    prop.chisq = FALSE # kein Chi-Quadrat-Anteil
)

# etwas umständlich mit dplyr
allbus_c_2023 %>%
  dplyr::count(sex_f, work_f)

allbus_c_2023 %>%
  dplyr::filter(!is.na(sex_f), !is.na(work_f)) %>%
  dplyr::count(sex_f, work_f) %>%
  tidyr::pivot_wider(names_from = work_f, values_from = n)


library(srvyr)

allbus_c_2023 %>%
  dplyr::filter(!is.na(sex_f), !is.na(work_f)) %>%
  srvyr::as_survey_design(weights = wghtpew) %>%
  srvyr::group_by(sex_f, work_f) %>%
  srvyr::summarise(n = srvyr::survey_total(),
                   pct = srvyr::survey_mean() * 100)


# questionr macht zwar weights aber janitor ist besser für piping


# mit questionr

# absolute Häufigkeiten
questionr::wtd.table(allbus_c_2023$sex_f, allbus_c_2023$work_f,
                     weights = allbus_c_2023$wghtpew)

# Zeilenprozente
questionr::wtd.table(allbus_c_2023$sex_f, allbus_c_2023$work_f,
                     weights = allbus_c_2023$wghtpew) %>%
  questionr::rprop()

questionr::wtd.table(allbus_c_2023$sex_f, allbus_c_2023$work_f,
                     weights = allbus_c_2023_raw$wghtpew) %>%
  questionr::rprop()  # Zeilenprozente

# mit janitor:
allbus_c_2023 %>%
  dplyr::filter(!is.na(sex_f), !is.na(work_f)) %>%
  janitor::tabyl(sex_f, work_f, show_missing_levels = FALSE) %>%
  janitor::adorn_percentages("row") %>%
  janitor::adorn_pct_formatting() %>%
  janitor::adorn_ns()


allbus_c_2023 %>%
  dplyr::filter(!is.na(sex_f), !is.na(work_f)) %>%
janitor::tabyl(sex_f, work_f, show_missing_levels = FALSE) %>%
  janitor::adorn_totals(where = c("row", "col")) %>%
  janitor::adorn_percentages(denominator = "row") %>%
  janitor::adorn_pct_formatting(digits = 1) %>%
  janitor::adorn_ns(position = "front") # %>% knitr::kable() # für quarto


library(gtsummary)

gtsummary::tbl_cross(
  data = allbus_c_2023,
  row = sex_f,
  col = work_f,
  percent = "row",
  missing = "no"
)


# scatter plots
# Korrelationskoeffizienten
# und Regressionen

