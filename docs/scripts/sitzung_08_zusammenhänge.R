# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# SITZUNG 08 Zusammenhänge                              -----------
# ______________________________________________________-----------


# setup ---------------------
library(haven)
library(dplyr)
library(ggplot2)
library(gtsummary) # ggf. vorher installieren!



# data ------------------------

# Lade den ALLBUS compact herunter (siehe Webseite => Daten)
# Lege ihn in deinen data-Ordner.
# Einlesen tun wir ihn mit dem haven-Paket:


allbus_c_2023_raw <- haven::read_dta("data/ZA8831_v1-3-0.dta")
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

# Überblick bestimmte Variablen:
# Position, Name, Label, Klasse, Values
allbus_c_2023_raw %>%
  labelled::look_for("wirtschaftslage")

# das gibt mir darüber Auskunft a) was zu NA gemacht werden muss
# und b) welche Labels zu den Werten gehören, damit ich entscheiden kann,
# ob ich die Variable als Faktor oder metrisch weiterverarbeite.

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


# staked bar charts ----------------

# anknüpfend an Übung 05 interesseirt uns vielleicht, inwiefern sich die
# Einschätzung, eine vollzeit arbeitende Frau könne eine gute Mutter sein,
# nach gender unterscheidet:

allbus_c_2023_raw %>%
  dplyr::mutate(fr07 = haven::as_factor(
    dplyr::case_when(fr07 < 0 ~ NA,
                     .default = fr07)
  )) %>%
  tidyr::drop_na(fr07) %>% # oder dplyr::filter(!is.na(fr07))
  ggplot2::ggplot(mapping = aes(x = fr07)) +
  ggplot2::geom_bar()

# das passiert, wenn ich die variable vorher nicht bereinige:
allbus_c_2023_raw %>%
  dplyr::mutate(fr07 = haven::as_factor(
    dplyr::case_when(fr07 < 0 ~ NA, .default = fr07))) %>%
  dplyr::filter(!is.na(fr07)) %>%
  ggplot2::ggplot(mapping = aes(x = fr07, fill = haven::as_factor(sex))) +
  ggplot2::geom_bar()

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
  ggplot2::ggplot(mapping = aes(x = fr07, fill = sex)) + # fill entscheidend!
  ggplot2::geom_bar()



# weights (NOCH ERGÄNZEN, ggf. lieber srvyr) ----------------

info_weights <- allbus_c_2023_raw %>%
  labelled::look_for("wght")
# wghtpew is what we want!

allbus_c_2023_raw %>%
  count(wghtpew)

# install.packages("survey")
library(survey)

# AUS MASCH ET AL NOCH ANPASSEN!nun legen wir ein design object an
allbus.w <- svydesign(ids =~ 1, data = allbus, weights =~ wghtpew)
# ids ist übrigens für Erhebungscluster, die brauchen wir hier nicht



# Kreuztabellen -----------------------

glimpse(allbus_c_2023_raw)

allbus_c_2023_raw %>%
  count(work)

allbus_c_2023_raw %>%
  count(sex)

# Bereinigung: negative Codes -> NA, haven_labelled -> Faktor
allbus_c_2023 <- allbus_c_2023_raw %>%
  dplyr::mutate(
    sex_f  = haven::as_factor(dplyr::case_when(sex  < 0 ~ NA, .default = sex)) %>%
      forcats::fct_drop(),
    work_f = haven::as_factor(dplyr::case_when(work < 0 ~ NA, .default = work)) %>%
      forcats::fct_drop() #  entfernt leere Faktorlevels
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





# mit janitor - leichter aber ohne weihgts
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



# mit gtsummary - mit weights möglich -------------

allbus_c_2023 %>%
  gtsummary::tbl_cross(
    row = sex_f,
    col = work_f,
    percent = "row",
    missing = "no"
  )


# mit gewichten!!

allbus_c_2023 %>%
  dplyr::filter(!is.na(sex_f), !is.na(work_f)) %>%
  survey::svydesign(~1, data = ., weights = ~wghtpew) %>%
  gtsummary::tbl_svysummary(
    by = work_f,
    include = sex_f,
    percent = "row",
    missing = "no"
  )




# scatter plots
# Korrelationskoeffizienten
# und Regressionen

