# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# SITZUNG 09 Regressionen                               -----------
# ______________________________________________________-----------


# 01 setup -------------

# Pakete
library(dplyr)


# Daten
allbus_c_2023_raw <- haven::read_dta("./data/ZA8831_v1-3-0.dta")



# 02 Inferenzstatistik - Beispiel --------------

# Bei Inferenzstatistik geht es darum, herauszufinden, ob ein in der
# Stichprobe gefundener Effekt (wahrscheinlich) auch in der Grundgesamtheit
# existiert.

# Dafür können grundsätzlich zwei verschiedne methoden der Schätzung angewendet werden
# intervallschätzung (haben iene Ober-und Untergrenze)
# und Punktschätzung (p = ... oder ***)


# Wenn wir beispielsweise die gewichtete Kreuztabelle aus der
# vergangenenen Sitzung nehmen, in der es um die Frage ging, ob es einen
# Zusammenhang zwischen Geschlecht und Erwerbsstatus gibt ...
allbus_c_2023 %>%
  dplyr::filter(!is.na(sex_f), !is.na(work_f)) %>%
  survey::svydesign(~1, data = ., weights = ~wghtpew) %>%
  gtsummary::tbl_svysummary(
    by      = work_f,
    include = sex_f,
    percent = "row",
    missing = "no"
  ) %>%
  gtsummary::add_p() %>%
  gtsummary::add_overall(last = TRUE)
# können wir mit einem Chi-Quadrat-Test herausfinden,
# ob der beobachtete Zusammenhang in der Grundgesamtheit
# wahrscheinlich auch existiert.

# Für die Zusammenhangsmaße aus der letzten Sitzug heißt das:
# Es gibt immer zwei Kennzahlen
# Die Effektstärke (z.B. Cramér's V) gibt an, wie stark der Zusammenhang ist,
# während der p-Wert (z.B. aus dem Chi²-Test) angibt, ob er signifikant ist


# Chi² Test
# mit report
stats::chisq.test(allbus_c_2023$sex_f, allbus_c_2023$work_f) %>%
  report::report()

sum(stats::chisq.test(allbus_c_2023$sex_f, allbus_c_2023$work_f)$observed)

effectsize::cramers_v(
  stats::chisq.test(allbus_c_2023$sex_f, allbus_c_2023$work_f),
  ci = 0.95,
  alternative = "two.sided"
)


# 03 Regressionen -------------

# Regressionen sind ein Verfahren, um den Zusammenhang zwischen einer abhängigen
# Variable (y) und einer oder mehreren unabhängigen Variablen (x) zu modellieren.

# Beispiel
# Forschungsfrage: Lässt sich Rechts-Links-Selbstverortung auf das Alter zurückführen?
