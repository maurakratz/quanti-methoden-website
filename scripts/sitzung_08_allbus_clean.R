# ALLBUS ------------------

# pakete
pacman::p_load(dplyr, labelled, summarytools, haven)

# datensatz
allbus_c_2023_raw <- haven::read_dta("./data/ZA8831_v1-3-0.dta")

dailyr::var_overview(allbus_c_2023_raw) %>%  print(n = Inf)


# Labels sichern (vor der Bereinigung!)
allbus_var_labels <- labelled::var_label(allbus_c_2023_raw)

# Missings bereinigen: alle negativen Werte -> NA
allbus_c_2023_clean <- allbus_c_2023_raw %>%
  dplyr::mutate(
    across(
      where(haven::is.labelled),
      ~ {
        x <- as.double(.x)
        x[x < 0] <- NA_real_
        labelled::labelled(
          x,
          labels = attr(.x, "labels")[attr(.x, "labels") >= 0],
          label  = attr(.x, "label")
        )
      }
    )
  )

# Labels wieder anfügen
allbus_c_2023_clean <- allbus_c_2023_clean %>%
  labelled::set_variable_labels(
    .labels  = allbus_var_labels,
    .strict  = FALSE
  )

# dfSummary (das kann etwas dauern!)
allbus_c_2023_clean %>%
  summarytools::dfSummary() %>%
  summarytools::view(file = "output/allbus_c_2023_dfSummary.html")
# im Anschluss gern mit # auskommentieren, damit es nicht jedes Mal so
# lang braucht
# "

# for print
allbus_c_2023_clean %>%
  summarytools::dfSummary(
    valid.col = FALSE,
    graph.col = FALSE
  ) %>%
  summarytools::view(file = "output/allbus_c_2023_dfSummary_print.html")
