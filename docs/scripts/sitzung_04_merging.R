# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# SITZUNG 04 Data merging                               -----------
# ______________________________________________________-----------


# 1 setup --------------------

# work directory überprüfen
getwd()

# Pakete installieren und laden
library(rio) # daten einlesen
library(dplyr) # %>%



# 2. Daten importieren & vorbereiten (recap) --------------------

# Ergebnisdaten (siehe Sitzung 2)
btw_2025_ergebnisse_raw <- rio::import("./data/kerg2.csv",
                                       sep = ";",
                                       dec = ",",
                                       skip = 9)


btw_2025_ergebnisse <- btw_2025_ergebnisse_raw %>%
  janitor::clean_names()

# Strukturdaten (siehe Übung 1)
btw_2025_strukturdaten_raw <- rio::import(
  "data/btw2025_strukturdaten.csv",
  sep = ";",
  dec = ",",
  skip = 9
)

btw_2025_strukturdaten <- btw_2025_strukturdaten_raw %>%
  janitor::clean_names()


btw_2025_strukturdaten <- btw_2025_strukturdaten %>%
  dplyr::rename(
    gemeinden = gemeinden_am_31_12_2023_anzahl,
    flache_km2 = flache_am_31_12_2023_km2,
    bev_insgesamt_1000 = bevolkerung_am_31_12_2023_insgesamt_in_1000,
    bev_deutsche_1000 = bevolkerung_am_31_12_2023_deutsche_in_1000,
    bev_auslander_pct = bevolkerung_am_31_12_2023_auslander_innen_percent,
    bev_dichte_ew_km2 = bevolkerungsdichte_am_31_12_2023_ew_je_km2,
    geburtensaldo_je_1000_ew = zu_bzw_abnahme_der_bevolkerung_2023_geburtensaldo_je_1000_ew,
    wanderungssaldo_je_1000_ew = zu_bzw_abnahme_der_bevolkerung_2022_wanderungssaldo_je_1000_ew,
    alter_unter_18_pct = alter_von_bis_jahren_am_31_12_2023_unter_18_percent,
    alter_18_24_pct = alter_von_bis_jahren_am_31_12_2023_18_24_percent,
    alter_25_34_pct = alter_von_bis_jahren_am_31_12_2023_25_34_percent,
    alter_35_59_pct = alter_von_bis_jahren_am_31_12_2023_35_59_percent,
    alter_60_74_pct = alter_von_bis_jahren_am_31_12_2023_60_74_percent,
    alter_75plus_pct = alter_von_bis_jahren_am_31_12_2023_75_und_mehr_percent,
    boden_siedlung_verkehr_pct = bodenflache_nach_art_der_tatsachlichen_nutzung_am_31_12_2022_siedlung_und_verkehr_percent,
    boden_vegetation_gewaesser_pct  = bodenflache_nach_art_der_tatsachlichen_nutzung_am_31_12_2022_vegetation_und_gewasser_percent,
    wohnungen_fertig_je_1000_ew = fertiggestellte_wohnungen_2023_je_1000_ew,
    wohnungen_bestand_je_1000_ew = bestand_an_wohnungen_am_31_12_2023_insgesamt_je_1000_ew,
    wohnflache_je_wohnung = wohnflache_am_31_12_2023_je_wohnung,
    wohnflache_je_ew = wohnflache_am_31_12_2023_je_ew,
    pkw_je_1000ew = pkw_bestand_am_01_01_2024_pkw_insgesamt_je_1000_ew,
    pkw_elektro_hybrid_pct = pkw_bestand_am_01_01_2024_pkw_mit_elektro_oder_hybrid_antrieb_percent,
    unternehmen_je_1000ew = unternehmensregister_2022_unternehmen_insgesamt_je_1000_ew,
    handwerk_je_1000ew = unternehmensregister_2022_handwerksunternehmen_je_1000_ew,
    schulabg_beruflich_2022 = schulabganger_innen_beruflicher_schulen_2022,
    schulabg_allg_je_1000ew = schulabganger_innen_allgemeinbildender_schulen_2022_insgesamt_ohne_externe_je_1000_ew,
    schulabg_ohne_abschluss_pct = schulabganger_innen_allgemeinbildender_schulen_2022_ohne_hauptschulabschluss_percent,
    schulabg_hauptschule_pct = schulabganger_innen_allgemeinbildender_schulen_2022_mit_hauptschulabschluss_percent,
    schulabg_mittlere_reife_pct = schulabganger_innen_allgemeinbildender_schulen_2022_mit_mittlerem_schulabschluss_percent,
    schulabg_abitur_pct = schulabganger_innen_allgemeinblldender_schulen_2022_mit_allgemeiner_und_fachhochschulreife_percent,
    kita_unter_3_quote = kindertagesbetreuung_am_01_03_2023_betreute_kinder_unter_3_jahre_betreuungsquote,
    kita_3_bis_6_quote = kindertagesbetreuung_am_01_03_2023_betreute_kinder_3_bis_unter_6_jahre_betreuungsquote,
    einkommen_je_ew_2021 = verfugbares_einkommen_der_privaten_haushalte_2021_eur_je_ew,
    bip_je_ew_2021 = bruttoinlandsprodukt_2021_eur_je_ew,
    svb_insgesamt_je_1000ew = sozialversicherungspflichtig_beschaftigte_am_30_06_2023_insgesamt_je_1000_ew,
    svb_landwirtschaft_pct = sozialversicherungspflichtig_beschaftigte_am_30_06_2023_land_und_forstwirtschaft_fischerei_percent,
    svb_prod_gewerbe_pct = sozialversicherungspflichtig_beschaftigte_am_30_06_2023_produzierendes_gewerbe_percent,
    svb_handel_gast_verkehr_pct = sozialversicherungspflichtig_beschaftigte_am_30_06_2023_handel_gastgewerbe_verkehr_percent,
    svb_oeffentl_dienstl_pct = sozialversicherungspflichtig_beschaftigte_am_30_06_2023_offentliche_und_private_dienstleister_percent,
    svb_uebrige_dienstl_pct = sozialversicherungspflichtig_beschaftigte_am_30_06_2023_ubrige_dienstleister_und_ohne_angabe_percent,
    sgb_2_insgesamt_je_1000ew = empfanger_innen_von_leistungen_nach_sgb_ii_august_2024_insgesamt_je_1000_ew,
    sgb_2_nicht_erwerbsfaehig_pct = empfanger_innen_von_leistungen_nach_sgb_ii_august_2024_nicht_erwerbsfahige_hilfebedurftige_percent,
    sgb_2_auslander_pct = empfanger_innen_von_leistungen_nach_sgb_ii_august_2024_auslander_innen_percent,
    alo_quote_insgesamt = arbeitslosenquote_november_2024_insgesamt,
    alo_quote_maenner = arbeitslosenquote_november_2024_manner,
    alo_quote_frauen = arbeitslosenquote_november_2024_frauen,
    alo_quote_15_24 = arbeitslosenquote_november_2024_15_bis_24_jahre,
    alo_quote_55_64 = arbeitslosenquote_november_2024_55_bis_64_jahre
  )




# Dazu müssen wir unsere bereits vorbereiteten Datensätze speichern und einlesen.
   # Führe das Skript aus Sitzung 2 komplett aus. Setze dann am unteren Ende
   # folgendes Befehl ein: save(btw_2025_ergebnisse, file = "data/btw_2025_ergebnisse.RData").
   # Öffne anschließend das Lösungsskript zur ersten Übung. Ergänze auch dort ganz
   # unten: save(btw_2025_strukturdaten, file = "data/btw_2025_strukturdaten.RData").
   # Führe dann das gesamte Skript aus.

# Nun können wir beide hier mit dem base R Befehl load() einlesen.
load("output/btw_2025_ergebnisse.RData")
load("output/btw_2025_strukturdaten.RData")



# 3. Daten mergen --------------------

# Beide Datensätze enthalten Angaben zur Wahlkreisnummer.

btw_2025_ergebnisse %>%
  distinct(gebietsnummer, gebietsart, gebietsname) %>%
  View()
   # dabei steht 99 hier für das Bundesgebiet.

btw_2025_strukturdaten %>%
  distinct(wahlkreis_nr, wahlkreis_name) %>%
  View()
   # Hier zeigen die Nummern über 900 Aggregatzahlen an, für die Bundesländer und
   # das gesamte Bundesgebiet. Wahlkreise tragen die Nummern 1-299.

# Insgesamt ist das Problem, dass zwar beide Datensätze Angaben zu a) Wahlkreisen, b) Bundes-
   # ländern und c) dem Bundesgebiet enthalten, diese Daten aber im Strukturdatensatz
   # in einer einzigen Variable stecken, im Ergebnisdatensatz hingegen in drei
   # verschiedenen: gebietsnummer, gebietsart, und gebietsname.

# Um also die Strukturdaten den Ergebnisdaten hinzufügen zu können, müssen wir
   # deren Struktur an jene des Ergebnisdatensatzes anpassen:

# Strukturdaten für Merge vorbereiten
btw_2025_strukturdaten_merge <- btw_2025_strukturdaten %>%
  dplyr::mutate(
    gebietsart = dplyr::case_when( # neue Variable Gebietsart erstellen
      wahlkreis_nr == 999 ~ "Bund",
      wahlkreis_nr >= 901 ~ "Land",
      TRUE ~ "Wahlkreis"
    ),
    wahlkreis_nr = dplyr::case_when( # die nummern an jene im ergebnisdatensatz anpassen
      wahlkreis_nr == 999 ~ 99,
      wahlkreis_nr == 901 ~ 1,
      wahlkreis_nr == 902 ~ 2,
      wahlkreis_nr == 903 ~ 3,
      wahlkreis_nr == 904 ~ 4,
      wahlkreis_nr == 905 ~ 5,
      wahlkreis_nr == 906 ~ 6,
      wahlkreis_nr == 907 ~ 7,
      wahlkreis_nr == 908 ~ 8,
      wahlkreis_nr == 909 ~ 9,
      wahlkreis_nr == 910 ~ 10,
      wahlkreis_nr == 911 ~ 11,
      wahlkreis_nr == 912 ~ 12,
      wahlkreis_nr == 913 ~ 13,
      wahlkreis_nr == 914 ~ 14,
      wahlkreis_nr == 915 ~ 15,
      wahlkreis_nr == 916 ~ 16,
      TRUE ~ wahlkreis_nr
    )
  )

# Merge durchführen
btw_2025_erg_struk <- btw_2025_ergebnisse %>%
  dplyr::left_join(btw_2025_strukturdaten_merge,
                   by = c("gebietsnummer" = "wahlkreis_nr", "gebietsart" = "gebietsart")
  )



# 4 Labels ----------

# Ergebnisdaten

# variablennamen ausgeben lassen zum Kopieren
btw_2025_ergebnisse %>%
  names()

# Liste mit Labels bauen
labels_btw_2025_ergebnisse <- list(
  wahlart = "Wahlart: Bundes, -Landes, -Kommunalwahl ",
  wahltag = "Datum des Wahltags",
  gebietsart = "Gebietsart: Bundesland, Wahlkreis, oder gesamtes Staatsgebiet",
  gebietsnummer = "Gebietsnummer",
  gebietsname = "Gebietsname",
  ueg_gebietsart = "Übergeordnete Gebietsart",
  ueg_gebietsnummer = "Übergeordnete Gebietsnummer",
  gruppenart = "Gruppenart: Partei, Einzelbewerber, System",
  gruppenname = "Gruppenname: z.B. Parteiname",
  gruppenreihenfolge = "Reihenfolge der Gruppe",
  stimme = "Stimmart (Erst-/Zweitstimme)",
  anzahl = "Anzahl Stimmen",
  prozent = "Stimmenanteil in Prozent",
  vorp_anzahl = "Anzahl Stimmen bei vorherangegangener Wahl",
  vorp_prozent = "Stimmenanteil bei vorherangegangener Wahl in Prozent",
  diff_prozent = "Veränderung zur vorherangegangenen Wahl in Prozent",
  diff_prozent_pkt = "Veränderung zur vorherangegangenen Wahl in Prozentpunkten",
  gewahlt = "Gewählt: Pateiname"
)


# Labels anbringen
btw_2025_erg_struk <- btw_2025_erg_struk %>%
  labelled::set_variable_labels(.labels = labels_btw_2025_ergebnisse,
                                .strict = FALSE)



# Strukturdaten

btw_2025_strukturdaten_labels <- stats::setNames(
  object = names(btw_2025_strukturdaten_raw), # Die Labels/Beschreibungen (Werte)
  nm = names(btw_2025_strukturdaten)          # Die aktuellen Spaltennamen (Namen)
)

# Jetzt als Liste an labelled übergeben
btw_2025_erg_struk <- btw_2025_erg_struk %>%
  labelled::set_variable_labels(
    .labels = as.list(btw_2025_strukturdaten_labels),
    .strict = FALSE
  )


# 5 Speichern ------------------

save(btw_2025_erg_struk, file = "output/btw_2025_erg_struk.RData")

View(btw_2025_erg_struk)

# Übersicht mit summarytools:
btw_2025_erg_struk %>%
  summarytools::dfSummary(
    max.distinct.values = 3,
    na.col = FALSE) %>%
  print(file = "output/btw_2025_erg_struk_summary.html")

