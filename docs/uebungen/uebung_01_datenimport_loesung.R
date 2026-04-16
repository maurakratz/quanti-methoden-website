# QUANTITATIVE DATENANALYSE IN R | Maura Kratz | SoSe26 -----------
# LÖSUNG ZU ÜBUNG 1: Datenimport                        -----------
# ______________________________________________________-----------


# Aufgabe 1 ------------------
   # Lade dir die Wahlkreisstrukturdaten herunter und lege sie in deinen .data-Unterordner.
   # Siehe dafür auf der Kurswebseite unter dem Tab "Daten" nach.

# Aufgabe 2 ------------------
   # Lade die nötigen Pakete: rio, labelled.
   # Lese die daten mir dem rio-Paket ein. Achte darauf, ggf. überflüssige Zeilen
   # zu überspringen. Sieh dir das Ergebnis an.

library(rio)
library(labelled)

btw_2025_strukturdaten_raw <- rio::import(
  "data/btw2025_strukturdaten.csv",
  skip = 9
)

# Aufgabe 3 ------------------------
   # Bereinige die Variablennamen.
btw_2025_strukturdaten <- btw_2025_strukturdaten_raw %>%
  janitor::clean_names()


# Aufgabe 4 ---------------------
   # Lass dir die Variablennamen ausgeben. Ändere sie zu kurzen, sinnvollen Namen um
   # (siehe Beispiel). Füge mit labelled::set_variable_labels() den neuen Namen
   # entsprechende Labels hinzu.

names(btw_2025_strukturdaten)

# a) Zu lange Namen umbenennen
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

# b) Labels hinzufügen
btw_2025_strukturdaten <- btw_2025_strukturdaten %>%
  labelled::set_variable_labels(
    land = "Bundesland",
    wahlkreis_nr = "Wahlkreisnummer",
    wahlkreis_name = "Wahlkreisname",
    gemeinden = "Anzahl Gemeinden (31.12.2023)",
    flache_km2 = "Fläche in km² (31.12.2023)",
    bev_insgesamt_1000 = "Bevölkerung insgesamt in 1000 (31.12.2023)",
    bev_deutsche_1000 = "Deutsche Bevölkerung in 1000 (31.12.2023)",
    bev_auslander_pct = "Ausländeranteil in % (31.12.2023)",
    bev_dichte_ew_km2 = "Bevölkerungsdichte EW/km² (31.12.2023)",
    geburtensaldo_je_1000_ew = "Geburtensaldo je 1000 EW (2023)",
    wanderungssaldo_je_1000_ew = "Wanderungssaldo je 1000 EW (2022)",
    alter_unter_18_pct = "Anteil unter 18 Jahre in % (31.12.2023)",
    alter_18_24_pct = "Anteil 18–24 Jahre in % (31.12.2023)",
    alter_25_34_pct = "Anteil 25–34 Jahre in % (31.12.2023)",
    alter_35_59_pct = "Anteil 35–59 Jahre in % (31.12.2023)",
    alter_60_74_pct = "Anteil 60–74 Jahre in % (31.12.2023)",
    alter_75plus_pct = "Anteil 75+ Jahre in % (31.12.2023)",
    boden_siedlung_verkehr_pct = "Siedlungs- und Verkehrsfläche in % (31.12.2022)",
    boden_vegetation_gewaesser_pct = "Vegetation und Gewässer in % (31.12.2022)",
    wohnungen_fertig_je_1000_ew = "Fertiggestellte Wohnungen je 1000 EW (2023)",
    wohnungen_bestand_je_1000_ew = "Wohnungsbestand je 1000 EW (31.12.2023)",
    wohnflache_je_wohnung = "Wohnfläche je Wohnung in m² (31.12.2023)",
    wohnflache_je_ew = "Wohnfläche je EW in m² (31.12.2023)",
    pkw_je_1000ew = "PKW je 1000 EW (01.01.2024)",
    pkw_elektro_hybrid_pct = "Anteil Elektro-/Hybrid-PKW in % (01.01.2024)",
    unternehmen_je_1000ew = "Unternehmen je 1000 EW (2022)",
    handwerk_je_1000ew = "Handwerksunternehmen je 1000 EW (2022)",
    schulabg_beruflich_2022 = "Schulabgänger berufliche Schulen (2022)",
    schulabg_allg_je_1000ew = "Schulabgänger allgemeinbildend je 1000 EW (2022)",
    schulabg_ohne_abschluss_pct = "Schulabgänger ohne Abschluss in % (2022)",
    schulabg_hauptschule_pct = "Schulabgänger mit Hauptschulabschluss in % (2022)",
    schulabg_mittlere_reife_pct = "Schulabgänger mit mittlerem Abschluss in % (2022)",
    schulabg_abitur_pct = "Schulabgänger mit Abitur/FH-Reife in % (2022)",
    kita_unter_3_quote = "Kita-Betreuungsquote unter 3 Jahre (01.03.2023)",
    kita_3_bis_6_quote = "Kita-Betreuungsquote 3–6 Jahre (01.03.2023)",
    einkommen_je_ew_2021 = "Verfügbares Einkommen je EW in EUR (2021)",
    bip_je_ew_2021 = "BIP je EW in EUR (2021)",
    svb_insgesamt_je_1000ew = "SV-Beschäftigte insgesamt je 1000 EW (30.06.2023)",
    svb_landwirtschaft_pct = "SV-Beschäftigte Landwirtschaft in % (30.06.2023)",
    svb_prod_gewerbe_pct = "SV-Beschäftigte produzierendes Gewerbe in % (30.06.2023)",
    svb_handel_gast_verkehr_pct = "SV-Beschäftigte Handel/Gastgewerbe/Verkehr in % (30.06.2023)",
    svb_oeffentl_dienstl_pct = "SV-Beschäftigte öffentl. Dienstleister in % (30.06.2023)",
    svb_uebrige_dienstl_pct = "SV-Beschäftigte übrige Dienstleister in % (30.06.2023)",
    sgb_2_insgesamt_je_1000ew = "SGB-II-Empfänger insgesamt je 1000 EW (Aug. 2024)",
    sgb_2_nicht_erwerbsfaehig_pct = "SGB-II: nicht erwerbsfähige Hilfebedürftige in % (Aug. 2024)",
    sgb_2_auslander_pct = "SGB-II: Ausländeranteil in % (Aug. 2024)",
    alo_quote_insgesamt  = "Arbeitslosenquote insgesamt (Nov. 2024)",
    alo_quote_maenner = "Arbeitslosenquote Männer (Nov. 2024)",
    alo_quote_frauen = "Arbeitslosenquote Frauen (Nov. 2024)",
    alo_quote_15_24 = "Arbeitslosenquote 15–24 Jahre (Nov. 2024)",
    alo_quote_55_64 = "Arbeitslosenquote 55–64 Jahre (Nov. 2024)",
    fussnoten = "Fußnoten"
  )

# Aufgabe 5 --------------
  # Lass dir mit summarytools eine Datensazübersicht ausgeben.
btw_2025_strukturdaten %>%
  summarytools::dfSummary(na.col = FALSE) %>%
  summarytools::view(file = "data/btw_2025_strukturdaten_summary.html")



# OPTIONAL: Alternative zu 4b)  --------------

# die ursprünglichen Variablennamen in btw_2025_strukturdaten_raw waren eigentlich
   # bereits gute labels
# die können wir also als Label an die neuen Variablennamen in btw_2025_strukturdaten
   # anbringen

# (named) Vektor erstellen mit neuen Namen als Werte und alte Namen als Namen der Werte
btw_2025_strukturdaten_labels <- setNames(
  names(btw_2025_strukturdaten_raw), # die alten Namen werden zu names
  names(btw_2025_strukturdaten) # neuen Spaltennamen werden zu values
)

btw_2025_strukturdaten <- btw_2025_strukturdaten %>%
  labelled::set_variable_labels(.labels = as.list(btw_2025_strukturdaten_labels)) # Vektor in Liste umwandeln, damit labelled damit arbeiten kann



