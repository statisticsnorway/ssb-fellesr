# Hjelpefunksjoner for testoppsett ----------------------------------------

lag_test_datadoc <- function(filsti, dataset = list(), variables = list()) {
  standard_dataset <- list(
    short_name = "test_data",
    version = "1",
    id = "dataset-id-1",
    owner = "test-team",
    file_path = sub("__DOC\\.json$", ".parquet", filsti),
    metadata_created_date = "2024-01-01T00:00:00Z",
    metadata_created_by = "test@ssb.no",
    metadata_last_updated_date = "2024-01-01T00:00:00Z",
    metadata_last_updated_by = "test@ssb.no",
    contains_data_from = "2024-01-01",
    contains_data_until = "2024-12-31",
    name = list(
      list(languageCode = "nb", languageText = "Testdatasett")
    ),
    description = list(
      list(languageCode = "nb", languageText = "Beskrivelse av testdatasett")
    ),
    version_description = NULL
  )

  for (felt in names(dataset)) {
    standard_dataset[felt] <- list(dataset[[felt]])
  }

  doc <- list(
    document_version = "1.0.0",
    datadoc = list(
      percentage_complete = 50,
      dataset = standard_dataset,
      variables = variables
    )
  )

  jsonlite::write_json(
    doc,
    path = filsti,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null"
  )

  invisible(filsti)
}

lag_test_variabler <- function() {
  list(
    list(
      short_name = "kjoenn",
      id = "var-1",
      data_type = "STRING",
      name = list(
        list(languageCode = "nb", languageText = "Kjønn"),
        list(languageCode = "en", languageText = "Sex")
      ),
      definition_uri = "https://metadata.ssb.no/variable-definitions/vardef_kjoenn",
      classification_uri = "https://www.ssb.no/klass/klassifikasjoner/2",
      is_personal_data = FALSE,
      unit_type = "20",
      variable_role = "IDENTIFIER",
      data_source = "67",
      temporality_type = "FIXED",
      contains_data_from = "2024-01-01",
      contains_data_until = "2024-12-31"
    ),
    list(
      short_name = "alder",
      id = "var-2",
      data_type = "INTEGER",
      name = list(
        list(languageCode = "nb", languageText = "Alder i år")
      ),
      definition_uri = "urn:ssb:variable-definition:vardef:vardef_alder",
      classification_uri = NULL,
      is_personal_data = TRUE,
      unit_type = "20",
      variable_role = "MEASURE",
      data_source = NULL,
      temporality_type = NULL,
      contains_data_from = "2024-01-01",
      contains_data_until = "2024-12-31"
    ),
    list(
      short_name = "udokumentert",
      id = "var-3",
      data_type = "STRING",
      name = NULL,
      definition_uri = NULL,
      classification_uri = NULL,
      is_personal_data = NULL,
      unit_type = NULL,
      variable_role = NULL,
      data_source = NULL,
      temporality_type = NULL,
      contains_data_from = "2024-01-01",
      contains_data_until = "2024-12-31"
    )
  )
}


# 1. Filstier, URL-er og enkle hjelpefunksjoner ---------------------------

test_that("datadoc_path konverterer mellom parquet og json og validerer input", {
  tmp_dir <- tempfile("datadoc-path-")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  parquet_fil <- file.path(tmp_dir, "personell_v1.parquet")
  doc_fil <- file.path(tmp_dir, "personell_v1__DOC.json")
  file.create(parquet_fil)
  file.create(doc_fil)

  # Konvertering uten advarsel når filene finnes
  expect_no_warning(
    expect_equal(datadoc_path(parquet_fil, to = "json"), doc_fil)
  )
  expect_no_warning(
    expect_equal(datadoc_path(doc_fil, to = "parquet"), parquet_fil)
  )

  # Beholder eksisterende målformat uendret
  expect_equal(datadoc_path(doc_fil, to = "json", warn_if_missing = FALSE), doc_fil)
  expect_equal(datadoc_path(parquet_fil, to = "parquet", warn_if_missing = FALSE), parquet_fil)

  # Støtter vektorer og store bokstaver i filendelse
  vektor_inn <- c("/tmp/a.PARQUET", "/tmp/b__DOC.json")
  expect_equal(
    datadoc_path(vektor_inn, to = "json", warn_if_missing = FALSE),
    c("/tmp/a__DOC.json", "/tmp/b__DOC.json")
  )

  # Advarsel ved manglende fil når warn_if_missing = TRUE
  manglende_parquet <- file.path(tmp_dir, "finnes_ikke.parquet")
  expect_warning(
    datadoc_path(manglende_parquet, to = "json", warn_if_missing = TRUE),
    "Følgende filer finnes ikke"
  )
  expect_no_warning(
    datadoc_path(manglende_parquet, to = "json", warn_if_missing = FALSE)
  )

  # Feil ved ugyldige argumenter
  expect_error(datadoc_path(NA_character_), "ikke-tomme filstier uten NA")
  expect_error(datadoc_path("   "), "ikke-tomme filstier uten NA")
  expect_error(datadoc_path("data.csv", warn_if_missing = FALSE), "må slutte på")
  expect_error(
    datadoc_path(" vanlig.json", to = "parquet", warn_if_missing = FALSE),
    "Til Parquet kreves"
  )
  expect_error(datadoc_path("a.parquet", warn_if_missing = NA), "TRUE eller FALSE")
})

test_that("vardef_url returnerer riktige endepunkter", {
  expect_equal(
    vardef_url(),
    "https://metadata.ssb.no/public/variable-definitions"
  )
  expect_equal(
    vardef_url("ekstern"),
    "https://metadata.ssb.no/public/variable-definitions"
  )
  expect_equal(
    vardef_url("intern"),
    "https://metadata.intern.ssb.no/variable-definitions"
  )
  expect_error(vardef_url("ukjent"))
})

test_that("vardef_get_klass_id henter ut KLASS-ID og varsler ved feil format", {
  expect_equal(
    vardef_get_klass_id(list(classification_uri = "https://www.ssb.no/klass/klassifikasjoner/131")),
    "131"
  )
  expect_equal(
    vardef_get_klass_id(list(classification_uri = "https://www.ssb.no/klass/klassifikasjoner/131/")),
    "131"
  )
  expect_equal(
    vardef_get_klass_id(list(classification_uri = NULL)),
    NA_character_
  )
  expect_warning(
    res <- vardef_get_klass_id(list(classification_uri = "https://www.ssb.no/klass/ikke_tall")),
    "Fant ingen KLASS-ID"
  )
  expect_identical(res, NA_character_)

  # Variant-URI skal gi advarsel og NA
  expect_warning(
    res_var <- vardef_get_klass_id(list(classification_uri = "https://www.ssb.no/klass/varianter/123")),
    "KLASS-variant"
  )
  expect_identical(res_var, NA_character_)
})


# 2. Etikettfunksjoner på datasett i minnet -------------------------------

test_that("show_column_labels og vars_without_column_labels håndterer kolonneetiketter", {
  df <- tibble::tibble(
    kjoenn = c(1, 2),
    alder = c(30, 40),
    tom = c("a", "b")
  )
  labelled::var_label(df$kjoenn) <- "Kjønn"
  labelled::var_label(df$tom) <- "   "

  expect_equal(vars_without_column_labels(df), c("alder", "tom"))

  df_vist <- show_column_labels(df)
  expect_equal(attr(df_vist$kjoenn, "pillar")$label, "Kjønn")
  expect_null(attr(df_vist$alder, "pillar"))
  expect_null(attr(df_vist$tom, "pillar"))

  # Bevarer gruppering
  df_gruppert <- dplyr::group_by(df, kjoenn)
  df_gruppert_vist <- show_column_labels(df_gruppert)
  expect_true(dplyr::is_grouped_df(df_gruppert_vist))
  expect_equal(dplyr::group_vars(df_gruppert_vist), "kjoenn")
})

test_that("vars_with_value_labels, values_without_labels, value_label_coverage og value_label_expectations fungerer", {
  df <- tibble::tibble(
    kjoenn = labelled::labelled(
      c("01", "02", "01", "99", NA),
      labels = c(Mann = "01", Kvinne = "02")
    ),
    region = c("0301", "4601", "0301", "0301", "0301"),
    alder = c(25, 30, 35, 40, 45)
  )

  # vars_with_value_labels
  expect_equal(vars_with_value_labels(df), "kjoenn")

  # values_without_labels
  uten_label <- values_without_labels(df)
  expect_s3_class(uten_label, "tbl_df")
  expect_equal(nrow(uten_label), 1L)
  expect_equal(uten_label$variable, "kjoenn")
  expect_equal(uten_label$value_without_label, "99")

  # value_label_coverage
  dekning_alle <- value_label_coverage(df, labelled_only = FALSE)
  expect_equal(dekning_alle$variable, c("kjoenn", "region", "alder"))
  expect_equal(dekning_alle$has_value_labels, c(TRUE, FALSE, FALSE))
  expect_equal(dekning_alle$n_values[1], 4L)
  expect_equal(dekning_alle$n_values_with_label[1], 3L)
  expect_equal(dekning_alle$pct_values_with_label[1], 75)
  expect_equal(dekning_alle$n_unique_values[1], 3L)
  expect_equal(dekning_alle$n_unique_values_with_label[1], 2L)
  expect_equal(dekning_alle$pct_unique_values_with_label[1], 66.67)

  dekning_kun_labelled <- value_label_coverage(df, labelled_only = TRUE)
  expect_equal(dekning_kun_labelled$variable, "kjoenn")
  expect_error(value_label_coverage(df, labelled_only = NA), "TRUE eller FALSE")

  # value_label_expectations
  forventet <- value_label_expectations(df, variabler = "alder")
  expect_equal(unname(forventet$should_have_value_labels), c(TRUE, TRUE, TRUE))
  expect_equal(unname(forventet$has_value_labels), c(TRUE, FALSE, FALSE))
  expect_equal(unname(forventet$missing_expected_value_labels), c(FALSE, TRUE, TRUE))

  kun_mangler <- value_label_expectations(df, missing_only = TRUE)
  expect_equal(kun_mangler$variable, "region")

  # Med Datadoc-filsti
  tmp_doc <- tempfile(fileext = "__DOC.json")
  on.exit(unlink(tmp_doc), add = TRUE)
  lag_test_datadoc(tmp_doc, variables = lag_test_variabler())

  med_doc <- value_label_expectations(df, filsti = tmp_doc)
  expect_true(all(c("vardef_id", "klass_id") %in% names(med_doc)))
  expect_equal(med_doc$vardef_id[med_doc$variable == "kjoenn"], "vardef_kjoenn")
  expect_equal(med_doc$klass_id[med_doc$variable == "kjoenn"], "2")
})

test_that("show_value_labels, add_labelled_columns og remove_all_labels håndterer verdietiketter", {
  df <- tibble::tibble(
    kjoenn = labelled::labelled(
      c("01", "02", "03"),
      labels = c(Mann = "01", Kvinne = "02")
    ),
    alder = c(20L, 30L, 40L)
  )
  labelled::var_label(df$kjoenn) <- "Kjønn"
  df <- show_column_labels(df)

  # show_value_labels = FALSE returnerer uendret
  expect_identical(show_value_labels(df, labels = FALSE), df)

  # show_value_labels = TRUE gir advarsel for kode "03" og beholder koden i faktor
  expect_warning(
    vist <- show_value_labels(df, labels = TRUE),
    "observerte koder uten verdietikett: 03"
  )
  expect_true(is.factor(vist$kjoenn))
  expect_equal(as.character(vist$kjoenn), c("Mann", "Kvinne", "03"))
  expect_equal(labelled::var_label(vist$kjoenn), "Kjønn")

  # add_labelled_columns
  expect_warning(
    med_kol <- add_labelled_columns(df, variabler = "kjoenn", postfix = "_tekst"),
    "observerte koder uten verdietikett: 03"
  )
  expect_true("kjoenn_tekst" %in% names(med_kol))
  expect_true(haven::is.labelled(med_kol$kjoenn))
  expect_true(is.factor(med_kol$kjoenn_tekst))
  expect_equal(as.character(med_kol$kjoenn_tekst), c("Mann", "Kvinne", "03"))

  # Feilhåndtering i add_labelled_columns
  expect_error(add_labelled_columns(df, variabler = "finnes_ikke"), "finnes ikke")
  expect_error(add_labelled_columns(df, variabler = "alder"), "ikke labelled")
  expect_error(add_labelled_columns(med_kol, variabler = "kjoenn", postfix = "_tekst"), "finnes allerede")

  # remove_all_labels fjerner både verdietiketter, kolonneetiketter og pillar-label
  uten_labels <- remove_all_labels(df)
  expect_false(haven::is.labelled(uten_labels$kjoenn))
  expect_null(labelled::var_label(uten_labels$kjoenn))
  expect_null(attr(uten_labels$kjoenn, "pillar"))
})

test_that("add_value_labels_from_file og add_column_labels_from_file legger til og renser etiketter", {
  df <- tibble::tibble(
    status = c(1L, 2L, 1L),
    region = c("01", "02", "01")
  )

  kodelister <- tibble::tibble(
    varnavn = c("status", "status", "region", "region"),
    kode = c("1", "2", "01", "02"),
    tekst = c("Intervju", "<rt>Frafall</rt> ,  med tekst", "Øst <.>", "^BedrNavn Vest"),
    sporsmaal_text = c(
      "Status<br><gt>på IO</gt>",
      "Status<br><gt>på IO</gt>",
      "Regionkode ...",
      "Regionkode ..."
    )
  )

  # Verdietiketter fra tabell
  df_val <- add_value_labels_from_file(df, kodelister)
  expect_true(is.integer(df_val$status))
  expect_equal(
    labelled::val_labels(df_val$status),
    c("Intervju" = 1L, "Frafall, med tekst" = 2L)
  )
  expect_equal(
    labelled::val_labels(df_val$region),
    c("Øst <.>" = "01", "^BedrNavn Vest" = "02")
  )

  # Kolonneetiketter fra Parquet-fil
  tmp_parquet <- tempfile(fileext = ".parquet")
  on.exit(unlink(tmp_parquet), add = TRUE)
  arrow::write_parquet(kodelister, tmp_parquet)

  df_col <- add_column_labels_from_file(df_val, tmp_parquet, clean_labels = TRUE)
  expect_equal(labelled::var_label(df_col$status), "Status på IO")
  expect_equal(labelled::var_label(df_col$region), "Regionkode ...")
  expect_equal(attr(df_col$status, "pillar")$label, "Status på IO")

  # Feil ved motstridende etiketter eller ikke-numerisk kode til heltallsvariabel
  motstridende <- tibble::tibble(
    varnavn = c("status", "status"),
    kode = c("1", "1"),
    tekst = c("A", "B")
  )
  expect_error(add_value_labels_from_file(df, motstridende), "Flere betydninger for kode")

  ugyldig_kode <- tibble::tibble(
    varnavn = "status",
    kode = "abc",
    tekst = "Ugyldig"
  )
  expect_error(add_value_labels_from_file(df, ugyldig_kode), "kan ikke tilpasses datatypen")
})


# 3. Lesing og kontroll av lokale Datadoc JSON-filer ----------------------

test_that("datadoc_variabeloversikt (uten vardef), datadoc_variable_metadata og add_column_labels fungerer lokalt", {
  tmp_dir <- tempfile("datadoc-lokal-")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  doc_fil <- file.path(tmp_dir, "personell_p2024_v1__DOC.json")
  lag_test_datadoc(doc_fil, variables = lag_test_variabler())

  # datadoc_variabeloversikt uten oppslag i Vardef
  oversikt_nb <- datadoc_variabeloversikt(doc_fil, language = "nb", include_vardef = FALSE)
  expect_equal(oversikt_nb$short_name, c("kjoenn", "alder", "udokumentert"))
  expect_equal(oversikt_nb$name, c("Kjønn", "Alder i år", NA_character_))
  expect_equal(oversikt_nb$classification_uri, c("2", NA_character_, NA_character_))

  oversikt_en <- datadoc_variabeloversikt(doc_fil, language = "en", include_vardef = FALSE)
  # kjoenn har engelsk tekst, alder faller tilbake til bokmål
  expect_equal(oversikt_en$name, c("Sex", "Alder i år", NA_character_))

  # datadoc_variable_metadata
  meta_kjoenn <- datadoc_variable_metadata(doc_fil, "kjoenn")
  expect_equal(meta_kjoenn$short_name, "kjoenn")
  expect_equal(meta_kjoenn$unit_type, "20")
  expect_error(datadoc_variable_metadata(doc_fil, "finnes_ikke"), "Fant ikke variabelen")

  # add_column_labels
  df <- tibble::tibble(kjoenn = c("1", "2"), alder = c(20L, 30L), udokumentert = c("x", "y"))
  df_labelled <- add_column_labels(df, doc_fil, language = "en")
  expect_equal(labelled::var_label(df_labelled$kjoenn), "Sex")
  expect_equal(labelled::var_label(df_labelled$alder), "Alder i år")
  expect_null(labelled::var_label(df_labelled$udokumentert))
})

test_that("summarise_missing_required_metadata og compare_variable_metadata oppdager mangler og forskjeller", {
  tmp_dir <- tempfile("datadoc-kontroll-")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  fil_1 <- file.path(tmp_dir, "data_2024__DOC.json")
  fil_2 <- file.path(tmp_dir, "data_2025__DOC.json")

  vars_1 <- lag_test_variabler()
  vars_2 <- lag_test_variabler()
  vars_2[[1]]$classification_uri <- "https://www.ssb.no/klass/klassifikasjoner/99"
  vars_2[[2]]$contains_data_until <- "2025-12-31"

  lag_test_datadoc(fil_1, variables = vars_1)
  lag_test_datadoc(fil_2, variables = vars_2)

  # summarise_missing_required_metadata
  mangler_kun_ufullstendige <- summarise_missing_required_metadata(fil_1, only_incomplete = TRUE)
  expect_equal(mangler_kun_ufullstendige$variable, c("alder", "udokumentert"))
  expect_equal(mangler_kun_ufullstendige$n_missing, c(2L, 6L))
  expect_equal(mangler_kun_ufullstendige$missing_required_fields[1], "data_source, temporality_type")

  mangler_alle <- summarise_missing_required_metadata(fil_1, only_incomplete = FALSE)
  expect_equal(nrow(mangler_alle), 3L)
  expect_true(mangler_alle$complete[mangler_alle$variable == "kjoenn"])

  # compare_variable_metadata
  diff_kun_ulike <- compare_variable_metadata(fil_1, fil_2, only_differences = TRUE)
  expect_equal(diff_kun_ulike$variable, c("kjoenn", "alder"))
  expect_true(diff_kun_ulike$ulik_klass_id[diff_kun_ulike$variable == "kjoenn"])
  expect_true(diff_kun_ulike$ulik_tidsperiode[diff_kun_ulike$variable == "alder"])

  diff_alle <- compare_variable_metadata(fil_1, fil_2, only_differences = FALSE)
  expect_equal(nrow(diff_alle), 3L)
  expect_false(diff_alle$ulik_metadata[diff_alle$variable == "udokumentert"])
})


# 4. Oppdatering og kopiering mellom Datadoc-filer ------------------------

test_that("replace_file_path og set_version_description oppdaterer datasettmetadata på disk", {
  tmp_fil <- tempfile(fileext = "__DOC.json")
  on.exit(unlink(tmp_fil), add = TRUE)
  lag_test_datadoc(tmp_fil, variables = lag_test_variabler())

  # replace_file_path
  replace_file_path(tmp_fil, "/buckets/produkt/ny_sti.parquet")
  lest <- jsonlite::read_json(tmp_fil, simplifyVector = FALSE)
  expect_equal(lest$datadoc$dataset$file_path, "/buckets/produkt/ny_sti.parquet")

  # set_version_description: legger til bokmål og engelsk
  set_version_description(tmp_fil, "Første versjon", language = "nb")
  set_version_description(tmp_fil, "First version", language = "en")

  # Gir advarsel når overwrite = FALSE og språket allerede finnes
  expect_warning(
    set_version_description(tmp_fil, "Endret versjon", language = "nb", overwrite = FALSE),
    "allerede utfylt"
  )

  # Overskriver når overwrite = TRUE uten å slette engelsk
  set_version_description(tmp_fil, "Revidert versjon", language = "nb", overwrite = TRUE)
  lest_etter <- jsonlite::read_json(tmp_fil, simplifyVector = FALSE)
  vd <- lest_etter$datadoc$dataset$version_description
  sprak <- vapply(vd, function(x) x$languageCode, character(1))
  tekst <- vapply(vd, function(x) x$languageText, character(1))
  expect_equal(tekst[sprak == "nb"], "Revidert versjon")
  expect_equal(tekst[sprak == "en"], "First version")
})

test_that("copy_metadata_variable, copy_metadata_dataset og copy_metadata kopierer metadata korrekt", {
  tmp_dir <- tempfile("datadoc-kopi-")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  fil_orig <- file.path(tmp_dir, "data_p2024_v1__DOC.json")
  fil_egen <- file.path(tmp_dir, "data_p2025_v2__DOC.json")

  vars_orig <- lag_test_variabler()
  vars_egen <- list(
    list(
      short_name = "kjoenn_ny",
      id = "egen-id-1",
      data_type = "INTEGER",
      name = NULL
    ),
    list(
      short_name = "alder",
      id = "egen-id-2",
      data_type = "INTEGER",
      name = list(list(languageCode = "nb", languageText = "Eksisterende alder"))
    ),
    list(
      short_name = "udokumentert",
      id = "egen-id-3",
      data_type = "STRING",
      name = NULL,
      contains_data_from = "2020-01-01",
      contains_data_until = "2020-12-31"
    )
  )

  lag_test_datadoc(fil_orig, variables = vars_orig)
  lag_test_datadoc(
    fil_egen,
    dataset = list(short_name = "egen_data", id = "egen-dataset-id", description = NULL),
    variables = vars_egen
  )

  # 1. copy_metadata_variable med navngitt kobling og overwrite = FALSE
  suppressMessages(
    copy_metadata_variable(
      filsti_egen = fil_egen,
      filsti_original = fil_orig,
      variabler = c(kjoenn_ny = "kjoenn", "alder"),
      overwrite = FALSE
    )
  )
  meta_kjoenn_ny <- datadoc_variable_metadata(fil_egen, "kjoenn_ny")
  meta_alder <- datadoc_variable_metadata(fil_egen, "alder")

  # Dokumentasjonsfelt er kopiert, men tekniske felt (short_name, id, data_type) er beholdt
  expect_equal(meta_kjoenn_ny$short_name, "kjoenn_ny")
  expect_equal(meta_kjoenn_ny$id, "egen-id-1")
  expect_equal(meta_kjoenn_ny$data_type, "INTEGER")
  expect_equal(meta_kjoenn_ny$unit_type, "20")
  # alder hadde allerede name og ble hoppet over pga. overwrite = FALSE
  expect_equal(meta_alder$name[[1]]$languageText, "Eksisterende alder")

  # 2. copy_metadata_dataset utleder versjon og periode fra filnavn (_p2025_v2)
  copy_metadata_dataset(fil_egen, fil_orig, overwrite = FALSE)
  lest_ds <- jsonlite::read_json(fil_egen, simplifyVector = FALSE)$datadoc$dataset
  expect_equal(lest_ds$short_name, "egen_data")
  expect_equal(lest_ds$id, "egen-dataset-id")
  expect_equal(lest_ds$version, "2")
  expect_equal(lest_ds$contains_data_from, "2025-01-01")
  expect_equal(lest_ds$contains_data_until, "2025-12-31")
  expect_equal(lest_ds$description[[1]]$languageText, "Beskrivelse av testdatasett")

  # 3. copy_metadata med ekskluder_variabler og dato_unntak
  dato_unntak <- data.frame(
    short_name = "alder",
    contains_data_from = "2023-01-01",
    contains_data_until = "2025-12-31"
  )
  suppressMessages(
    copy_metadata(
      filsti_egen = fil_egen,
      filsti_original = fil_orig,
      variabler = c(kjoenn_ny = "kjoenn"),
      ekskluder_variabler = "udokumentert",
      overwrite = TRUE,
      dato_unntak = dato_unntak
    )
  )

  meta_kjoenn_etter <- datadoc_variable_metadata(fil_egen, "kjoenn_ny")
  meta_alder_etter <- datadoc_variable_metadata(fil_egen, "alder")
  meta_udok_etter <- datadoc_variable_metadata(fil_egen, "udokumentert")

  expect_equal(meta_kjoenn_etter$contains_data_from, "2025-01-01")
  expect_equal(meta_alder_etter$name[[1]]$languageText, "Alder i år")
  expect_equal(meta_alder_etter$contains_data_from, "2023-01-01")
  expect_equal(meta_udok_etter$contains_data_from, "2020-01-01")

  # Feil ved overlapp mellom ekskluder_variabler og dato_unntak
  expect_error(
    copy_metadata(
      filsti_egen = fil_egen,
      filsti_original = fil_orig,
      ekskluder_variabler = "alder",
      dato_unntak = dato_unntak
    ),
    "både i `ekskluder_variabler` og `dato_unntak`"
  )
})

test_that("set_variable_names_from_labels oppdaterer variabelnavn og intervjumetadata", {
  tmp_fil <- tempfile(fileext = "__DOC.json")
  on.exit(unlink(tmp_fil), add = TRUE)
  lag_test_datadoc(tmp_fil, variables = lag_test_variabler())

  metadata_tabell <- data.frame(
    variabel = c("udokumentert", "alder"),
    variable_text = c("Spørsmål <br> om helse", "Ny alderstekst")
  )

  set_variable_names_from_labels(
    filsti = tmp_fil,
    metadata = metadata_tabell,
    overwrite = FALSE,
    population_description = "Alle personer 16-79 år"
  )

  meta_udok <- datadoc_variable_metadata(tmp_fil, "udokumentert")
  meta_alder <- datadoc_variable_metadata(tmp_fil, "alder")

  expect_equal(meta_udok$name[[1]]$languageText, "Spørsmål om helse")
  expect_equal(meta_udok$unit_type, "20")
  expect_equal(meta_udok$temporality_type, "STATUS")
  expect_equal(meta_udok$data_source, "67")
  expect_equal(meta_udok$variable_role, "MEASURE")
  expect_equal(meta_udok$population_description[[1]]$languageText, "Alle personer 16-79 år")

  # alder hadde allerede navn og ble ikke overskrevet når overwrite = FALSE,
  # men tomme fagfelt (data_source, temporality_type) ble fylt ut
  expect_equal(meta_alder$name[[1]]$languageText, "Alder i år")
  expect_equal(meta_alder$data_source, "67")
  expect_equal(meta_alder$temporality_type, "STATUS")
})


# 5. Funksjoner med eksterne oppslag (mocked) -----------------------------

test_that("get_all_variable_definitions, get_variable_definition_by_shortname og get_variable_definition_by_id fungerer med mock", {
  mock_vardef_df <- data.frame(
    id = c("id_1", "id_2"),
    short_name = c("kjoenn", "utgaatt_var"),
    `name.nb` = c("Kjønn", "Utgått variabel"),
    classification_uri = c("https://www.ssb.no/klass/klassifikasjoner/2", ""),
    valid_from = c("2020-01-01", "2010-01-01"),
    valid_until = c("", "2019-12-31"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    .vardef_get = function(path = NULL, query = list(), status = "auto", ...) {
      if (!is.null(path)) {
        if (path == "id_1") return(list(id = "id_1", short_name = "kjoenn"))
        return(NULL)
      }
      if (!is.null(query$short_name)) {
        if (status == "ekstern" && query$short_name == "intern_var") return(NULL)
        if (status == "intern" && query$short_name == "intern_var") {
          return(list(id = "id_intern", short_name = "intern_var"))
        }
        if (query$short_name == "kjoenn") {
          return(list(id = "id_1", short_name = "kjoenn"))
        }
        return(NULL)
      }
      mock_vardef_df
    }
  )

  # Filtrering på dato i get_all_variable_definitions
  alle <- get_all_variable_definitions()
  expect_equal(nrow(alle), 2L)

  gyldige_2025 <- get_all_variable_definitions(date = "2025-01-01")
  expect_equal(gyldige_2025$short_name, "kjoenn")

  # Oppslag på kortnavn inkl. fallback fra ekstern til intern ved status = "auto"
  expect_equal(get_variable_definition_by_shortname("kjoenn")$id, "id_1")
  expect_equal(get_variable_definition_by_shortname("intern_var", status = "auto")$id, "id_intern")
  expect_null(get_variable_definition_by_shortname("finnes_ikke", quiet = TRUE))

  # Oppslag på ID
  expect_equal(get_variable_definition_by_id("id_1")$short_name, "kjoenn")
  expect_null(get_variable_definition_by_id("ukjent_id", quiet = TRUE))
})

test_that("datadoc_variabeloversikt, datadoc_variables_with_classification_uri, check_variables_in_vardef og copy_vardef_to_datadoc fungerer med mock", {
  tmp_fil <- tempfile(fileext = "__DOC.json")
  on.exit(unlink(tmp_fil), add = TRUE)
  lag_test_datadoc(tmp_fil, variables = lag_test_variabler())

  mock_vardef_tabell <- data.frame(
    id = c("vardef_kjoenn", "vardef_alder", "vardef_udok"),
    short_name = c("kjoenn", "alder", "udokumentert"),
    `name.nb` = c("Kjønn (Vardef)", "Alder (Vardef)", "Dokumentert fra Vardef"),
    classification_reference = c("2", "", "131"),
    unit_types = I(list("20", "20", c("20", "01"))),
    variable_status = c("PUBLISHED_EXTERNAL", "PUBLISHED_EXTERNAL", "PUBLISHED_INTERNAL"),
    valid_from = c("2020-01-01", "2020-01-01", "2022-01-01"),
    valid_until = c(NA_character_, NA_character_, NA_character_),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    get_all_variable_definitions = function(date = NULL, language = "nb", status = "auto", ...) {
      if (status == "ekstern") {
        return(mock_vardef_tabell[1:2, , drop = FALSE])
      }
      mock_vardef_tabell[3, , drop = FALSE]
    }
  )

  # datadoc_variabeloversikt med include_vardef = TRUE
  oversikt <- datadoc_variabeloversikt(tmp_fil, include_vardef = TRUE)
  expect_equal(oversikt$vardef_name, c("Kjønn (Vardef)", "Alder (Vardef)", NA_character_))
  expect_equal(oversikt$vardef_classification_uri, c("2", NA_character_, NA_character_))

  # datadoc_variables_with_classification_uri
  med_kodeliste <- datadoc_variables_with_classification_uri(tmp_fil, with_codelist = TRUE)
  uten_kodeliste <- datadoc_variables_with_classification_uri(tmp_fil, with_codelist = FALSE)
  expect_equal(med_kodeliste$short_name, "kjoenn")
  expect_equal(uten_kodeliste$short_name, c("alder", "udokumentert"))

  # check_variables_in_vardef
  sjekk <- check_variables_in_vardef(c("kjoenn", "udokumentert", "ukjent"))
  expect_equal(sjekk$finnes_i_vardef, c(TRUE, TRUE, FALSE))
  expect_equal(sjekk$kilde, c("ekstern", "intern", NA_character_))

  # copy_vardef_to_datadoc med overwrite = FALSE oppdaterer bare "udokumentert"
  suppressMessages(
    res_kopi <- copy_vardef_to_datadoc(
      filsti = tmp_fil,
      overwrite = FALSE,
      temporality_type = "STATUS"
    )
  )
  expect_equal(res_kopi$resultat, c("beholdt", "beholdt", "oppdatert"))

  meta_udok <- datadoc_variable_metadata(tmp_fil, "udokumentert")
  expect_equal(meta_udok$name[[1]]$languageText, "Dokumentert fra Vardef")
  expect_equal(meta_udok$definition_uri, "urn:ssb:variable-definition:vardef:vardef_udok")
  expect_equal(meta_udok$unit_type, "20")
  expect_equal(meta_udok$data_source, "75")
  expect_equal(meta_udok$temporality_type, "STATUS")
})

test_that("add_value_labels henter kodeliste via klassR med mock", {
  tmp_fil <- tempfile(fileext = "__DOC.json")
  on.exit(unlink(tmp_fil), add = TRUE)
  lag_test_datadoc(tmp_fil, variables = lag_test_variabler())

  local_mocked_bindings(
    datadoc_variables_with_classification_uri = function(filsti, language = "nb") {
      data.frame(
        short_name = "kjoenn",
        classification_uri = "2",
        vardef_classification_uri = NA_character_,
        contains_data_until = "2024-12-31",
        stringsAsFactors = FALSE
      )
    }
  )

  local_mocked_bindings(
    get_klass = function(classification, date = NULL, language = "nb", ...) {
      data.frame(
        code = c("1", "2"),
        name = c("Mann", "<b>Kvinne</b>"),
        stringsAsFactors = FALSE
      )
    },
    .package = "klassR"
  )

  df <- tibble::tibble(kjoenn = c("1", "2", "1"), alder = c(20L, 30L, 40L))
  res <- add_value_labels(df, tmp_fil, clean_labels = TRUE)
  expect_equal(labelled::val_labels(res$kjoenn), c(Mann = "1", Kvinne = "2"))
})

test_that("create_empty_datadoc validerer argumenter og filstier før kjøring", {
  expect_error(create_empty_datadoc("fil. parquet", quiet = "nei"), "TRUE eller FALSE")
  expect_error(create_empty_datadoc("ikke_parquet.csv"), "parquet-fil")
  expect_error(
    create_empty_datadoc("/ feil/sti/data.parquet"),
    "Kunne ikke finne statistikk-kortnavn"
  )

  # Returnerer eksisterende sti med melding dersom __DOC.json allerede finnes
  tmp_dir <- tempfile("empty-datadoc-")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  parquet_fil <- file.path(tmp_dir, "eksisterende.parquet")
  doc_fil <- file.path(tmp_dir, "eksisterende__DOC.json")
  file.create(doc_fil)

  expect_message(
    res <- create_empty_datadoc(parquet_fil),
    "Datadoc-filen finnes allerede"
  )
  expect_equal(res, doc_fil)
})
