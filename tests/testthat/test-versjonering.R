
# Oppsett -----------------------------------------------------------------

## Lager midlertidig mappe med midlertidige testfiler ---------------------

test_dir <- paste0(tempdir(),
                   "/test-versjonering-",
                   format(Sys.time(), "%Y%m%d%H%M%S"),
                   "/")

dir.create(test_dir)

if (!dir.exists(test_dir)) stop("Kunne ikke lage testmappe.")

test_filer <- paste0(test_dir, c("test_fil_p2022_v1.txt",
                                 "test_fil_p2022_v2.txt",
                                 "test_fil_p2022_v4.txt",
                                 "test_fil_p2022_v5.txt",
                                 "test_fil_p2022_v6.txt",
                                 "test_fil_p2022_v19.txt",
                                 "test_fil_p2022.txt"))

### Husk å redigere `test_filer_ny`, `test_filer_nyeste` og
### `test_filer_uversjonert` dersom du endrer `test_filer`.

test_filer_ny          <- paste0(test_dir, "test_fil_p2022_v20.txt")
test_filer_nyeste      <- test_filer[6]
test_filer_uversjonert <- test_filer[7]

### For å simulere en arkivmappe som inneholder filer fra andre perioder,
### samt helt andre filer

ekstrafiler <- paste0(test_dir, c("test_fil_p2021_v1.txt",
                                  "test_fil_p2021_v2.txt",
                                  "test_fil_p2021_v3.txt",
                                  "test_fil_p2021_v4.txt",
                                  "test_fil_p2021_v5.txt",
                                  "test_fil_p2021_v6.txt",
                                  "test_fil_p2021_v11.txt",
                                  "test_fil_p2021.txt",
                                  "annen_fil_p2021_v1.txt",
                                  "annen_fil_p2021_v2.txt",
                                  "annen_fil_p2021_v3.txt",
                                  "annen_fil_p2021_v4.txt",
                                  "annen_fil_p2021_v5.txt",
                                  "annen_fil_p2021_v6.txt",
                                  "annen_fil_p2021_v11.txt",
                                  "fil_med_v.txt",
                                  "fil_med_versjon.txt",
                                  "fil_med_v1_gammel.txt",
                                  "fil_med_v1gml.txt",
                                  "annen_fil_p2021.txt"))

ukjent_fil    <- paste0(test_dir, "ukjent_fil_p2022.txt")
ukjent_fil_ny <- paste0(test_dir, "ukjent_fil_p2022_v1.txt")

for (fil in c(test_filer, ekstrafiler)) {

  file.create(fil)

}

if(!all(file.exists(test_filer))) stop("Kunne ikke lage testfiler.")

# Tester ------------------------------------------------------------------

test_that("finn_siste_versjon fungerer", {

  expect_equal(finn_siste_versjon(test_filer[1]),
               test_filer_nyeste)

  expect_equal(finn_siste_versjon(test_filer_uversjonert),
               test_filer_nyeste)

  expect_error(finn_siste_versjon(ukjent_fil))

})

test_that("lag_ny_versjon fungerer", {

  expect_equal(lag_ny_versjon(test_filer[1]),
               test_filer_ny)

  expect_equal(lag_ny_versjon(ukjent_fil),
               ukjent_fil_ny)

})

test_that("finn_versjon fungerer", {

  expect_equal(finn_versjon("fil_v1.txt"), 1L)
  expect_equal(finn_versjon("fil_v99.txt"), 99L)
  expect_equal(finn_versjon("fil_v999.txt"), 999L)

  expect_equal(finn_versjon("/fiktiv/mappe/arkiv/fil_v1.txt"), 1L)
  expect_equal(finn_versjon("/fiktiv/mappe/arkiv/fil_v99.txt"), 99L)
  expect_equal(finn_versjon("/fiktiv/mappe/arkiv/fil_v999.txt"), 999L)

  expect_equal(finn_versjon("gs://fiktivt/team/arkiv/fil_v1.txt"), 1L)
  expect_equal(finn_versjon("gs://fiktivt/team/arkiv/fil_v99.txt"), 99L)
  expect_equal(finn_versjon("gs://fiktivt/team/arkiv/fil_v999.txt"), 999L)

  expect_error(finn_versjon("fil_uten_versjon.txt"))
  expect_error(finn_versjon("/fiktiv/mappe/arkiv/fil_uten_versjon.txt"))
  expect_error(finn_versjon("gs://fiktivt/team/arkiv/fil_uten_versjon.txt"))

})

# Rydding -----------------------------------------------------------------

## Sletter testmappe ------------------------------------------------------

unlink(test_dir, recursive = TRUE)
