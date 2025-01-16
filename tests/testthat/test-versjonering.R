
# Oppsett -----------------------------------------------------------------

## Lager midlertidig mappe med midlertidige testfiler ---------------------

test_dir <- paste0(tempdir(),
                   "/test-versjonering-",
                   format(Sys.time(), "%Y%m%d%H%M%S"),
                   "/")

dir.create(test_dir)

if (!dir.exists(test_dir)) stop("Kunne ikke lage testmappe.")

test_filer <- list(eldst       = paste0(test_dir, "test_fil_p2022_v1.txt"),
                   v2          = paste0(test_dir, "test_fil_p2022_v2.txt"),
                   v3          = paste0(test_dir, "test_fil_p2022_v4.txt"),
                   v4          = paste0(test_dir, "test_fil_p2022_v5.txt"),
                   v5          = paste0(test_dir, "test_fil_p2022_v6.txt"),
                   sist        = paste0(test_dir, "test_fil_p2022_v19.txt"),
                   uversjonert = paste0(test_dir, "test_fil_p2022.txt"))

### Husk å redigere `test_filer_ny` dersom du endrer `test_filer`.

test_filer_ny <- paste0(test_dir, "test_fil_p2022_v20.txt")

### Lager også andre filer for å simulere en arkivmappe som inneholder filer fra
### andre perioder, samt helt andre filer

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

### Konstruerer også noen fil- og mappenavn som vi ikke kommer til å opprette,
### for å teste feilmeldinger samt oppretting av ikke-hittil-versjonerte filer.
### Vi bruker tempmappe-navnet for å være helt sikre på at disse
### filene/mappene ikke eksisterer når vi kjører testene.

ukjent_fil    <- paste0(test_dir, "ukjent_fil_p2022.txt")
ukjent_fil_ny <- paste0(test_dir, "ukjent_fil_p2022_v1.txt")

ukjent_mappe <- paste0(tempdir(),
                       "/test-versjonering-",
                       format(Sys.time(), "%Y%m%d%H%M%S_ikkelag"),
                       "/")

for (fil in c(test_filer, ekstrafiler)) {

  file.create(fil)

}

if(!all(vapply(c(test_filer, ekstrafiler), file.exists, logical(1)))) {

  stop("Kunne ikke lage testfiler.")

}

# Tester ------------------------------------------------------------------


test_that("lag_versjonert_filsti klarer å finne den siste versjonen", {

  # Finner vi den siste versjonen av en fil, uavhengig av hvilken versjon vi
  # angir som input (inkludert en fil uten versjonsstempel?)

  expect_true(all(vapply(test_filer,
                         lag_versjonert_filsti,
                         versjon = "siste",
                         character(1)) == test_filer$sist))

  # Får vi feilmelding dersom vi prøver å finne den siste versjonen til en fil
  # som ikke eksisterer?

  expect_error(lag_versjonert_filsti(ukjent_fil, versjon = "siste"))

})

test_that("lag_versjonert_filsti klarer å opprette en ny versjon", {

  # Klarer vi å lage en ny versjon, uavhengig av hvilken versjon vi angir som
  # input (inkludert en fil uten versjonsstempel?)

  expect_true(all(vapply(test_filer,
                         lag_versjonert_filsti,
                         versjon = "ny",
                         character(1)) == test_filer_ny))

  # Fungerer det å lage en versjon for en fil som ikke er versjonert enda?

  expect_equal(lag_versjonert_filsti(ukjent_fil,
                                     versjon = "ny"),
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
