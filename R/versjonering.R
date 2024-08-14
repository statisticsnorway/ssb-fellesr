
finn_versjon <- function(fil) {

  if (!grepl(".*_v\\d{1,}\\..*$", fil)) {

    stop("Filen er ikke versjonert i henhold til navnestandarden. ",
         "Se https://manual.dapla.ssb.no/statistikkere/navnestandard.html ",
         "for mer informasjon.")

  }

  ## Vi henter ut versjonsnummeret ved å erstatte hele filnavnet med
  ## tallene etter "_v"

  versjon <- as.integer(gsub(pattern = ".*_v(\\d{1,})\\..*$",
                             replacement = "\\1",
                             x = fil))

  return(versjon)

}

lag_versjonert_filsti <- function(fil,
                                  versjon = c("siste", "ny", integer(1))) {

  mappe <- dirname(fil)

  if (!dir.exists(mappe)) stop("Fant ikke angitt mappe: ", mappe)

  basenavn <- gsub(pattern = "_v\\d{1,}$",
                   replacement = "",
                   x = tools::file_path_sans_ext(basename(fil)))

  filendelse <- tools::file_ext(fil)

  filer <- list.files(mappe, basenavn)

  # Om vi skal lage en ny versjon så godtar vi at det ikke finnes fra før, i
  # tilfelle det er den første versjonen vi skal lage nå

  if (versjon != "ny" & length(filer) == 0) {

    stop("Fant ingen filer som matchet ", basenavn, " i ", mappe, ".")

  }

  versjonerte_filer <- filer[grepl("_v\\d{1,}\\..*$", filer)]

  if (length(versjonerte_filer) == 0) {

    if (versjon == "ny") {

      ny_versjon <- 1

    } else if (versjon == "siste" | is.integer(versjon)) {

      stop("Fant ingen versjonerte filer i ", mappe, ".")

    }

  } else {

    versjoner <- vapply(versjonerte_filer, finn_versjon, integer(1))

    if (versjon == "ny") {

      ny_versjon <- max(versjoner) + 1

    } else if (versjon == "siste") {

      ny_versjon <- max(versjoner)

    } else if (is.integer(versjon)) {

      if (!versjon %in% versjoner) {

        stop("Fant ikke versjon ", versjon, ".")

      } else {

        # versjon er et gyldig heltall, så vi bruker det uendret

        ny_versjon <- versjon

      }

    }

  }

  ny_filsti <- paste0(mappe, "/", basenavn, "_v", ny_versjon, ".", filendelse)

  if (versjon == "ny") {

    if (file.exists(ny_filsti)) {

      stop("Genererte et filnavn som allerede eksisterer i angitt mappe.")

    } else {

      return(ny_filsti)

    }

  } else if (versjon == "siste" | is.integer(versjon)) {

    if (!file.exists(ny_filsti)) {

      stop("Klarte ikke å finne siste versjon. ",
           "Pass på at alle filer i mappen slutter med ",
           "_v{versjonsnummer}.{filendelse}")

    } else {

      return(ny_filsti)

    }

  }

}

finn_siste_versjon <- function(fil) lag_versjonert_filsti(fil,
                                                          versjon = "siste")

lag_ny_versjon <- function(fil) lag_versjonert_filsti(fil,
                                                      versjon = "ny")
