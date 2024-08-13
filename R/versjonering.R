
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

  basenavn <- gsub(pattern = "_v\\d{1,}$",
                   replacement = "",
                   x = tools::file_path_sans_ext(basename(fil)))

  filendelse <- tools::file_ext(fil)

  filer <- list.files(mappe, basenavn)

  versjonerte_filer <- filer[grepl("_v\\d{1,}\\..*$", filer)]

  if (length(versjonerte_filer) == 0) {

    if (versjon == "ny") {

      versjon <- 1

    } else if (versjon == "siste" | is.integer(versjon)) {

      stop("Fant ingen versjonerte filer i ", mappe, ".")

    }

  } else {

    versjoner <- vapply(versjonerte_filer, finn_versjon, integer(1))

    if (versjon == "ny") {

      versjon <- max(versjoner) + 1

    } else if (versjon == "siste") {

      versjon <- max(versjoner)

    } else if (is.integer(versjon)) {

      if (!versjon %in% versjoner) {

        stop("Fant ikke versjon ", versjon, ".")

      }

      # Ingen else, siden `versjon` allerede er et gyldig heltall

    }

  }

  return(paste0(mappe, "/", basenavn, "_v", versjon, ".", filendelse))

}

finn_siste_versjon <- function(fil) {

  if (!file.exists(fil)) stop("Fant ikke angitt fil i mappen.")

  siste_versjon <- lag_versjonert_filsti(fil, versjon = "siste")

  if (!file.exists(siste_versjon)) {

    stop("Klarte ikke å finne siste versjon. ",
         "Pass på at alle filer i mappen slutter med ",
         "_v{versjonsnummer}.{filendelse}")

  } else {

    return(siste_versjon)

  }

}

lag_ny_versjon <- function(fil) {

  ny_versjon <- lag_versjonert_filsti(fil, versjon = "ny")

  if (file.exists(ny_versjon)) {

    stop("Genererte et filnavn som allerede eksisterer i angitt mappe.")

  } else {

    return(ny_versjon)

  }

}
