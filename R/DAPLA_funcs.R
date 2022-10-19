#' Funksjon for å koble til GCS bucket
#'
#' `gcs_bucket` er en hjelpefunksjon som kobler til en GCS bucket ved hjelp av access_token og expiration som er lagret som miljøvariabler i Jupyter på DAPLA.
#'
#' @param bucket Full sti til GCS bucket.
#'
#' @examples
#' \dontrun{
#' bucket <- gcs_bucket("ssb-prod-dapla-felles-data-delt")
#' bucket
#'}
#'@encoding UTF-8

gcs_bucket <- function(bucket) {
  if (Sys.getenv('CLUSTER_ID') %in% c("staging-bip-ap", "prod-bip-app")){
    response <- httr::GET(Sys.getenv('LOCAL_USER_PATH'), httr::add_headers('Authorization' = paste0('token ', Sys.getenv("JUPYTERHUB_API_TOKEN"))))
    access_token <- httr::content(response)$exchanged_tokens$google$access_token
    expiration <- httr::content(response)$exchanged_tokens$google$exp
  } else {
    access_token <- getPass::getPass("Skriv inn access_token")
    expiration <- as.numeric(getPass::getPass("Skriv inn expiration"))
  }
  bucket <- arrow::gs_bucket(bucket, access_token  = access_token, expiration = expiration)
}


#' Funksjon for å laste inn .parquet-fil fra GCS bucket
#' #'
#' Funksjonen `read_parquet` kan brukes til å lese inn .parquet-filer fra GCS.
#'
#' @param bucket Full sti til GCS bucket.
#' @param file Navn på filen som skal leses inn.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_parquet.html)
#'
#' @examples
#' \dontrun{
#' data <- read_parquet(bucket = "ssb-prod-dapla-felles-data-delt/R_smoke_test", file = "1987")
#'
#' data <- read_parquet(bucket = "ssb-prod-dapla-felles-data-delt/R_smoke_test", file = "1987", col_select = c("Year", "Month"))
#'}
#'@encoding UTF-8

read_parquet <- function(file, ...) {

  # Jupyterlab (DAPLA)
  if (Sys.getenv('CLUSTER_ID') %in% c("staging-bip-app", "prod-bip-app")) {
    # file <- gsub(".parquet", "", file)
    # df <- arrow::read_parquet(gcs_bucket(bucket)$path(paste0(file, ".parquet")), ...)
    df <- arrow::read_parquet(gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)
  }

  # Jupyterlab (produksjonssonen)
  if (grepl("sl-stata-p3", Sys.info()["nodename"]) | grepl("sl-python-03", Sys.info()["nodename"]) |
      (grepl("FW-XAPROD", Sys.info()["nodename"]) & grepl("[A-Za-z]:", file))){
    df <- arrow::read_parquet(file, ...)
  }

  # Windows (produksjonssonen) - fra Linux
  if (grepl("FW-XAPROD", Sys.info()["nodename"]) & grepl("/ssb/", file)){ # FW-XAPROD = RStudio (Windows)
    # Brukernavn og passord (Windows) #
    options(usr = Sys.info()[["user"]])
    options(passwd = rstudioapi::askForPassword("Windows passord:"))

    df <- arrow::read_parquet(
      RCurl::getBinaryURL(
        url = paste0("sftp://sl-sas-work-1", file),
        userpwd = paste0(getOption("usr"), ":",  getOption("passwd"))
      )
    )
  }

  return(df)
}


#' Funksjon for å laste inn .feather-fil fra GCS bucket
#' #'
#' Funksjonen `read_feather` kan brukes til å lese inn .feather-filer fra GCS.
#'
#' @param bucket Full sti til GCS bucket.
#' @param file Navn på filen som skal leses inn.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_feather.html)
#'
#' @examples
#' \dontrun{
#'}
#'@encoding UTF-8

read_feather <- function(file, ...) {
  if (Sys.getenv('CLUSTER_ID') %in% c("staging-bip-app", "prod-bip-app")) {
    df <- arrow::read_feather(gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)

  }

  # Jupyterlab (produksjonssonen)
  if (grepl("sl-stata-p3", Sys.info()["nodename"]) | grepl("sl-python-03", Sys.info()["nodename"]) |
      (grepl("FW-XAPROD", Sys.info()["nodename"]) & grepl("[A-Za-z]:", file))){
    df <- arrow::read_feather(file, ...)
  }

  if (grepl("FW-XAPROD", Sys.info()["nodename"]) & grepl("/ssb/", file)){ # FW-XAPROD = RStudio (Windows)

    # Brukernavn og passord (Windows) #
    options(usr = Sys.info()[["user"]])
    options(passwd = rstudioapi::askForPassword("Windows passord:"))

    df <- arrow::read_feather(
      RCurl::getBinaryURL(
        url = paste0("sftp://sl-sas-work-1", file),
        userpwd = paste0(getOption("usr"), ":",  getOption("passwd"))
      ))
  }

  return(df)


}



#' Funksjon for å laste inn "multifile" datasett fra GCS bucket
#'
#' Funksjonen `open_dataset` kan brukes til å lese deler av datasett (bl.a. .parquet-, .feather- og .csv-filer) fra GCS. Det lages en forbindelse til mappen der filen ligger og deretter kan man bruke argumenter fra `dplyr`, som `filter` og `select`, før man bruker `collect` til å lese inn dataene i R. `open_dataset` kan også brukes til å lese inn sf-objekter (lagret som .parquet-fil med pakken `sfarrow`).
#'
#' @param bucket Full sti til GCS bucket.
#' @param file Navn på filen som skal leses inn.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_parquet.html)
#'
#' @examples
#' \dontrun{
# data <- open_dataset(bucket = "ssb-prod-spesh-personell-data-kilde/2020") %>%
#  dplyr::filter(FRTK_ID_SSB == "964977402") %>%
#  dplyr::select(ARB_ARBMARK_STATUS, PERS_SUM_ARBEIDSTID, FRTK_ID_SSB, FRTK_ID_SSB) %>%
#  dplyr::collect()
#
#  data <- open_dataset(bucket = "ssb-prod-spesh-personell-data-kilde/Vegnett") %>%
#  dplyr::filter(municipality == "301") %>%
#  sfarrow::read_sf_dataset()
#'}
#'@encoding UTF-8

open_dataset <- function(bucket, ...) {

  # Jupyterlab (DAPLA)
  if (Sys.getenv('CLUSTER_ID') %in% c("staging-bip-app", "prod-bip-app")) {
  ds <- arrow::open_dataset(gcs_bucket(bucket), ...)
  }

  # Jupyterlab (produksjonssonen)
  if (grepl("sl-stata-p3", Sys.info()["nodename"]) | grepl("sl-python-03", Sys.info()["nodename"]) |
      (grepl("FW-XAPROD", Sys.info()["nodename"]) & grepl("[A-Za-z]:", file))){
    # OBS: mangler
  }

  # Windows (produksjonssonen) - fra Linux
  if (grepl("FW-XAPROD", Sys.info()["nodename"]) & grepl("/ssb/", file)){ # FW-XAPROD = RStudio (Windows)
    # OBS: mangler
  }

  return(df)
}

#' Funksjon for å laste inn .JSON-fil fra GCS bucket
#'
#' Funksjonen `read_json` kan brukes til å lese inn JSON-filer GCS.
#'
#' @param bucket Full sti til GCS bucket.
#' @param file Navn på filen som skal leses inn.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_json_arrow.html)
#'
#' @examples
#' \dontrun{
#' data <- read_json(bucket = "ssb-prod-spesh-personell-data-kilde", file = "example_1.json")
#'}
#'@encoding UTF-8

read_json <- function(file, ...) {
  if (Sys.getenv('CLUSTER_ID') %in% c("staging-bip-app", "prod-bip-app")) {
    # file <- gsub(".json", "", basename(file))
    # df <- arrow::read_json_arrow(gcs_bucket(dirname(file))$path(paste0(basename(file), ".json")), ...)
    df <- arrow::read_json_arrow(gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)

  }

  # Jupyterlab (produksjonssonen)
  if (grepl("sl-stata-p3", Sys.info()["nodename"]) | grepl("sl-python-03", Sys.info()["nodename"]) |
      (grepl("FW-XAPROD", Sys.info()["nodename"]) & grepl("[A-Za-z]:", file))){
    # OBS: mangler
  }

  # Windows (produksjonssonen) - fra Linux
  if (grepl("FW-XAPROD", Sys.info()["nodename"]) & grepl("/ssb/", file)){ # FW-XAPROD = RStudio (Windows)
    # OBS: mangler
  }

}


#' Funksjon for å laste inn .csv-fil fra GCS bucket
#'
#' Funksjonen `read_csv` kan brukes til å lese inn JSON-filer GCS.
#'
#' @param bucket Full sti til GCS bucket.
#' @param file Navn på filen som skal leses inn.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_delim_arrow.html)
#'
#' @examples
#' \dontrun{
#' data <- read_csv(bucket = "ssb-prod-spesh-personell-data-kilde", file = "example_1.csv")
#'}
#'@encoding UTF-8

read_csv <- function(file, ...) {
  if (Sys.getenv('CLUSTER_ID') %in% c("staging-bip-app", "prod-bip-app")) {
    df <- arrow::read_delim_arrow(gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)
  }

  # Jupyterlab (produksjonssonen)
  if (grepl("sl-stata-p3", Sys.info()["nodename"]) | grepl("sl-python-03", Sys.info()["nodename"]) |
      (grepl("FW-XAPROD", Sys.info()["nodename"]) & grepl("[A-Za-z]:", file))){
    df <- readr::read_delim(file, ...)
  }

  if (grepl("FW-XAPROD", Sys.info()["nodename"]) & grepl("/ssb/", file)){ # FW-XAPROD = RStudio (Windows)
    # OBS: test med ulike typer separator!
    options(usr = Sys.info()[["user"]])
    options(passwd = rstudioapi::askForPassword("Windows passord:"))

    df <- readr::read_delim(
      RCurl::getBinaryURL(
        url = paste0("sftp://sl-sas-work-1", file),
        userpwd = paste0(getOption("usr"), ":",  getOption("passwd"))
      ),
    )
  }

  return(df)
}

#' Funksjon for å lagre .parquet-fil til GCS bucket
#'
#' Funksjonen `write_parquet` kan brukes til å skrive parquet-filer til GCS bucket.
#'
#' @param bucket Full sti til GCS bucket.
#' @param file Navn på filen som skal skrives.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_delim_arrow.html)
#'
#' @examples
#' \dontrun{
# write_parquet(data, bucket = "ssb-prod-dapla-felles-data-delt/R_smoke_test", file = "1987_1996.parquet")
#' }
#'@encoding UTF-8

write_parquet <- function(data, bucket, file, ...) {
  arrow::write_parquet(data, gcs_bucket(bucket)$path(file), ...)
}

#' Funksjon for å lagre .feather-fil til GCS bucket
#'
#' Funksjonen `write_parquet` kan brukes til å skrive .feather-filer til GCS bucket.
#'
#' @param bucket Full sti til GCS bucket.
#' @param file Navn på filen som skal skrives.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/write_feather.html)
#'
#' @examples
#' \dontrun{
#' }
#'@encoding UTF-8

write_feather <- function(data, bucket, file, ...) {
  # legg til .feather endelse dersom dette ikke finnes?
  arrow::write_feather(data, gcs_bucket(bucket)$path(file), ...)
}



#' Funksjon for å lagre .csv-fil til GCS bucket
#'
#' Funksjonen `write_csv` kan brukes til å skrive parquet-filer til GCS bucket.
#'
#' @param bucket Full sti til GCS bucket.
#' @param file Navn på filen som skal skrives.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_delim_arrow.html)
#'
#' @examples
#' \dontrun{
#' write_csv(data, bucket = "ssb-prod-dapla-felles-data-delt/R_smoke_test", file = "1987_1996.parquet")
#' }
#'@encoding UTF-8

write_csv <- function(data, bucket, file, ...) {
  # legg til .csv endelse dersom dette ikke finnes?
  arrow::write_csv_arrow(data, gcs_bucket(bucket)$path(file), ...)
}



#' Funksjon for å sjekke hvilke filer som finnes i en GCS bucket
#'
#' Funksjonen `list.files` kan brukes til å sjekke hvilke filer som finnes i en GCS bucket
#'
#' @param bucket Full sti til GCS bucket.
#'
#' @examples
#' \dontrun{
#' list.files("ssb-prod-dapla-felles-data-delt/R_smoke_test")
#' }
#'@encoding UTF-8
#'

list.files <- function(bucket) {
  gcs_bucket(bucket)$ls(recursive = T)
}





#' Funksjon for å laste inn filer fra GCS bucket
#' #'
#' Funksjonen `read_SSB` kan brukes til å lese inn filer fra GCS.
#'
#' @param bucket Full sti til GCS bucket.
#' @param file Navn på filen som skal leses inn.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_parquet.html)
#'
#' @examples
#' \dontrun{
#' read_SSB_parquet <- read_SSB("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.parquet")
#'
#' read_SSB_MFD <- read_SSB("ssb-prod-spesh-personell-data-kilde/enda_en_ny_mappe")
#'
#' read_SSB_feather <- read_SSB("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.feather")
#'
#' read_SSB_csv <- read_SSB("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.csv")
#'
#' read_SSB_json <- read_SSB("ssb-prod-spesh-personell-data-kilde/example_1.json")
#'}
#'@encoding UTF-8

read_SSB <- function(file, ...) {

  if(grepl(".parquet", basename(file))){
    df <- read_parquet(file, ...)
  } else if(grepl(".feather", basename(file))){
    df <- read_feather(file, ...)
  } else if(grepl(".csv", basename(file)) | grepl(".txt", basename(file)) | grepl(".dat", basename(file))){
    df <- read_csv(file, ...)
  } else if(grepl(".json", basename(file))){
    df <- read_json(file, ...)
  } else {
   df <- open_dataset(file, ...) %>%
       dplyr::collect()
    }
  return(df)

  }


#' Funksjon for å skrive filer til GCS bucket
#' #'
#' Funksjonen `write_SSB` kan brukes til å skrive filer til GCS.
#'
#' @param bucket Full sti til GCS bucket.
#' @param file Navn på filen som skal leses inn.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_parquet.html)
#'
#' @examples
#' \dontrun{
#'}
#'@encoding UTF-8

# OBS: denne er ikke påbegynt
write_SSB <- function(file, ...) {
  if(grepl(".parquet", basename(file))){
    write_parquet(data, file, ...)
  } else if(grepl(".feather", basename(file))){
    write_feather(data, file, ...)
  } else if(grepl(".csv", basename(file)) | grepl(".txt", basename(file)) | grepl(".dat", basename(file))){
    write_csv(data, file, ...)
  } else if(grepl(".json", basename(file))){
    write_json(data, file, ...)
  } else {
    # df <- open_dataset(file, ...) %>%
    #   dplyr::collect()
  }

}
