#' Funksjon for å koble til Google Cloud Storage bucket
#'
#' `gcs_bucket` er en hjelpefunksjon som kobler til en Google Cloud Storage bucket ved hjelp av access_token og expiration som er lagret som miljøvariabler i Jupyter på DAPLA.
#'
#' @param bucket Full sti til Google Cloud Storage bucket.
#'
#' @examples
#' \dontrun{
#' bucket <- gcs_bucket("ssb-prod-dapla-felles-data-delt")
#' bucket
#'}
#'@encoding UTF-8

gcs_bucket <- function(bucket) {
  if (Sys.getenv('CLUSTER_ID') %in% c("staging-bip-app", "prod-bip-app")){
    response <- httr::GET(Sys.getenv('LOCAL_USER_PATH'), httr::add_headers('Authorization' = paste0('token ', Sys.getenv("JUPYTERHUB_API_TOKEN"))))
    access_token <- httr::content(response)$exchanged_tokens$google$access_token
    expiration <- httr::content(response)$exchanged_tokens$google$exp
  } else {
    access_token <- getPass::getPass("Skriv inn access_token")
    expiration <- as.numeric(getPass::getPass("Skriv inn expiration"))
  }
  bucket <- arrow::gs_bucket(bucket, access_token  = access_token, expiration = expiration)
}


#' Funksjon for å laste inn .parquet-fil fra Google Cloud Storage bucket
#' #'
#' Funksjonen `read_parquet` kan brukes til å lese inn .parquet-filer fra Google Cloud Storage.
#'
#' @param bucket Full sti til Google Cloud Storage bucket.
#' @param file Navn på filen som skal leses inn.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_parquet.html)
#'
#' @examples
#' \dontrun{
#' # Jupyterlab (DAPLA)
#' data <- read_parquet("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.parquet")
#'
#' # Jupyterlab (produksjonssonen)
#' # OBS: mangler
#'
#' # RStudio Windows (produksjonssonen) - fra Linux
#' # OBS: mangler
#'}
#'@encoding UTF-8

read_parquet <- function(file, ...) {

  # Jupyterlab (DAPLA)
  if (Sys.getenv('CLUSTER_ID') %in% c("staging-bip-app", "prod-bip-app")) {
    df <- arrow::read_parquet(gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)
  }

  # Jupyterlab (produksjonssonen)
  if (grepl("sl-stata-p3", Sys.info()["nodename"]) | grepl("sl-python-03", Sys.info()["nodename"]) |
      (grepl("FW-XAPROD", Sys.info()["nodename"]) & grepl("[A-Za-z]:", file))){
    df <- arrow::read_parquet(file, ...)
  }

  # RStudio Windows (produksjonssonen) - fra Linux
  if (grepl("FW-XAPROD", Sys.info()["nodename"]) & grepl("/ssb/", file)){

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


#' Funksjon for å laste inn .feather-fil fra Google Cloud Storage bucket
#' #'
#' Funksjonen `read_feather` kan brukes til å lese inn .feather-filer fra Google Cloud Storage.
#'
#' @param bucket Full sti til Google Cloud Storage bucket.
#' @param file Navn på filen som skal leses inn.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_feather.html)
#'
#' @examples
#' \dontrun{
#' # Jupyterlab (DAPLA)
#' data <- read_feather("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.feather")
#'
#' # Jupyterlab (produksjonssonen)
#' # OBS: mangler
#'
#' # RStudio Windows - fra Linux
#'  # OBS: mangler
#'}
#'@encoding UTF-8

read_feather <- function(file, ...) {
  # Jupyterlab (DAPLA)
  if (Sys.getenv('CLUSTER_ID') %in% c("staging-bip-app", "prod-bip-app")) {
    df <- arrow::read_feather(gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)
  }

  # Jupyterlab (produksjonssonen)
  if (grepl("sl-stata-p3", Sys.info()["nodename"]) | grepl("sl-python-03", Sys.info()["nodename"]) |
      (grepl("FW-XAPROD", Sys.info()["nodename"]) & grepl("[A-Za-z]:", file))){
    df <- arrow::read_feather(file, ...)
  }

  # RStudio Windows - fra Linux
  if (grepl("FW-XAPROD", Sys.info()["nodename"]) & grepl("/ssb/", file)){
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


#' Funksjon for å laste inn "multifile" datasett fra Google Cloud Storage bucket
#'
#' Funksjonen `open_dataset` kan brukes til å lese deler av datasett (bl.a. .parquet-, .feather- og .csv-filer) fra Google Cloud Storage. Det lages en forbindelse til mappen der filen ligger og deretter kan man bruke argumenter fra `dplyr`, som `filter` og `select`, før man bruker `collect` til å lese inn dataene i R. `open_dataset` kan også brukes til å lese inn sf-objekter (lagret som .parquet-fil med pakken `sfarrow`).
#'
#' @param bucket Full sti til Google Cloud Storage bucket.
#' @param file Navn på filen som skal leses inn.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_parquet.html)
#'
#' @examples
#' \dontrun{
#' data <- open_dataset("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987_1996_dataset") %>%
#'  dplyr::filter(Year == 1996 & TailNum == "N2823W") %>%
#'  dplyr::select(Year, Month, DayofMonth, TailNum) %>%
#'  dplyr::collect()
#'
#' # SF (OBS: legge denne på ssb-prod-dapla-felles-data-delt)
#  data <- open_dataset(bucket = "ssb-prod-spesh-personell-data-kilde/Vegnett") %>%
#  dplyr::filter(municipality == "301") %>%
#  sfarrow::read_sf_dataset()
#'}
#'@encoding UTF-8

open_dataset <- function(file, ...) {

  # Jupyterlab (DAPLA)
  if (Sys.getenv('CLUSTER_ID') %in% c("staging-bip-app", "prod-bip-app")) {
  ds <- arrow::open_dataset(gcs_bucket(file), ...)
  }

  # Jupyterlab (produksjonssonen)
  if (grepl("sl-stata-p3", Sys.info()["nodename"]) | grepl("sl-python-03", Sys.info()["nodename"]) |
      (grepl("FW-XAPROD", Sys.info()["nodename"]) & grepl("[A-Za-z]:", file))){
  ds <- arrow::open_dataset(file, ...)
  }

  # Windows (produksjonssonen) - fra Linux
  if (grepl("FW-XAPROD", Sys.info()["nodename"]) & grepl("/ssb/", file)){ # FW-XAPROD = RStudio (Windows)
    # OBS: mangler
  }

  return(ds)
}

#' Funksjon for å laste inn .json-fil fra Google Cloud Storage
#'
#' Funksjonen `read_json` kan brukes til å lese inn JSON-filer Google Cloud Storage.
#'
#' @param bucket Full sti til Google Cloud Storage bucket.
#' @param file Navn på filen som skal leses inn.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_json_arrow.html)
#'
#' @examples
#' \dontrun{
#' data <- read_json("ssb-prod-spesh-personell-data-kilde/example_1.json")
#'}
#'@encoding UTF-8

read_json <- function(file, ...) {
  if (Sys.getenv('CLUSTER_ID') %in% c("staging-bip-app", "prod-bip-app")) {
    df <- arrow::read_json_arrow(gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)
  }

  # Jupyterlab (produksjonssonen) + lokale filer fra RStudio Windows (produksjonssonen)
  if (grepl("sl-stata-p3", Sys.info()["nodename"]) | grepl("sl-python-03", Sys.info()["nodename"]) |
      (grepl("FW-XAPROD", Sys.info()["nodename"]) & grepl("[A-Za-z]:", file))){
    df <- arrow::read_json_arrow(file, ...)
  }

  # RStudio Windows (produksjonssonen) - fra Linux
  if (grepl("FW-XAPROD", Sys.info()["nodename"]) & grepl("/ssb/", file)){ # FW-XAPROD = RStudio (RStudio Windows)
    # OBS: mangler
  }

  return(df)

}


#' Funksjon for å laste inn .csv-fil fra Google Cloud Storage bucket
#'
#' Funksjonen `read_csv` kan brukes til å lese inn JSON-filer Google Cloud Storage.
#'
#' @param bucket Full sti til Google Cloud Storage bucket.
#' @param file Navn på filen som skal leses inn.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_delim_arrow.html)
#'
#' @examples
#' \dontrun{
#' data <- read_csv("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.csv")
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

#' Funksjon for å lagre .parquet-fil til Google Cloud Storage bucket
#'
#' Funksjonen `write_parquet` kan brukes til å skrive parquet-filer til Google Cloud Storage bucket.
#'
#' @param bucket Full sti til Google Cloud Storage bucket.
#' @param file Navn på filen som skal skrives.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_delim_arrow.html)
#'
#' @examples
#' \dontrun{
# write_parquet(data, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.parquet")
#' }
#'@encoding UTF-8

write_parquet <- function(data, file, ...) {
  arrow::write_parquet(data, gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)
}

#' Funksjon for å lagre .feather-fil til Google Cloud Storage bucket
#'
#' Funksjonen `write_feather` kan brukes til å skrive .feather-filer til Google Cloud Storage bucket.
#'
#' @param bucket Full sti til Google Cloud Storage bucket.
#' @param file Navn på filen som skal skrives.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/write_feather.html)
#'
#' @examples
#' \dontrun{
#' }
#'@encoding UTF-8

write_feather <- function(data, file, ...) {
  arrow::write_feather(data, gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)

}



#' Funksjon for å lagre .csv-fil til Google Cloud Storage bucket
#'
#' Funksjonen `write_csv` kan brukes til å skrive parquet-filer til Google Cloud Storage bucket.
#'
#' @param bucket Full sti til Google Cloud Storage bucket.
#' @param file Navn på filen som skal skrives.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_delim_arrow.html)
#'
#' @examples
#' \dontrun{
#' write_csv(data, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.csv")
#' }
#'@encoding UTF-8

write_csv <- function(data, bucket, file, ...) {
  arrow::write_csv_arrow(data, gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)

}


#' Funksjon for å sjekke hvilke filer som finnes i en Google Cloud Storage bucket
#'
#' Funksjonen `list.files` kan brukes til å sjekke hvilke filer som finnes i en Google Cloud Storage bucket
#'
#' @param bucket Full sti til Google Cloud Storage bucket.
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



#' Funksjon for å laste inn filer fra Google Cloud Storage bucket
#' #'
#' Funksjonen `read_SSB` kan brukes til å lese inn filer fra Google Cloud Storage.
#'
#' @param bucket Full sti til Google Cloud Storage bucket.
#' @param file Navn på filen som skal leses inn.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_parquet.html)
#'
#' @examples
#' \dontrun{
#' read_SSB_parquet <- read_SSB("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.parquet")
#'
#' read_SSB_MFD <- read_SSB("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987_1996_dataset")
#'
#' read_SSB_feather <- read_SSB("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.feather")
#'
#' read_SSB_csv <- read_SSB("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.csv")
#'
#' read_SSB_json <- read_SSB("ssb-prod-spesh-personell-data-kilde/example_1.json") # OBS: flytt filen til delt
#'}
#'@encoding UTF-8

read_SSB <- function(file, ...) {

  if(grepl("\\.parquet", basename(file))){
    df <- read_parquet(file, ...)
  } else if(grepl("\\.feather", basename(file))){
    df <- read_feather(file, ...)
  } else if(grepl("\\.csv", basename(file)) | grepl(".txt", basename(file)) | grepl(".dat", basename(file))){
    df <- read_csv(file, ...)
  } else if(grepl("\\.json", basename(file))){
    df <- read_json(file, ...)
  } else {
   df <- open_dataset(file, ...) %>%
       dplyr::collect()
    }
  return(df)

  }


#' Funksjon for å skrive filer til Google Cloud Storage bucket
#' #'
#' Funksjonen `write_SSB` kan brukes til å skrive filer til Google Cloud Storage.
#'
#' @param bucket Full sti til Google Cloud Storage bucket.
#' @param file Navn på filen som skal leses inn.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_parquet.html)
#'
#' @examples
#' \dontrun{
#'}
#'@encoding UTF-8

write_SSB <- function(data, file, partitioning = FALSE, ...) { # OBS: legge til mulighet for partitioning?
  if (grepl("\\.parquet", basename(file))){
    write_parquet(data, file, ...)
  } else if (grepl("\\.feather", basename(file))){
    write_feather(data, file, ...)
  } else if (grepl("\\.csv", basename(file)) | grepl(".txt", basename(file)) | grepl(".dat", basename(file))){
    write_csv(data, file, ...)
  } else if (partitioning == TRUE) {
    print("OBS: mangler")
  } else {
    print("write_SSB kan for øyeblikket kun skrive .parquet-, .feather- og .csv-filer")
  }
}
