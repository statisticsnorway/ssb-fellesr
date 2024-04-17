#' Funksjon for aa sjekke hvilket miljoe man er i
#'
#' `env_check` er en hjelpefunksjon som sjekker hvilket miljø man er i. DAPLA_ENVIRONMENT: Angir miljøet (DEV, STAGING, TEST, PROD), DAPLA_REGION: Viser kjøreområdet (ON_PREM, DAPLA_LAB, BIP, CLOUD_RUN) og DAPLA_SERVICE: Identifiserer tjenesten (JUPYTERLAB, VS_CODE, R_STUDIO, KILDOMATEN).
#'
#' @returns Karaktervektor: f.eks. PROD-ON_PREM-JUPYTERLAB eller PROD-BIP-JUPYTERLAB
#'
#' @export
#'
#' @examples
#' \dontrun{
#' env_check()
#'}
#'@encoding UTF-8

env_check <- function() {

  env <- paste0(Sys.getenv("DAPLA_ENVIRONMENT"), "-",
                Sys.getenv("DAPLA_REGION"), "-",
                Sys.getenv("DAPLA_SERVICE"))

  if (Sys.getenv("DAPLA_REGION") == "" | Sys.getenv("DAPLA_ENVIRONMENT") == "" | Sys.getenv("DAPLA_SERVICE") == ""){
    warning("Ukjent miljø. Denne funksjonene fungerer kun på Dapla og i produksjonssonen")
  }
  return(env)
}



#' Funksjon for aa koble til Google Cloud Storage bucket med arrow
#'
#' `gcs_bucket` er en hjelpefunksjon som kobler til en bucket paa Google Cloud Storage med pakken `arrow`. Autentiseringen skjer via access_token og expiration som er lagret som miljoevariabler i Jupyter paa DAPLA.
#'
#' @param bucket Full sti til Google Cloud Storage bucket.
#' @export
#' @examples
#' \dontrun{
#' bucket <- gcs_bucket("ssb-prod-dapla-felles-data-delt")
#' bucket
#'}
#'@encoding UTF-8

gcs_bucket <- function(bucket) {
  if (Sys.getenv("DAPLA_REGION") == "BIP"){
    response <- httr::GET(Sys.getenv('LOCAL_USER_PATH'), httr::add_headers('Authorization' = paste0('token ', Sys.getenv("JUPYTERHUB_API_TOKEN"))))
    access_token <- httr::content(response)$exchanged_tokens$google$access_token
    expiration <- httr::content(response)$exchanged_tokens$google$exp
  }
  else if (Sys.getenv("DAPLA_REGION") == "DAPLA_LAB"){

    response <- httr::POST(Sys.getenv("OIDC_TOKEN_EXCHANGE_URL"),
                           httr::add_headers("Content-Type" = "application/x-www-form-urlencoded"),
                           body = list(subject_token = Sys.getenv('OIDC_TOKEN'),
                                       grant_type = "urn:ietf:params:oauth:grant-type:token-exchange",
                                       requested_token_type = "urn:ietf:params:oauth:token-type:access_token",
                                       requested_issuer = "google",
                                       client_id = "onyxia"),
                           encode = "form")
    access_token <- httr::content(response)$access_token
    expiration <- Sys.time() + as.numeric(httr::content(response)$expires_in)
  }
  else {
    access_token <- getPass::getPass("Skriv inn access_token")
    expiration <- as.numeric(getPass::getPass("Skriv inn expiration"))
  }
  bucket <- arrow::gs_bucket(bucket, access_token = access_token, expiration = as.POSIXct(expiration, origin="1970-01-01"))
}

#' Funksjon for aa koble til Google Cloud Storage bucket med googleCloudStorageR
#'
#' `gcs_global_bucket` er en hjelpefunksjon som kobler til en bucket paa Google Cloud Storage med pakken `googleCloudStorageR`. Autentiseringen skjer via access_token og expiration som er lagret som miljoevariabler i Jupyter paa DAPLA.
#'
#' @param bucket Full sti til Google Cloud Storage bucket.
#'
#' @examples
#' \dontrun{
#' gcs_global_bucket("ssb-prod-dapla-felles-data-delt")
#' bucket
#'}
#'@encoding UTF-8

gcs_global_bucket <- function(bucket) {
  gcs_auth <- function() {
    manual_token <- function(scopes, ...) {
      if (Sys.getenv("DAPLA_REGION") == "BIP"){
        response <- httr::GET(Sys.getenv('LOCAL_USER_PATH'),
                              httr::add_headers('Authorization' = paste0('token ', Sys.getenv("JUPYTERHUB_API_TOKEN"))))
        credentials <- list()
        credentials$access_token <- httr::content(response)$exchanged_tokens$google$access_token
      }
      else if (Sys.getenv("DAPLA_REGION") == "DAPLA_LAB"){
        response <- httr::POST(Sys.getenv("OIDC_TOKEN_EXCHANGE_URL"),
                               httr::add_headers("Content-Type" = "application/x-www-form-urlencoded"),
                               body = list(subject_token = Sys.getenv('OIDC_TOKEN'),
                                           grant_type = "urn:ietf:params:oauth:grant-type:token-exchange",
                                           requested_token_type = "urn:ietf:params:oauth:token-type:access_token",
                                           requested_issuer = "google",
                                           client_id = "onyxia"),
                               encode = "form")

        credentials <- list()
        credentials$access_token <- httr::content(response)$access_token
      }
      httr::oauth2.0_token(endpoint = NULL, app = gargle::gargle_client(), scope = NULL, credentials = credentials)
    }

    gargle::cred_funs_add(manual_token = NULL)
    gargle::cred_funs_add(manual_token = manual_token)

    token <- gargle::token_fetch()
    googleCloudStorageR::gcs_auth(token = token)
  }

  gcs_auth()
  googleCloudStorageR::gcs_global_bucket(bucket)
}

#' Funksjon for aa laste inn .parquet-fil fra Google Cloud Storage bucket
#'
#' Funksjonen `read_parquet` kan brukes til aa lese inn .parquet-filer fra Google Cloud Storage.
#'
#' @param file Full sti og navn paa filen som skal leses inn fra Google Cloud Storage bucket.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_parquet.html)
#'
#' @examples
#' \dontrun{
#' data <- read_parquet("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.parquet")
#'}
#'@encoding UTF-8

read_parquet <- function(file, ...) {

    # Fjerner "gs://" fra filstien dersom det er spesifisert
file <- gsub("gs://", "", file)

  # DAPLA
  if (Sys.getenv("DAPLA_REGION") == "BIP" | Sys.getenv("DAPLA_REGION") == "DAPLA_LAB") {
    df <- arrow::read_parquet(gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)
  }

  # Produksjonssonen
  if (Sys.getenv("DAPLA_REGION") == "ON_PREM"){
    df <- arrow::read_parquet(file, ...)
  }

  return(df)
}

#' Funksjon for aa laste inn .parquet-fil (i sf-format) fra Google Cloud Storage bucket
#'
#' Funksjonen `read_parquet_sf` kan brukes til aa lese inn .parquet-filer (i sf-format) fra Google Cloud Storage.
#'
#' @param file Full sti og navn paa filen som skal leses inn fra Google Cloud Storage bucket.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_parquet.html)
#'
#' @examples
#' \dontrun{
#' data <- read_parquet_sf("ssb-prod-dapla-felles-data-delt/GIS/testdata/veger_oslo.parquet")
#'}
#'@encoding UTF-8

read_parquet_sf <- function(file, ...) {

  # Fjerner "gs://" fra filstien dersom det er spesifisert
  file <- gsub("gs://", "", file)

  # DAPLA
  if (Sys.getenv("DAPLA_REGION") == "BIP" | Sys.getenv("DAPLA_REGION") == "DAPLA_LAB") {

    ds <- arrow::read_parquet(gcs_bucket(dirname(file))$path(paste0(basename(file))), as_data_frame = FALSE, ...)

    metadata <- ds$metadata
    geo <- jsonlite::fromJSON(metadata$geo)
    crs <- geo$columns$geometry$crs
    sfarrow:::validate_metadata(geo)
    df <- dplyr::collect(ds)
    df <- as.data.frame(df)
    primary_geom <- geo$primary_column
    df[[primary_geom]] <- sf::st_as_sfc(df[[primary_geom]], crs = sf::st_crs(geo$columns$geometry$crs), EWKB=TRUE)
    df <- sf::st_sf(df, sf_column_name = primary_geom)

  }

  # Produksjonssonen
  if (Sys.getenv("DAPLA_REGION") == "ON_PREM"){
    ds <- arrow::read_parquet(file, as_data_frame = FALSE, ...)
    metadata <- ds$metadata
    geo <- jsonlite::fromJSON(metadata$geo)
    crs <- geo$columns$geometry$crs
    sfarrow:::validate_metadata(geo)
    df <- dplyr::collect(ds)
    df <- as.data.frame(df)
    primary_geom <- geo$primary_column
    df[[primary_geom]] <- sf::st_as_sfc(df[[primary_geom]], crs = sf::st_crs(geo$columns$geometry$crs), EWKB=TRUE)
    df <- sf::st_sf(df, sf_column_name = primary_geom)
  }

  return(df)
}


#' Funksjon for aa laste inn .feather-fil fra Google Cloud Storage bucket
#'
#' Funksjonen `read_feather` kan brukes til aa lese inn .feather-filer fra Google Cloud Storage.
#'
#' @param file Full sti og navn paa filen som skal leses inn fra Google Cloud Storage bucket.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_feather.html)
#'
#' @examples
#' \dontrun{
#' data <- read_feather("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.feather")
#'}
#'@encoding UTF-8

read_feather <- function(file, ...) {

  # Fjerner "gs://" fra filstien dersom det er spesifisert
file <- gsub("gs://", "", file)

  # DAPLA
  if (Sys.getenv("DAPLA_REGION") == "BIP" | Sys.getenv("DAPLA_REGION") == "DAPLA_LAB") {
    df <- arrow::read_feather(gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)
  }

  # Produksjonssonen
  if (Sys.getenv("DAPLA_REGION") == "ON_PREM"){
    df <- arrow::read_feather(file, ...)
  }

  return(df)
}


#' Funksjon for aa laste inn "multifile" datasett fra Google Cloud Storage bucket
#'
#' Funksjonen `open_dataset` kan brukes til aa lese deler av datasett (bl.a. .parquet-, .feather- og .csv-filer) fra Google Cloud Storage. Det lages en forbindelse til mappen der filen ligger og deretter kan man bruke argumenter fra `dplyr`, som `filter` og `select`, foer man bruker `collect` til aa lese inn dataene i R. `open_dataset` kan ogsaa brukes til aa lese inn sf-objekter (lagret som .parquet-fil med pakken `sfarrow`).
#'
#' @param file Full sti og navn paa filen som skal leses inn fra Google Cloud Storage bucket.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/open_dataset.html)
#' @export
#' @examples
#' \dontrun{
#' data <- open_dataset("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987_1996_dataset") %>%
#'  dplyr::filter(Year == 1996 & TailNum == "N2823W") %>%
#'  dplyr::select(Year, Month, DayofMonth, TailNum) %>%
#'  dplyr::collect()
#'
#' data <- open_dataset("ssb-prod-dapla-felles-data-delt/GIS/Vegnett/2022") %>%
#'  dplyr::filter(municipality == "301") %>%
#'  sfarrow::read_sf_dataset()
#'  }
#'@encoding UTF-8

open_dataset <- function(file, ...) {

    # Fjerner "gs://" fra filstien dersom det er spesifisert
file <- gsub("gs://", "", file)

  # DAPLA
  if (Sys.getenv("DAPLA_REGION") == "BIP" | Sys.getenv("DAPLA_REGION") == "DAPLA_LAB") {
    ds <- arrow::open_dataset(gcs_bucket(file), ...)
  }

  # Produksjonssonen
  if (Sys.getenv("DAPLA_REGION") == "ON_PREM"){
    ds <- arrow::open_dataset(file, ...)
  }

  return(ds)
}

#' Funksjon for aa laste inn .json-fil fra Google Cloud Storage
#'
#' Funksjonen `read_json` kan brukes til aa lese inn .json-filer Google Cloud Storage.
#'
#' @param file Full sti og navn paa filen som skal leses inn fra Google Cloud Storage bucket.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_json_arrow.html)
#'
#' @examples
#' \dontrun{
#' data <- read_json("ssb-prod-spesh-personell-data-kilde/example_1.json")
#'}
#'@encoding UTF-8

read_json <- function(file, ...) {

  # Fjerner "gs://" fra filstien dersom det er spesifisert
  file <- gsub("gs://", "", file)

  if (Sys.getenv("DAPLA_REGION") == "BIP" | Sys.getenv("DAPLA_REGION") == "DAPLA_LAB") {

    df <- arrow::read_json_arrow(gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)
  }

  # Jupyterlab (produksjonssonen) + lokale filer fra RStudio Windows (produksjonssonen)
  if (Sys.getenv("DAPLA_REGION") == "ON_PREM"){
    df <- arrow::read_json_arrow(file, ...)
  }

  return(df)

}


#' Funksjon for aa laste inn .csv-fil fra Google Cloud Storage bucket
#'
#' Funksjonen `read_csv` kan brukes til aa lese inn .csv-filer Google Cloud Storage.
#'
#' @param file Full sti og navn paa filen som skal leses inn fra Google Cloud Storage bucket.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_delim_arrow.html)
#'
#' @examples
#' \dontrun{
#' data <- read_csv("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.csv")
#'}
#'@encoding UTF-8

read_csv <- function(file, ...) {

  # Fjerner "gs://" fra filstien dersom det er spesifisert
  file <- gsub("gs://", "", file)

  # DAPLA
  if (Sys.getenv("DAPLA_REGION") == "BIP" | Sys.getenv("DAPLA_REGION") == "DAPLA_LAB") {
    df <- arrow::read_delim_arrow(gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)
  }

  # Produksjonssonen
  if (Sys.getenv("DAPLA_REGION") == "ON_PREM"){
    df <- readr::read_delim(file, ...)
  }

  return(df)
}

#' Funksjon for aa laste inn .rds-fil fra Google Cloud Storage bucket
#'
#' Funksjonen `read_rds` kan brukes til aa lese inn .rds-filer fra Google Cloud Storage.
#'
#' @param file Full sti og navn paa filen som skal leses inn fra Google Cloud Storage bucket.
#' @param ... Flere parametere (se: https://rdrr.io/cran/googleCloudStorageR/man/gcs_get_object.html)
#'
#' @examples
#' \dontrun{
#' data <- read_rds("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.rds")
#'}
#'@encoding UTF-8

read_rds <- function(file, ...) {

  # Fjerner "gs://" fra filstien dersom det er spesifisert
  file <- gsub("gs://", "", file)

  # DAPLA
  if (Sys.getenv("DAPLA_REGION") == "BIP" | Sys.getenv("DAPLA_REGION") == "DAPLA_LAB") {

    gcs_global_bucket(sub("/.*", "", file))

    my_parse <- function(obj){
      tmp <- tempfile(fileext = ".rds")
      on.exit(unlink(tmp))
      suppressMessages(googleCloudStorageR::gcs_get_object(obj, saveToDisk = tmp))
      readRDS(tmp)
    }

    df <- my_parse(sub(paste0(".*", sub("/.*", "", file), "/"), "", file))
  }

  # Produksjonssonen
  if (Sys.getenv("DAPLA_REGION") == "ON_PREM"){
    df <- readRDS(file, ...)
  }
  return(df)
}


#' Funksjon for aa laste inn .xml-fil fra Google Cloud Storage bucket
#'
#' Funksjonen `read_xml` kan brukes til aa lese inn .xml-filer fra Google Cloud Storage.
#'
#' @param file Full sti og navn paa filen som skal leses inn fra Google Cloud Storage bucket.
#' @param ... Flere parametere (se: https://rdrr.io/cran/googleCloudStorageR/man/gcs_get_object.html)
#'
#' @examples
#' \dontrun{
#' data <- read_xml("ssb-prod-dapla-felles-data-delt/R_smoke_test/XXX.xml")
#'}
#'@encoding UTF-8

read_xml <- function(file, ...) {

  # Fjerner "gs://" fra filstien dersom det er spesifisert
  file <- gsub("gs://", "", file)

  # DAPLA
  if (Sys.getenv("DAPLA_REGION") == "BIP" | Sys.getenv("DAPLA_REGION") == "DAPLA_LAB") {

    suppressMessages(gcs_global_bucket(sub("/.*", "", file)))

    my_parse <- function(obj){
      tmp <- tempfile(fileext = ".xml")
      on.exit(unlink(tmp))
      suppressMessages(googleCloudStorageR::gcs_get_object(obj, saveToDisk = tmp))
      XML::xmlToDataFrame(tmp)
    }

    df <- my_parse(sub(paste0(".*", sub("/.*", "", file), "/"), "", file))
  }

  # Produksjonssonen (mangler)

  return(df)
}


#' Funksjon for aa lagre .parquet-fil til Google Cloud Storage bucket
#'
#' Funksjonen `write_parquet` kan brukes til aa skrive .parquet-filer til Google Cloud Storage bucket.
#'
#' @param data Filen som skal skrives.
#' @param file Full filsti og filnavn for hvor filen skal skrives.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/write_parquet.html)
#'
#' @examples
#' \dontrun{
#' write_parquet(data, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.parquet")
#' }
#'@encoding UTF-8

write_parquet <- function(data, file, ...) {

# Fjerner "gs://" fra filstien dersom det er spesifisert
file <- gsub("gs://", "", file)

  # DAPLA
  if (Sys.getenv("DAPLA_REGION") == "BIP" | Sys.getenv("DAPLA_REGION") == "DAPLA_LAB") {
    arrow::write_parquet(data, gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)
  }

  # Produksjonssonen
  if (Sys.getenv("DAPLA_REGION") == "ON_PREM"){
    arrow::write_parquet(data, file, ...)
  }


}






#' Funksjon for aa lagre "partitioned" .parquet-fil til Google Cloud Storage bucket
#'
#' Funksjonen `write_dataset` kan brukes til aa skrive "partitioned" .parquet-filer til Google Cloud Storage bucket. "Partitioning" angis ut fra hvilke variabler datasettet er gruppert etter (gjoeres via [dplyr::group_by()]).
#'
#' @param data Filen som skal skrives.
#' @param file Full filsti og filnavn for hvor filen skal skrives.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/write_dataset.html)
#'
#' @examples
#' \dontrun{
#' write_dataset(data, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_multifile_dataset_test")
#' }
#'@encoding UTF-8

write_dataset <- function(data, file, ...) {

  # Fjerner "gs://" fra filstien dersom det er spesifisert
  file <- gsub("gs://", "", file)

  arrow::write_dataset(data, gcs_bucket(dirname(file))$path(paste0(basename(file))),
                       partitioning = dplyr::group_vars(data))
}



#' Funksjon for aa lagre .feather-fil til Google Cloud Storage bucket
#'
#' Funksjonen `write_feather` kan brukes til aa skrive .feather-filer til Google Cloud Storage bucket.
#'
#' @param data Filen som skal skrives.
#' @param file Full filsti og filnavn for hvor filen skal skrives.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/write_feather.html)
#'
#' @examples
#' \dontrun{
#' write_feather(data, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.feather")
#' }
#'@encoding UTF-8

write_feather <- function(data, file, ...) {

  # Fjerner "gs://" fra filstien dersom det er spesifisert
  file <- gsub("gs://", "", file)

  # DAPLA
  if (Sys.getenv("DAPLA_REGION") == "BIP" | Sys.getenv("DAPLA_REGION") == "DAPLA_LAB") {
    arrow::write_feather(data, gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)
  }

  # Produksjonssonen
  if (Sys.getenv("DAPLA_REGION") == "ON_PREM"){
    arrow::write_feather(data, file, ...)
  }

}



#' Funksjon for aa lagre .csv-fil til Google Cloud Storage bucket
#'
#' Funksjonen `write_csv` kan brukes til aa skrive .csv-filer til Google Cloud Storage bucket.
#'
#' @param data Filen som skal skrives.
#' @param file Full filsti og filnavn for hvor filen skal skrives.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_delim_arrow.html)
#'
#' @examples
#' \dontrun{
#' write_csv(data, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.csv")
#' }
#'@encoding UTF-8

write_csv <- function(data,
                      file, ...) {

  # Fjerner "gs://" fra filstien dersom det er spesifisert
  file <- gsub("gs://", "", file)

  # DAPLA
  if (Sys.getenv("DAPLA_REGION") == "BIP" | Sys.getenv("DAPLA_REGION") == "DAPLA_LAB") {
    arrow::write_csv_arrow(data, gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)
  }

  # Produksjonssonen
  if (Sys.getenv("DAPLA_REGION") == "ON_PREM"){
    arrow::write_csv_arrow(data, file, ...)
  }


}

#' Funksjon for aa lagre .rds-fil til Google Cloud Storage bucket
#'
#' Funksjonen `write_rds` kan brukes til aa skrive .rds-filer til Google Cloud Storage bucket.
#'
#' @param data Filen som skal skrives.
#' @param file Full filsti og filnavn for hvor filen skal skrives.
#' @param ... Flere parametere (se dokumentasjonen til: [fellesr::write_parquet()]/[fellesr::write_sf_parquet()]/[fellesr::write_feather()]/[fellesr::write_csv()]/[fellesr::write_dataset()])
#' @examples
#' \dontrun{
#' write_rds(data, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_rds_test.rds")
#' }
#'@encoding UTF-8

write_rds <- function(data,
                      file, ...) {

  # Fjerner "gs://" fra filstien dersom det er spesifisert
  file <- gsub("gs://", "", file)

  f <- function(input, output){
    saveRDS(input, file = output)
  }

  gcs_global_bucket(sub("/.*", "", file))
  googleCloudStorageR::gcs_upload(data, name = sub(paste0(".*", sub("/.*", "", file), "/"), "", file), object_function = f)

}


#' Funksjon for aa sjekke hvilke filer som finnes i en mappe i en Google Cloud Storage bucket
#'
#' Funksjonen `gcs.list.files` kan brukes til aa sjekke hvilke filer som finnes i en Google Cloud Storage bucket
#'
#' @param bucket Full sti til Google Cloud Storage bucket (med eventuelle undermapper).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' gcs.list.files("ssb-prod-dapla-felles-data-delt/R_smoke_test")
#' }
#'@encoding UTF-8
#'

gcs.list.files <- function(bucket) {

  # Fjerner "gs://" fra filstien dersom det er spesifisert
  bucket <- gsub("gs://", "", bucket)

  gcs_bucket(bucket)$ls(recursive = T)
}

#' Funksjon for aa sjekke hvilke filer som finnes i en Google Cloud Storage bucket
#'
#' Funksjonen `gcs_list_objects` kan brukes til aa sjekke hvilke filer som finnes i en Google Cloud Storage bucket. I tillegg ser man stoerrelsen til filene og tidspnktet filen sist ble endret.
#'
#' @param bucket Full sti til Google Cloud Storage bucket.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' gcs_list_objects("ssb-prod-dapla-felles-data-delt")
#'
#' gcs_list_objects("ssb-prod-dapla-felles-data-delt") %>%
#' dplyr::filter(grepl("GIS", name) == T)
#' dplyr::arrange(desc(updated))
#'
#' }
#'@encoding UTF-8
#'

gcs_list_objects <- function(bucket) {

  # Fjerner "gs://" fra filstien dersom det er spesifisert
  bucket <- gsub("gs://", "", bucket)

  if (dirname(bucket) == "."){
    gcs_global_bucket(bucket)
    googleCloudStorageR::gcs_list_objects(bucket)
  } else {
    gcs_global_bucket(dirname(bucket))
    googleCloudStorageR::gcs_list_objects(dirname(bucket))
  }
}


#' Funksjon for aa slette fil fra en Google Cloud Storage bucket
#'
#' Funksjonen `gcs_delete_object` kan brukes til aa slette filer fra Google Cloud Storage bucket.
#'
#' @param file Full sti til Google Cloud Storage bucket og filen som skal slettes.
#'
#' @examples
#' \dontrun{
#' gcs_delete_object("ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.parquet")
#' }
#'@encoding UTF-8
#'

gcs_delete_object <- function(file) {

  # Fjerner "gs://" fra filstien dersom det er spesifisert
  file <- gsub("gs://", "", file)

  gcs_global_bucket(sub("/.*", "", file))
  googleCloudStorageR::gcs_delete_object(sub(paste0(".*", sub("/.*", "", file), "/"), "", file))
}


#' Funksjon for aa lagre sf-objekt som .parquet-fil til Google Cloud Storage bucket
#'
#' Funksjonen `write_sf_parquet` kan brukes til aa skrive sf-objekter som .parquet-filer til Google Cloud Storage bucket.
#'
#' @param data Filen som skal skrives.
#' @param file Full filsti og filnavn for hvor filen skal skrives.
#' @param ... Flere parametere (se dokumentasjonen til: [fellesr::write_parquet()]/[fellesr::write_sf_parquet()]/[fellesr::write_feather()]/[fellesr::write_csv()]/[fellesr::write_dataset()])
#'
#' @export
#'
#' @examples
#' \dontrun{
#' write_sf_parquet(data, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_sf.parquet")
#' }
#'@encoding UTF-8
#'

write_sf_parquet <- function(data, file, ...) {

  # Fjerner "gs://" fra filstien dersom det er spesifisert
  file <- gsub("gs://", "", file)

  geo_metadata <- sfarrow:::create_metadata(data)
  df <- sfarrow::encode_wkb(data)
  tbl <- arrow::Table$create(df)
  tbl$metadata[["geo"]] <- geo_metadata

  write_parquet(tbl, file, ...)
}

gcs_validate_file_path <- function(path) {

  regex <- paste0("^(?:gs:\\/\\/)?(?<bucket>[^\\/\\.:]+)\\/",
                  "(?<file>[^\\.]+(?<extension>\\..+))$")

  match <- stringr::str_match(path, regex)[1,]

  if (is.na(match[1])) {

    stop("`file` har uventet format.")

  } else {

    return(match)

  }

}

#' Import av filer fra GCS-bøtter med `rio::import`.
#'
#' @param file Full sti og filnavn på filen som skal importeres. Eventuelt
#'   `gs://`-prefiks fjernes, og det øverste mappenivået tolkes som navn på
#'   GCS-bøtten.
#' @param ... Argumenter som sendes videre til [rio::import], og dermed til
#'   underliggende eksporterende funksjoner.
#'
#' @return Resultatet av å kjøre [rio::import] på den angitte filen.
#'
#' @details Funksjonen laster ned den angitte filen på GCS til en midlertidig
#' fil i det lokale filsystemet, og benytter [rio::import] til å importere den.
#'
#' Se [rio::import] for en oversikt over filformatene som kan importeres med
#' denne funksjonen.
#'
#' @export
#'
#' @seealso [gcs_export()] og [rio::import()]
#'
#' @examples
#'
#' \dontrun{
#'
#' gcs_import("ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_xlsx_test.xlsx")
#' gcs_import("ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_csv_test.csv")
#' gcs_import("ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.parquet")
#' gcs_import("ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_rds_test.rds")
#' gcs_import("ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_json_test.json")
#'
#'
#' }
#'
gcs_import <- function(file, ...) {

  path <- gcs_validate_file_path(file)

  tmp <- tempfile(fileext = path["extension"])

  on.exit(unlink(tmp))

  suppressMessages(gcs_global_bucket(path["bucket"]))

  suppressMessages(
    googleCloudStorageR::gcs_get_object(object_name = path["file"],
                                        saveToDisk  = tmp)
  )

  return(rio::import(tmp, ...))

}


#' Eksport av filer til GCS-bøtter med `rio::export`.
#'
#' Skriv data.frame og andre objekter til bøtter med  [rio::export].
#'
#' @param data objekt som skal eksporteres.
#' @param file Full sti og filnavn på filen som skal opprettes. Eventuelt
#'   `gs://`-prefiks fjernes, og det øverste mappenivået tolkes som navn på
#'   GCS-bøtten.
#' @param ... Argumenter som sendes videre til [rio::export], og dermed til
#'   underliggende eksporterende funksjoner.
#' @param export_function En funksjon med parametere `input` og `output` som
#'   eksporterer den ønskede filen.
#'
#' @return Resultatet av eksporten som returnert av [rio::export].
#'
#' @details Parameterene `x`, `file`, og alle parametere i `...` blir sendt
#' videre til [rio::export] (default), eller en funksjon angitt .
#'
#' Se [rio::export] for en oversikt over filformatene som kan eksporteres med
#' denne funksjonen.
#'
#' @export
#'
#' @seealso [gcs_import()] og [rio::export()]
#'
#' @examples
#' \dontrun{
#'
#' gcs_export(mtcars, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_xlsx_test.xlsx")
#'
#' gcs_export(mtcars, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_csv_test.csv")
#'
#' gcs_export(mtcars, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.parquet")
#'
#' gcs_export(mtcars, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_rds_test.rds")
#'
#' gcs_export(data = list(foo = "bar", egg = "ham"),
#'            file = "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_json_test.json")
#'
#' }
#'
gcs_export <- function(data, file, ...) {

  path <- gcs_validate_file_path(file)

  export_function <- function(input, output) {

    do.call(rio::export, c(list(x = input, file = output), list(...)))

  }

  gcs_global_bucket(path["bucket"])

  invisible(
    googleCloudStorageR::gcs_upload(file = data,
                                    name = path["file"],
                                    predefinedAcl = "bucketLevel",
                                    object_function = export_function)
  )

}
