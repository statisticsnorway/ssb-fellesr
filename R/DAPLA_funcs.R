#' Funksjon for å koble til Google Cloud Storage bucket med arrow
#'
#' `gcs_bucket` er en hjelpefunksjon som kobler til en bucket på Google Cloud Storage med pakken `arrow`. Autentiseringen skjer via access_token og expiration som er lagret som miljøvariabler i Jupyter på DAPLA.
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

#' Funksjon for å koble til Google Cloud Storage bucket med googleCloudStorageR
#'
#' `gcs_global_bucket` er en hjelpefunksjon som kobler til en bucket på Google Cloud Storage med pakken `googleCloudStorageR`. Autentiseringen skjer via access_token og expiration som er lagret som miljøvariabler i Jupyter på DAPLA.
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
      response <- httr::GET(Sys.getenv('LOCAL_USER_PATH'),
                            httr::add_headers('Authorization' = paste0('token ', Sys.getenv("JUPYTERHUB_API_TOKEN"))))
      credentials <- list()
      credentials$access_token <- httr::content(response)$exchanged_tokens$google$access_token
      httr::oauth2.0_token(endpoint = NULL, app = gargle::gargle_app(), scope = NULL, credentials = credentials)
    }

    gargle::cred_funs_add(manual_token)

    token <- gargle::token_fetch()
    googleCloudStorageR::gcs_auth(token = token)
  }

  gcs_auth()
  googleCloudStorageR::gcs_global_bucket(bucket)
}

#' Funksjon for å laste inn .parquet-fil fra Google Cloud Storage bucket
#'
#' Funksjonen `read_parquet` kan brukes til å lese inn .parquet-filer fra Google Cloud Storage.
#'
#' @param file Full sti og navn på filen som skal leses inn fra Google Cloud Storage bucket.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_parquet.html)
#'
#' @examples
#' \dontrun{
#' data <- read_parquet("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.parquet")
#'}
#'@encoding UTF-8

read_parquet <- function(file, ...) {

  # Jupyterlab (DAPLA)
  if (Sys.getenv('CLUSTER_ID') %in% c("staging-bip-app", "prod-bip-app")) {
    df <- arrow::read_parquet(gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)
  }

  # Jupyterlab (produksjonssonen)
  if (grepl("sl-stata-p3", Sys.info()["nodename"]) | grepl("sl-python-03", Sys.info()["nodename"]) | grepl("onprem", Sys.getenv("JUPYTER_IMAGE_SPEC")) |
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
#'
#' Funksjonen `read_feather` kan brukes til å lese inn .feather-filer fra Google Cloud Storage.
#'
#' @param file Full sti og navn på filen som skal leses inn fra Google Cloud Storage bucket.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/read_feather.html)
#'
#' @examples
#' \dontrun{
#' data <- read_feather("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.feather")
#'}
#'@encoding UTF-8

read_feather <- function(file, ...) {
  # Jupyterlab (DAPLA)
  if (Sys.getenv('CLUSTER_ID') %in% c("staging-bip-app", "prod-bip-app")) {
    df <- arrow::read_feather(gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)
  }

  # Jupyterlab (produksjonssonen)
  if (grepl("sl-stata-p3", Sys.info()["nodename"]) | grepl("sl-python-03", Sys.info()["nodename"]) | grepl("onprem", Sys.getenv("JUPYTER_IMAGE_SPEC")) |
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
#' @param file Full sti og navn på filen som skal leses inn fra Google Cloud Storage bucket.
#' @param ... Flere parametere (se: https://arrow.apache.org/docs/r/reference/open_dataset.html)
#'
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

  # Jupyterlab (DAPLA)
  if (Sys.getenv('CLUSTER_ID') %in% c("staging-bip-app", "prod-bip-app")) {
    ds <- arrow::open_dataset(gcs_bucket(file), ...)
  }

  # Jupyterlab (produksjonssonen)
  if (grepl("sl-stata-p3", Sys.info()["nodename"]) | grepl("sl-python-03", Sys.info()["nodename"]) | grepl("onprem", Sys.getenv("JUPYTER_IMAGE_SPEC")) |
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
#' Funksjonen `read_json` kan brukes til å lese inn .json-filer Google Cloud Storage.
#'
#' @param file Full sti og navn på filen som skal leses inn fra Google Cloud Storage bucket.
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
  if (grepl("sl-stata-p3", Sys.info()["nodename"]) | grepl("sl-python-03", Sys.info()["nodename"]) | grepl("onprem", Sys.getenv("JUPYTER_IMAGE_SPEC")) |
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
#' Funksjonen `read_csv` kan brukes til å lese inn .csv-filer Google Cloud Storage.
#'
#' @param file Full sti og navn på filen som skal leses inn fra Google Cloud Storage bucket.
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
  if (grepl("sl-stata-p3", Sys.info()["nodename"]) | grepl("sl-python-03", Sys.info()["nodename"]) | grepl("onprem", Sys.getenv("JUPYTER_IMAGE_SPEC")) |
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

#' Funksjon for å laste inn .rds-fil fra Google Cloud Storage bucket
#'
#' Funksjonen `read_rds` kan brukes til å lese inn .rds-filer fra Google Cloud Storage.
#'
#' @param file Full sti og navn på filen som skal leses inn fra Google Cloud Storage bucket.
#' @param ... Flere parametere (se: https://rdrr.io/cran/googleCloudStorageR/man/gcs_get_object.html)
#'
#' @examples
#' \dontrun{
#' data <- read_rds("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.rds")
#'}
#'@encoding UTF-8

read_rds <- function(file, ...) {

  if (Sys.getenv('CLUSTER_ID') %in% c("staging-bip-app", "prod-bip-app")) {
    gcs_global_bucket(sub("/.*", "", file))

    my_parse <- function(obj){
      tmp <- tempfile(fileext = ".rds")
      on.exit(unlink(tmp))
      suppressMessages(googleCloudStorageR::gcs_get_object(obj, saveToDisk = tmp))
      readRDS(tmp)
    }

    df <- my_parse(sub(paste0(".*", sub("/.*", "", file), "/"), "", file))
  }

  # Jupyterlab (produksjonssonen)
  if (grepl("sl-stata-p3", Sys.info()["nodename"]) | grepl("sl-python-03", Sys.info()["nodename"]) | grepl("onprem", Sys.getenv("JUPYTER_IMAGE_SPEC")) |
      (grepl("FW-XAPROD", Sys.info()["nodename"]) & grepl("[A-Za-z]:", file))){
    df <- readRDS(file, ...)
  }
  return(df)
}


#' Funksjon for å laste inn .xml-fil fra Google Cloud Storage bucket
#'
#' Funksjonen `read_xml` kan brukes til å lese inn .xml-filer fra Google Cloud Storage.
#'
#' @param file Full sti og navn på filen som skal leses inn fra Google Cloud Storage bucket.
#' @param ... Flere parametere (se: https://rdrr.io/cran/googleCloudStorageR/man/gcs_get_object.html)
#'
#' @examples
#' \dontrun{
#' data <- read_xml("ssb-prod-dapla-felles-data-delt/R_smoke_test/XXX.xml")
#'}
#'@encoding UTF-8

read_xml <- function(file, ...) {

  if (Sys.getenv('CLUSTER_ID') %in% c("staging-bip-app", "prod-bip-app")) {
    suppressMessages(gcs_global_bucket(sub("/.*", "", file)))

    my_parse <- function(obj){
      tmp <- tempfile(fileext = ".xml")
      on.exit(unlink(tmp))
      suppressMessages(googleCloudStorageR::gcs_get_object(obj, saveToDisk = tmp))
      XML::xmlToDataFrame(tmp)
    }

    df <- my_parse(sub(paste0(".*", sub("/.*", "", file), "/"), "", file))
  }
  return(df)
}


#' Funksjon for å lagre .parquet-fil til Google Cloud Storage bucket
#'
#' Funksjonen `write_parquet` kan brukes til å skrive .parquet-filer til Google Cloud Storage bucket.
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
  arrow::write_parquet(data, gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)
}


#' Funksjon for å lagre "partitioned" .parquet-fil til Google Cloud Storage bucket
#'
#' Funksjonen `write_dataset` kan brukes til å skrive "partitioned" .parquet-filer til Google Cloud Storage bucket. "Partitioning" angis ut fra hvilke variabler datasettet er gruppert etter (gjøres via [dplyr::group_by()]).
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
  arrow::write_dataset(data, gcs_bucket(dirname(file))$path(paste0(basename(file))),
                       partitioning = dplyr::group_vars(data))
}



#' Funksjon for å lagre .feather-fil til Google Cloud Storage bucket
#'
#' Funksjonen `write_feather` kan brukes til å skrive .feather-filer til Google Cloud Storage bucket.
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
  arrow::write_feather(data, gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)

}



#' Funksjon for å lagre .csv-fil til Google Cloud Storage bucket
#'
#' Funksjonen `write_csv` kan brukes til å skrive .csv-filer til Google Cloud Storage bucket.
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
  arrow::write_csv_arrow(data, gcs_bucket(dirname(file))$path(paste0(basename(file))), ...)

}

#' Funksjon for å lagre .rds-fil til Google Cloud Storage bucket
#'
#' Funksjonen `write_rds` kan brukes til å skrive .rds-filer til Google Cloud Storage bucket.
#'
#' @param data Filen som skal skrives.
#' @param file Full filsti og filnavn for hvor filen skal skrives.
#'
#' @examples
#' \dontrun{
#' write_rds(data, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_rds_test.rds")
#' }
#'@encoding UTF-8

write_rds <- function(data,
                      file, ...) {

  f <- function(input, output){
    saveRDS(input, file = output)
  }

  gcs_global_bucket(sub("/.*", "", file))
  googleCloudStorageR::gcs_upload(data, name = sub(paste0(".*", sub("/.*", "", file), "/"), "", file), object_function = f)

}


#' Funksjon for å sjekke hvilke filer som finnes i en mappe i en Google Cloud Storage bucket
#'
#' Funksjonen `list.files` kan brukes til å sjekke hvilke filer som finnes i en Google Cloud Storage bucket
#'
#' @param bucket Full sti til Google Cloud Storage bucket (med eventuelle undermapper).
#'
#' @export
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

#' Funksjon for å sjekke hvilke filer som finnes i en Google Cloud Storage bucket
#'
#' Funksjonen `list.files` kan brukes til å sjekke hvilke filer som finnes i en Google Cloud Storage bucket. I tillegg ser man størrelsen til filene og tidspnktet filen sist ble endret.
#'
#' @param bucket Full sti til Google Cloud Storage bucket.
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
  if (dirname(bucket) == "."){
    gcs_global_bucket(bucket)
    googleCloudStorageR::gcs_list_objects(bucket)
  } else {
    gcs_global_bucket(dirname(bucket))
    googleCloudStorageR::gcs_list_objects(dirname(bucket))
  }
}


#' Funksjon for å slette fil fra en Google Cloud Storage bucket
#'
#' Funksjonen `gcs_delete_object` kan brukes til å slette filer fra Google Cloud Storage bucket.
#'
#' @param bucket Full sti til Google Cloud Storage bucket og filen som skal slettes.
#'
#' @examples
#' \dontrun{
#' gcs_delete_object("ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.parquet")
#' }
#'@encoding UTF-8
#'

gcs_delete_object <- function(file) {
  gcs_global_bucket(sub("/.*", "", file))
  googleCloudStorageR::gcs_delete_object(sub(paste0(".*", sub("/.*", "", file), "/"), "", file))
}


#' Funksjon for å lagre sf-objekt som .parquet-fil til Google Cloud Storage bucket
#'
#' Funksjonen `write_sf_parquet` kan brukes til å skrive sf-objekter som .parquet-filer til Google Cloud Storage bucket.
#'
#' @param data Filen som skal skrives.
#' @param file Full filsti og filnavn for hvor filen skal skrives.
#'
#' @examples
#' \dontrun{
#' write_sf_parquet(data, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_sf.parquet")
#' }
#'@encoding UTF-8
#'

write_sf_parquet <- function(data, file) {

  # Låner funksjoner fra sfarrow #
  create_metadata <- function(df){
    warning(strwrap("This is an initial implementation of Parquet/Feather file support
                  and geo metadata. This is tracking version 0.1.0 of the metadata
                  (https://github.com/geopandas/geo-arrow-spec). This metadata
                  specification may change and does not yet make stability promises.
                  We do not yet recommend using this in a production setting unless
                  you are able to rewrite your Parquet/Feather files.",
                    prefix = "\n", initial = ""
    ), call.=FALSE)

    # reference: https://github.com/geopandas/geo-arrow-spec
    geom_cols <- lapply(df, function(i) inherits(i, "sfc"))
    geom_cols <- names(which(geom_cols==TRUE))
    col_meta <- list()

    for(col in geom_cols){
      col_meta[[col]] <- list(crs = sf::st_crs(df[[col]])$wkt,
                              encoding = "WKB",
                              bbox = as.numeric(sf::st_bbox(df[[col]])))
    }

    geo_metadata <- list(primary_column = attr(df, "sf_column"),
                         columns = col_meta,
                         schema_version = "0.1.0",
                         creator = list(library="sfarrow"))

    return(jsonlite::toJSON(geo_metadata, auto_unbox=TRUE))
  }

  encode_wkb <- function(df){
    geom_cols <- lapply(df, function(i) inherits(i, "sfc"))
    geom_cols <- names(which(geom_cols==TRUE))

    df <- as.data.frame(df)

    for(col in geom_cols){
      obj_geo <- sf::st_as_binary(df[[col]])
      attr(obj_geo, "class") <- c("arrow_binary", "vctrs_vctr", attr(obj_geo, "class"), "list")
      df[[col]] <- obj_geo
    }
    return(df)
  }

  geo_metadata <- create_metadata(data)
  df <- encode_wkb(data)
  tbl <- arrow::Table$create(df)
  tbl$metadata[["geo"]] <- geo_metadata

  write_parquet(tbl, file)
}




#' Funksjon for å laste inn filer fra Google Cloud Storage bucket
#'
#' Funksjonen `read_SSB` kan brukes til å lese inn filer fra Google Cloud Storage. Funksjonen støtter .parquet- (inkludert sf-objekter), .feather-, .rds- og .csv-, .xml- og .json-filer.
#'
#' @param file Full sti og navn på filen som skal leses inn fra Google Cloud Storage bucket.
#' @param sf Boolsk. Standardverdi er FALSE. Sett `sf = TRUE` dersom .parquet-filen er et sf-objekt.
#' @param ... Flere parametere (se dokumentasjonen til: [fellesr::read_parquet()]/[fellesr::open_dataset()]/[fellesr::read_feather()]/[fellesr::read_csv()]/[fellesr::read_rds()]/[fellesr::read_parquet()])
#' 
#' @export
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
#' read_SSB_rds <- read_SSB("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.rds")
#'
#' read_SSB_json <- read_SSB("ssb-prod-spesh-personell-data-kilde/example_1.json")
#'
#' read_SSB_xml <- read_SSB("ssb-prod-spesh-personell-data-kilde/XXX.xml")
#'
#'}
#'@encoding UTF-8

read_SSB <- function(file, sf = FALSE, ...) {

  if(grepl("\\.parquet", basename(file)) & sf == FALSE){
    df <- read_parquet(file, ...)
  } else if(sf == TRUE){
    df <- open_dataset(file, ...) %>%
      sfarrow::read_sf_dataset()
  } else if(grepl("\\.feather", basename(file))){
    df <- read_feather(file, ...)
  } else if(grepl("\\.csv", basename(file)) | grepl(".txt", basename(file)) | grepl(".dat", basename(file))){
    df <- read_csv(file, ...)
  } else if(grepl("\\.json", basename(file))){
    df <- read_json(file, ...)
  } else if(grepl("\\.xml", basename(file))){
    df <- read_xml(file, ...)
  } else if(grepl("\\.rds", basename(file)) | grepl("\\.RDS", basename(file))) {
    df <- read_rds(file, ...)
  } else {
    df <- open_dataset(file, ...) %>%
      dplyr::collect()
  }
  return(df)

}


#' Funksjon for å skrive filer til Google Cloud Storage bucket
#'
#' Funksjonen `write_SSB` kan brukes til å skrive filer til Google Cloud Storage. Funksjonen støtter .parquet- (inkludert sf-objekter), .feather-, .rds- og .csv-filer.
#'
#' @param data Filen som skal skrives.
#' @param file Full filsti og filnavn for hvor filen skal skrives.
#' @param sf Boolsk. Standardverdi er FALSE. Sett `sf = TRUE` dersom filen som skal lagres er et sf-objekt.
#' @param ... Flere parametere (se dokumentasjonen til: [fellesr::write_parquet()]/[fellesr::write_sf_parquet()]/[fellesr::write_feather()]/[fellesr::write_csv()]/[fellesr::write_dataset()])
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' write_SSB(data, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.parquet")
#'
#' write_SSB(data, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_multifile_dataset_test", partitioning = TRUE)
#'
#' write_SSB(data, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.feather")
#'
#' write_SSB(data, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.csv")
#'
#' write_rds(data, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_rds_test.rds")
#'
#'
#'}
#'@encoding UTF-8

write_SSB <- function(data, file, partitioning = FALSE, sf = FALSE, ...) { # OBS: legge til mulighet for partitioning?
  if (grepl("\\.parquet", basename(file)) & sf == FALSE){
    write_parquet(data, file, ...)
  } else if (grepl("\\.parquet", basename(file)) & sf == TRUE){
    write_sf_parquet(data, file, ...)
  } else if (grepl("\\.feather", basename(file))){
    write_feather(data, file, ...)
  } else if (grepl("\\.csv", basename(file)) | grepl(".txt", basename(file)) | grepl(".dat", basename(file))){
    write_csv(data, file, ...)
  } else if (grepl("\\.rds", basename(file)) | grepl("\\.RDS", basename(file))){
    write_rds(data, file, ...)
  } else if (partitioning == TRUE) {
    write_dataset(data, file, ...)
  } else {
    print("write_SSB kan for øyeblikket kun skrive .parquet-, .feather-, .rds- og .csv-filer")
  }
}
