
#' Eksport av filer på Dapla og på bakken
#'
#' @inheritParams gcs_export
#'
#' @return
#'
#' På bakken: resultatet av eksporten som returnert av [rio::export].
#'
#' På Dapla: resultatet av eksporten som returnert av [googleCloudStorageR::gcs_upload]
#'
#' @details
#'
#' Se [rio::export] for tilgjengelige filformater. Funksjonen støtter de samme paramete
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' write_ssb(mtcars, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_xlsx_test.xlsx")
#'
#' write_ssb(mtcars, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_csv_test.csv")
#'
#' write_ssb(mtcars, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.parquet")
#'
#' write_ssb(mtcars, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_rds_test.rds")
#'
#' write_ssb(data = list(foo = "bar", egg = "ham"),
#'           file = "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_json_test.json")
#'
#' }
#'
write_ssb <- function(data, file, ...) {

  write_fun <- switch(Sys.getenv("DAPLA_REGION"),
                      BIP       = gcs_export,
                      DAPLA_LAB = gcs_export,
                      ON_PREM   = rio::export,
                      stop("Ukjent miljø, avbryter"))

  return(write_fun(data, file, ...))

}

#' @rdname write_ssb
#' @export
write_SSB <- write_ssb


#' Import av filer på Dapla og på bakken
#'
#' @inheritParams gcs_import
#' @inheritParams rio::import
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' read_ssb("ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_xlsx_test.xlsx")
#' read_ssb("ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_csv_test.csv")
#' read_ssb("ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.parquet")
#' read_ssb("ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_rds_test.rds")
#' read_ssb("ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_json_test.json")
#'
#'
#' }
#'
read_ssb <- function(file, ...) {

  read_fun <- switch(Sys.getenv("DAPLA_REGION"),
                     BIP       = gcs_import,
                     DAPLA_LAB = gcs_import,
                     ON_PREM   = rio::import,
                     stop("Ukjent miljø, avbryter"))

  return(read_fun(file, ...))

}

#' @rdname read_ssb
#' @export
read_SSB <- read_ssb





