
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
#' @seealso [import_gcs()] og [rio::export()]
#'
#' @examples
#' \dontrun{
#'
#' export_gcs(mtcars, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_xlsx_test.xlsx")
#'
#' export_gcs(mtcars, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_csv_test.csv")
#'
#' export_gcs(mtcars, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.parquet")
#'
#' export_gcs(mtcars, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_rds_test.rds")
#'
#' export_gcs(data = list(foo = "bar", egg = "ham"),
#'            file = "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_json_test.json")
#'
#' }
#'
export_gcs <- function(data, file, ...) {

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
#' import_gcs("ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_xlsx_test.xlsx")
#' import_gcs("ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_csv_test.csv")
#' import_gcs("ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.parquet")
#' import_gcs("ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_rds_test.rds")
#' import_gcs("ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_json_test.json")
#'
#'
#' }
#'
import_gcs <- function(file, ...) {

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



#' Eksport av filer på Dapla og på bakken
#'
#' @inheritParams export_gcs
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
                      BIP       = export_gcs,
                      DAPLA_LAB = export_gcs,
                      ON_PREM   = rio::export,
                      stop("Ukjent miljø, avbryter"))

  return(write_fun(data, file, ...))

}

#' @rdname write_ssb
#' @export
write_SSB <- write_ssb


#' Import av filer på Dapla og på bakken
#'
#' @inheritParams import_gcs
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
                     BIP       = import_gcs,
                     DAPLA_LAB = import_gcs,
                     ON_PREM   = rio::import,
                     stop("Ukjent miljø, avbryter"))

  return(read_fun(file, ...))

}

#' @rdname read_ssb
#' @export
read_SSB <- read_ssb

