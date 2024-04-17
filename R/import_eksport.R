
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



