
test_that("importering fungerer", {

  smoke_test <- "ssb-prod-dapla-felles-data-delt/R_smoke_test/"

  expect_no_error(
    suppressMessages({

      gcs_import(paste0(smoke_test, "write_SSB_xlsx_test.xlsx"))

      gcs_import(paste0(smoke_test, "write_SSB_csv_test.csv"))

      gcs_import(paste0(smoke_test, "write_SSB_parquet_test.parquet"))

      gcs_import(paste0(smoke_test, "write_SSB_rds_test.rds"))

      gcs_import(paste0(smoke_test, "write_SSB_json_test.json"))

    })
  )

})
