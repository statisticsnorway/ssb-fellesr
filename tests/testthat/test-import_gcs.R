
test_that("importering fungerer", {

  smoke_test <- "ssb-prod-dapla-felles-data-delt/R_smoke_test/"

  expect_no_error(
    suppressMessages({

      import_gcs(paste0(smoke_test, "write_SSB_xlsx_test.xlsx"))

      import_gcs(paste0(smoke_test, "write_SSB_csv_test.csv"))

      import_gcs(paste0(smoke_test, "write_SSB_parquet_test.parquet"))

      import_gcs(paste0(smoke_test, "write_SSB_rds_test.rds"))

      import_gcs(paste0(smoke_test, "write_SSB_json_test.json"))

    })
  )

})
