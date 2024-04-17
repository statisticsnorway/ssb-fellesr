test_that("importering fungerer", {

  smoke_test <- "ssb-prod-dapla-felles-data-delt/R_smoke_test/"

  expect_no_error(
    suppressMessages({

      read_ssb(paste0(smoke_test, "write_SSB_xlsx_test.xlsx"))

      read_ssb(paste0(smoke_test, "write_SSB_csv_test.csv"))

      read_ssb(paste0(smoke_test, "write_SSB_parquet_test.parquet"))

      read_ssb(paste0(smoke_test, "write_SSB_rds_test.rds"))

      read_ssb(paste0(smoke_test, "write_SSB_json_test.json"))

      read_ssb(paste0(smoke_test,
                      "write_SSB_feather_test.feather"))

    })
  )

})
