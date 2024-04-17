test_that("eksportering fungerer", {

  smoke_test <- "ssb-prod-dapla-felles-data-delt/R_smoke_test/"

  expect_no_error(
    suppressMessages({

      write_ssb(mtcars, paste0(smoke_test,
                               "write_SSB_xlsx_test.xlsx"))

      write_ssb(mtcars, paste0(smoke_test,
                               "write_SSB_csv_test.csv"))

      write_ssb(mtcars, paste0(smoke_test,
                               "write_SSB_parquet_test.parquet"))

      write_ssb(mtcars, paste0(smoke_test,
                               "write_SSB_rds_test.rds"))

      write_ssb(data = list(foo = "bar", egg = "ham"),
                file = paste0(smoke_test, "write_SSB_json_test.json"))

      write_ssb(mtcars, paste0(smoke_test,
                               "write_SSB_feather_test.feather"))

    })
  )

})
