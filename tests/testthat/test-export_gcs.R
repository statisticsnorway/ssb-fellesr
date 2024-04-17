test_that("eksportering fungerer", {

  smoke_test <- "ssb-prod-dapla-felles-data-delt/R_smoke_test/"

  expect_no_error(
    suppressMessages({

      export_gcs(mtcars, paste0(smoke_test,
                               "export_gcs_xlsx_test.xlsx"))

      export_gcs(mtcars, paste0(smoke_test,
                               "export_gcs_csv_test.csv"))

      export_gcs(mtcars, paste0(smoke_test,
                               "export_gcs_parquet_test.parquet"))

      export_gcs(mtcars, paste0(smoke_test,
                               "export_gcs_rds_test.rds"))

      export_gcs(data = list(foo = "bar", egg = "ham"),
                file = paste0(smoke_test, "export_gcs_json_test.json"))

    })
  )

})




