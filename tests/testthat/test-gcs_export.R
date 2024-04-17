test_that("eksportering fungerer", {

  smoke_test <- "ssb-prod-dapla-felles-data-delt/R_smoke_test/"

  expect_no_error(
    suppressMessages({

      gcs_export(mtcars, paste0(smoke_test,
                               "gcs_export_xlsx_test.xlsx"))

      gcs_export(mtcars, paste0(smoke_test,
                               "gcs_export_csv_test.csv"))

      gcs_export(mtcars, paste0(smoke_test,
                               "gcs_export_parquet_test.parquet"))

      gcs_export(mtcars, paste0(smoke_test,
                               "gcs_export_rds_test.rds"))

      gcs_export(data = list(foo = "bar", egg = "ham"),
                file = paste0(smoke_test, "gcs_export_json_test.json"))

    })
  )

})




