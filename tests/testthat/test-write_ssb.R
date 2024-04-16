test_that("eksportering fungerer", {

  expect_no_error(write_ssb(mtcars, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_xlsx_test.xlsx"))

  expect_no_error(write_ssb(mtcars, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_csv_test.csv"))

  expect_no_error(write_ssb(mtcars, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.parquet"))

  expect_no_error(write_ssb(mtcars, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_rds_test.rds"))

  expect_no_error(
    write_ssb(data = list(foo = "bar", egg = "ham"),
               file = "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_json_test.json")
  )

})






