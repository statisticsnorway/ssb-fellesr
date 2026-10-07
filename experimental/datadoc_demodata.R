
library(tidyverse)

# Lager demodata 
demodata <- klassR::get_klass(131, correspond = 104, date = "2026-01-01") |> 
  dplyr::rename(komm_nr = sourceCode,
                fylke_nr = targetCode) |> 
  dplyr::select(komm_nr, fylke_nr)

# 
filsti <- "/buckets/produkt/datadoc_data/klargjorte_data/person_inntekt_p2022-12-31_v1.parquet"

datadoc_variabeloversikt_df <- fellesr::datadoc_variabeloversikt(filsti)
DT::datatable(datadoc_variabeloversikt_df)

test <- arrow::read_parquet(filsti) |>
  fellesr::add_value_labels(filsti)

