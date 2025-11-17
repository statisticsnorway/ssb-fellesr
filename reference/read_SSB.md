# Funksjon for aa laste inn filer fra Google Cloud Storage bucket

Funksjonen `read_SSB` kan brukes til aa lese inn filer fra Google Cloud
Storage. Funksjonen stoetter .parquet- (inkludert sf-objekter),
.feather-, .rds- og .csv-, .xml- og .json-filer.

## Usage

``` r
read_SSB(file, sf = FALSE, ...)
```

## Arguments

- file:

  Full sti og navn paa filen som skal leses inn fra Google Cloud Storage
  bucket.

- sf:

  Boolsk. Standardverdi er FALSE. Sett `sf = TRUE` dersom .parquet-filen
  er et sf-objekt.

- ...:

  Flere parametere (se dokumentasjonen til:
  [`read_parquet()`](read_parquet.md)/[`open_dataset()`](open_dataset.md)/[`read_feather()`](read_feather.md)/[`read_csv()`](read_csv.md)/[`read_rds()`](read_rds.md)/[`read_parquet()`](read_parquet.md))

## Examples

``` r
if (FALSE) { # \dontrun{
read_SSB_parquet <- read_SSB("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.parquet")

read_SSB_MFD <- read_SSB("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987_1996_dataset")

read_SSB_feather <- read_SSB("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.feather")

read_SSB_csv <- read_SSB("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.csv")

read_SSB_rds <- read_SSB("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.rds")

read_SSB_json <- read_SSB("ssb-prod-spesh-personell-data-kilde/example_1.json")

read_SSB_xml <- read_SSB("ssb-prod-spesh-personell-data-kilde/XXX.xml")

} # }
```
