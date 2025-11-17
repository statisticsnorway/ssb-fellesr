# Funksjon for aa laste inn .parquet-fil (i sf-format) fra Google Cloud Storage bucket

Funksjonen `read_parquet_sf` kan brukes til aa lese inn .parquet-filer
(i sf-format) fra Google Cloud Storage.

## Usage

``` r
read_parquet_sf(file, ...)
```

## Arguments

- file:

  Full sti og navn paa filen som skal leses inn fra Google Cloud Storage
  bucket.

- ...:

  Flere parametere (se:
  https://arrow.apache.org/docs/r/reference/read_parquet.html)

## Examples

``` r
if (FALSE) { # \dontrun{
data <- read_parquet_sf("ssb-prod-dapla-felles-data-delt/GIS/testdata/veger_oslo.parquet")
} # }
```
