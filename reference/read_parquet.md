# Funksjon for aa laste inn .parquet-fil fra Google Cloud Storage bucket

Funksjonen `read_parquet` kan brukes til aa lese inn .parquet-filer fra
Google Cloud Storage.

## Usage

``` r
read_parquet(file, ...)
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
data <- read_parquet("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.parquet")
} # }
```
