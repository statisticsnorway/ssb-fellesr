# Funksjon for aa laste inn .feather-fil fra Google Cloud Storage bucket

Funksjonen `read_feather` kan brukes til aa lese inn .feather-filer fra
Google Cloud Storage.

## Usage

``` r
read_feather(file, ...)
```

## Arguments

- file:

  Full sti og navn paa filen som skal leses inn fra Google Cloud Storage
  bucket.

- ...:

  Flere parametere (se:
  https://arrow.apache.org/docs/r/reference/read_feather.html)

## Examples

``` r
if (FALSE) { # \dontrun{
data <- read_feather("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.feather")
} # }
```
