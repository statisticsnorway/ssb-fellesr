# Funksjon for aa laste inn .rds-fil fra Google Cloud Storage bucket

Funksjonen `read_rds` kan brukes til aa lese inn .rds-filer fra Google
Cloud Storage.

## Usage

``` r
read_rds(file, ...)
```

## Arguments

- file:

  Full sti og navn paa filen som skal leses inn fra Google Cloud Storage
  bucket.

- ...:

  Flere parametere (se:
  https://rdrr.io/cran/googleCloudStorageR/man/gcs_get_object.html)

## Examples

``` r
if (FALSE) { # \dontrun{
data <- read_rds("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.rds")
} # }
```
