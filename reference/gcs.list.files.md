# Funksjon for aa sjekke hvilke filer som finnes i en mappe i en Google Cloud Storage bucket

Funksjonen `gcs.list.files` kan brukes til aa sjekke hvilke filer som
finnes i en Google Cloud Storage bucket

## Usage

``` r
gcs.list.files(bucket)
```

## Arguments

- bucket:

  Full sti til Google Cloud Storage bucket (med eventuelle undermapper).

## Examples

``` r
if (FALSE) { # \dontrun{
gcs.list.files("ssb-prod-dapla-felles-data-delt/R_smoke_test")
} # }
```
