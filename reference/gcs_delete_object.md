# Funksjon for aa slette fil fra en Google Cloud Storage bucket

Funksjonen `gcs_delete_object` kan brukes til aa slette filer fra Google
Cloud Storage bucket.

## Usage

``` r
gcs_delete_object(file)
```

## Arguments

- file:

  Full sti til Google Cloud Storage bucket og filen som skal slettes.

## Examples

``` r
if (FALSE) { # \dontrun{
gcs_delete_object("ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.parquet")
} # }
```
