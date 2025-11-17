# Funksjon for aa laste inn .xml-fil fra Google Cloud Storage bucket

Funksjonen `read_xml` kan brukes til aa lese inn .xml-filer fra Google
Cloud Storage.

## Usage

``` r
read_xml(file, ...)
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
data <- read_xml("ssb-prod-dapla-felles-data-delt/R_smoke_test/XXX.xml")
} # }
```
