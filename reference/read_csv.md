# Funksjon for aa laste inn .csv-fil fra Google Cloud Storage bucket

Funksjonen `read_csv` kan brukes til aa lese inn .csv-filer Google Cloud
Storage.

## Usage

``` r
read_csv(file, ...)
```

## Arguments

- file:

  Full sti og navn paa filen som skal leses inn fra Google Cloud Storage
  bucket.

- ...:

  Flere parametere (se:
  https://arrow.apache.org/docs/r/reference/read_delim_arrow.html)

## Examples

``` r
if (FALSE) { # \dontrun{
data <- read_csv("ssb-prod-dapla-felles-data-delt/R_smoke_test/1987.csv")
} # }
```
