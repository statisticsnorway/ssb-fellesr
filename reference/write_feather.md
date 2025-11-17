# Funksjon for aa lagre .feather-fil til Google Cloud Storage bucket

Funksjonen `write_feather` kan brukes til aa skrive .feather-filer til
Google Cloud Storage bucket.

## Usage

``` r
write_feather(data, file, ...)
```

## Arguments

- data:

  Filen som skal skrives.

- file:

  Full filsti og filnavn for hvor filen skal skrives.

- ...:

  Flere parametere (se:
  https://arrow.apache.org/docs/r/reference/write_feather.html)

## Examples

``` r
if (FALSE) { # \dontrun{
write_feather(data, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.feather")
} # }
```
