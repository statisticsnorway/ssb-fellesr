# Funksjon for aa lagre .parquet-fil til Google Cloud Storage bucket

Funksjonen `write_parquet` kan brukes til aa skrive .parquet-filer til
Google Cloud Storage bucket.

## Usage

``` r
write_parquet(data, file, ...)
```

## Arguments

- data:

  Filen som skal skrives.

- file:

  Full filsti og filnavn for hvor filen skal skrives.

- ...:

  Flere parametere (se:
  https://arrow.apache.org/docs/r/reference/write_parquet.html)

## Examples

``` r
if (FALSE) { # \dontrun{
write_parquet(data, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.parquet")
} # }
```
