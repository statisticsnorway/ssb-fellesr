# Funksjon for aa lagre .csv-fil til Google Cloud Storage bucket

Funksjonen `write_csv` kan brukes til aa skrive .csv-filer til Google
Cloud Storage bucket.

## Usage

``` r
write_csv(data, file, ...)
```

## Arguments

- data:

  Filen som skal skrives.

- file:

  Full filsti og filnavn for hvor filen skal skrives.

- ...:

  Flere parametere (se:
  https://arrow.apache.org/docs/r/reference/read_delim_arrow.html)

## Examples

``` r
if (FALSE) { # \dontrun{
write_csv(data, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_test.csv")
} # }
```
