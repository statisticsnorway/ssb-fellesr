# Funksjon for aa lagre sf-objekt som .parquet-fil til Google Cloud Storage bucket

Funksjonen `write_sf_parquet` kan brukes til aa skrive sf-objekter som
.parquet-filer til Google Cloud Storage bucket.

## Usage

``` r
write_sf_parquet(data, file, ...)
```

## Arguments

- data:

  Filen som skal skrives.

- file:

  Full filsti og filnavn for hvor filen skal skrives.

- ...:

  Flere parametere (se dokumentasjonen til:
  [`write_parquet()`](write_parquet.md)/`write_sf_parquet()`/[`write_feather()`](write_feather.md)/[`write_csv()`](write_csv.md)/[`write_dataset()`](write_dataset.md))

## Examples

``` r
if (FALSE) { # \dontrun{
write_sf_parquet(data, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_parquet_sf.parquet")
} # }
```
