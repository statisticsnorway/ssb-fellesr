# Funksjon for aa lagre "partitioned" .parquet-fil til Google Cloud Storage bucket

Funksjonen `write_dataset` kan brukes til aa skrive "partitioned"
.parquet-filer til Google Cloud Storage bucket. "Partitioning" angis ut
fra hvilke variabler datasettet er gruppert etter (gjoeres via
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)).

## Usage

``` r
write_dataset(data, file, ...)
```

## Arguments

- data:

  Filen som skal skrives.

- file:

  Full filsti og filnavn for hvor filen skal skrives.

- ...:

  Flere parametere (se:
  https://arrow.apache.org/docs/r/reference/write_dataset.html)

## Examples

``` r
if (FALSE) { # \dontrun{
write_dataset(data, "ssb-prod-dapla-felles-data-delt/R_smoke_test/write_SSB_multifile_dataset_test")
} # }
```
