# Lag filsti til Datadoc-fil

Oppretter filstien til en Datadoc-fil basert på filstien til en
Parquet-fil. Filendelsen `.parquet` erstattes med `__DOC.json`.

## Usage

``` r
datadoc_path(filsti)
```

## Arguments

- filsti:

  En tekststreng eller tegnvektor med filstien til én eller flere
  Parquet-filer.

## Value

En tekststreng eller tegnvektor med filstien til den tilhørende
DataDoc-filen.

## Examples

``` r
datadoc_path("/buckets/data/personell_v1.parquet")
#> [1] "/buckets/data/personell_v1__DOC.json"

datadoc_path(
  c(
    "/buckets/data/personell_v1.parquet",
    "/buckets/data/regnskap_v1.parquet"
  )
)
#> [1] "/buckets/data/personell_v1__DOC.json"
#> [2] "/buckets/data/regnskap_v1__DOC.json" 
```
