# Konverter filsti mellom Parquet og Datadoc

Konverterer en filsti mellom en Parquet-fil og den tilhørende Datadoc
JSON-filen.

## Usage

``` r
datadoc_path(filsti, to = c("json", "parquet"), warn_if_missing = TRUE)
```

## Arguments

- filsti:

  En tekststreng eller karaktervektor med filstien til én eller flere
  Parquet- eller Datadoc-filer.

- to:

  En tekststreng som angir hvilket filformat filstien skal konverteres
  til. Gyldige verdier er `"json"` og `"parquet"`. Standardverdien er
  `"json"`.

- warn_if_missing:

  En logisk verdi som angir om det skal gis en advarsel dersom filen den
  konverterte filstien peker til, ikke finnes. Standardverdien er
  `TRUE`.

## Value

En tekststreng eller karaktervektor med den konverterte filstien.

## Details

Standardoppførselen er å konvertere fra `.parquet` til `__DOC.json`. Ved
å sette `to = "parquet"` konverteres en Datadoc-fil tilbake til den
tilhørende Parquet-filstien. Filstier som allerede har ønsket format,
returneres uendret. Ved `to = "json"` godtas også andre JSON-filnavn enn
`__DOC.json`.

## Examples

``` r
datadoc_path("/buckets/data/personell_v1.parquet")
#> Warning: Følgende filer finnes ikke:
#> /buckets/data/personell_v1__DOC.json
#> [1] "/buckets/data/personell_v1__DOC.json"

datadoc_path(
  "/buckets/data/personell_v1__DOC.json",
  to = "parquet"
)
#> Warning: Følgende filer finnes ikke:
#> /buckets/data/personell_v1.parquet
#> [1] "/buckets/data/personell_v1.parquet"
```
