# Finn variabler med eller uten kodeliste

Henter en oversikt over variablene i en DataDoc-fil og filtrerer
variablene etter om de har en tilknyttet kodeliste.

## Usage

``` r
variables_with_classification_uri(
  filsti,
  with_codelist = TRUE,
  language = "nb"
)
```

## Arguments

- filsti:

  En tekststreng med filstien til en Parquet-fil. Filstien til den
  tilhørende DataDoc-filen utledes ved hjelp av
  [`datadoc_path()`](datadoc_path.md).

- with_codelist:

  En logisk verdi som angir hvilke variabler som skal returneres. Når
  verdien er `TRUE`, returneres variabler med kodeliste. Når verdien er
  `FALSE`, returneres variabler uten kodeliste. Standardverdien er
  `TRUE`.

- language:

  En tekststreng med språkkoden som skal brukes ved uthenting av
  språkavhengig metadata. Standardverdien er `"nb"`.

## Value

En `data.frame` med én rad per variabel som oppfyller det valgte
kriteriet. Kolonnene er de samme som i resultatet fra
[`datadoc_variabeloversikt()`](datadoc_variabeloversikt.md).

## Details

En variabel regnes som å ha en kodeliste dersom det finnes en
`classification_uri` enten direkte i DataDoc-filen eller i den
tilknyttede variabeldefinisjonen.

Funksjonen undersøker kolonnene `classification_uri` og
`vardef_classification_uri` i resultatet fra
[`datadoc_variabeloversikt()`](datadoc_variabeloversikt.md).

En variabel regnes som å ha en kodeliste dersom minst én av disse
kolonnene inneholder en verdi som ikke er tom eller manglende.

`with_codelist` må være én enkelt logisk verdi og kan ikke være `NA`.

## See also

[`datadoc_variabeloversikt()`](datadoc_variabeloversikt.md) for
oversikten som filtreres.

## Examples

``` r
if (FALSE) { # \dontrun{
# Hent variabler med kodeliste
variables_with_classification_uri(
  filsti = "/buckets/data/personell_v1.parquet"
)

# Hent variabler uten kodeliste
variables_with_classification_uri(
  filsti = "/buckets/data/personell_v1.parquet",
  with_codelist = FALSE
)

# Hent engelskspråklig metadata
variables_with_classification_uri(
  filsti = "/buckets/data/personell_v1.parquet",
  language = "en"
)
} # }
```
