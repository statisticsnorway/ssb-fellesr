# Hent metadata for én variabel fra Datadoc

Leser en DataDoc-fil og henter det fullstendige metadataobjektet for en
bestemt variabel.

## Usage

``` r
metadata_variable(filsti, variabel)
```

## Arguments

- filsti:

  En tekststreng med filstien til Parquet-filen. Filstien til den
  tilhørende DataDoc-filen utledes med
  [`datadoc_path()`](datadoc_path.md).

- variabel:

  En tekststreng med kortnavnet til variabelen som metadata skal hentes
  for.

## Value

En liste med metadataene som er registrert for variabelen i
DataDoc-filen.

## Details

Funksjonen leser DataDoc-filen med
[`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
og søker etter en variabel der feltet `short_name` er identisk med
verdien i `variabel`.

Funksjonen stopper med en feilmelding dersom variabelen ikke finnes,
eller dersom flere variabler har samme kortnavn.

## See also

[`datadoc_path()`](datadoc_path.md) for å utlede filstien til
DataDoc-filen og
[`datadoc_variabeloversikt()`](datadoc_variabeloversikt.md) for å lage
en tabellarisk oversikt over alle variablene.

## Examples

``` r
if (FALSE) { # \dontrun{
metadata <- metadata_variable(
  filsti = "data/personell.parquet",
  variabel = "kjoenn"
)

metadata$name
metadata$classification_uri
} # }
```
