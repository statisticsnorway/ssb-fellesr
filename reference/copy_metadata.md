# Kopier metadata mellom Datadoc-filer

Kopierer variabelmetadata fra en original Datadoc-fil til en annen
Datadoc-fil. Metadata kopieres automatisk for variabler som har samme
`short_name` i begge filer. Det kan i tillegg angis eksplisitte
koblinger mellom variabler med ulike kortnavn.

## Usage

``` r
copy_metadata(
  filsti_datadoc_egen,
  filsti_datadoc_original,
  variabler = NULL,
  overwrite = TRUE
)
```

## Arguments

- filsti_datadoc_egen:

  En tekststreng med filstien til DataDoc-filen som skal oppdateres.

- filsti_datadoc_original:

  En tekststreng med filstien til DataDoc-filen som metadata skal
  kopieres fra.

- variabler:

  `NULL` eller en navngitt tekstvektor med eksplisitte koblinger mellom
  variabler. Navnet på hvert element angir `short_name` i filen som skal
  oppdateres, mens verdien angir `short_name` i originalfilen.
  Standardverdien er `NULL`.

- overwrite:

  En logisk verdi som angir om eksisterende metadata skal erstattes. Når
  verdien er `TRUE`, kopieres metadata for alle aktuelle variabler. Når
  verdien er `FALSE`, hoppes variabler over dersom feltet `name` i
  mottakerfilen ikke er `NULL`. Standardverdien er `TRUE`.

## Value

Den oppdaterte Datadoc-strukturen som en liste. Strukturen skrives
samtidig tilbake til filen angitt i `filsti_datadoc_egen`.

## Details

Funksjonen finner først alle variabler som har samme `short_name` i de
to Datadoc-filene. Metadata for disse variablene kopieres automatisk.

Argumentet `variabler` kan brukes til å koble variabler som har ulike
kortnavn i de to filene. En kobling som er angitt eksplisitt i
`variabler`, får forrang dersom mottakervariabelen også inngår blant
variablene med identiske kortnavn.

For hver variabel kopieres hele metadataobjektet fra originalfilen.
Feltet `short_name` erstattes deretter med kortnavnet som brukes i filen
som oppdateres.

Når `overwrite = FALSE`, regnes en variabel som å ha eksisterende
metadata dersom feltet `name` ikke er `NULL`. Andre metadatafelt tas
ikke med i denne vurderingen.

Følgende kontroller utføres før filen endres:

- `overwrite` må være én enkelt logisk verdi;

- `short_name` må være unik i begge Datadoc-filene;

- `variabler` må være en navngitt tekstvektor dersom argumentet ikke er
  `NULL`;

- samme mottakervariabel kan ikke oppgis flere ganger; og

- alle eksplisitt oppgitte variabler må finnes i de respektive
  DataDoc-filene.

Den oppdaterte strukturen skrives til `filsti_datadoc_egen` med
[`jsonlite::write_json()`](https://jeroen.r-universe.dev/jsonlite/reference/read_json.html).
Den eksisterende filen overskrives.

## See also

[`copy_metadata_variable()`](copy_metadata_variable.md) for å kopiere
metadata bare for eksplisitt angitte variabler.

## Examples

``` r
if (FALSE) { # \dontrun{
# Kopier metadata for alle variabler med samme short_name
copy_metadata(
  filsti_datadoc_egen = "/buckets/data/egen__DOC.json",
  filsti_datadoc_original = "/buckets/data/original__DOC.json"
)

# Legg også til en eksplisitt kobling mellom ulike kortnavn
copy_metadata(
  filsti_datadoc_egen = "/buckets/data/egen__DOC.json",
  filsti_datadoc_original = "/buckets/data/original__DOC.json",
  variabler = c(
    kjoenn = "sex",
    bostedskommune = "kommune"
  )
)

# Kopier bare til variabler som ikke allerede har metadata
copy_metadata(
  filsti_datadoc_egen = "/buckets/data/egen__DOC.json",
  filsti_datadoc_original = "/buckets/data/original__DOC.json",
  overwrite = FALSE
)
} # }
```
