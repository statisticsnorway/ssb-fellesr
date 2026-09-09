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
  ekskluder_variabler = NULL,
  overwrite = TRUE,
  dato_unntak = NULL
)
```

## Arguments

- filsti_datadoc_egen:

  En tekststreng med filstien til Datadoc-filen som skal oppdateres.
  Filnavnet må inneholde periode og versjon på formen `_pYYYY_vN`, for
  eksempel `_p2025_v1__DOC.json`.

- filsti_datadoc_original:

  En tekststreng med filstien til Datadoc-filen som metadata skal
  kopieres fra.

- variabler:

  `NULL` eller en navngitt tekstvektor med eksplisitte koblinger mellom
  variabler. Navnet på hvert element angir `short_name` i filen som skal
  oppdateres, mens verdien angir `short_name` i originalfilen.
  Standardverdien er `NULL`.

- ekskluder_variabler:

  `NULL` eller en tekstvektor med `short_name` for variabler i
  mottakerfilen som ikke skal endres. For disse variablene kopieres
  verken metadata eller gyldighetsperiode. En variabel kan ikke samtidig
  være oppgitt i `dato_unntak`. Standardverdien er `NULL`.

- overwrite:

  En logisk verdi som angir om eksisterende metadata skal erstattes. Når
  verdien er `TRUE`, kopieres metadata for alle aktuelle variabler. Når
  verdien er `FALSE`, hoppes variabler over dersom feltet `name` i
  mottakerfilen ikke er `NULL`. Standardverdien er `TRUE`.

- dato_unntak:

  `NULL` eller en data frame med egne gyldighetsperioder for
  enkeltvariabler. Metadata kopieres på vanlig måte for disse
  variablene, men feltene `contains_data_from` og `contains_data_until`
  overskrives med periodene angitt i `dato_unntak`. Dataframen må
  inneholde kolonnene `short_name`, `contains_data_from` og
  `contains_data_until`. Datoene skal være på formatet `"YYYY-MM-DD"`.
  En variabel kan ikke samtidig være oppgitt i `ekskluder_variabler`.
  Standardverdien er `NULL`.

## Value

Den oppdaterte Datadoc-strukturen som en liste. Strukturen skrives
samtidig tilbake til filen angitt i `filsti_datadoc_egen`.

## Details

Variabler kan ekskluderes fullstendig fra oppdateringen ved hjelp av
`ekskluder_variabler`. For disse variablene kopieres verken metadata
eller gyldighetsperiode.

Gyldighetsperioden for øvrige variabler i mottakerfilen settes
automatisk ut fra perioden i filnavnet. For eksempel gir `_p2025_v1`
perioden `2025-01-01` til `2025-12-31`. Det kan angis egne
gyldighetsperioder for enkeltvariabler ved hjelp av `dato_unntak`.

Funksjonen finner først alle variabler som har samme `short_name` i de
to Datadoc-filene. Metadata for disse variablene kopieres automatisk.

Argumentet `variabler` kan brukes til å koble variabler som har ulike
kortnavn i de to filene. En kobling som er angitt eksplisitt i
`variabler`, får forrang dersom mottakervariabelen også inngår blant
variablene med identiske kortnavn.

Variabler som er oppgitt i `ekskluder_variabler`, fjernes fra listen
over variabler som skal få kopiert metadata. De hoppes også over når
gyldighetsperioden oppdateres. Disse variablene beholdes derfor uendret
i mottakerfilen.

For hver øvrige variabel kopieres hele metadataobjektet fra
originalfilen. Feltet `short_name` erstattes deretter med kortnavnet som
brukes i filen som oppdateres.

Når `overwrite = FALSE`, regnes en variabel som å ha eksisterende
metadata dersom feltet `name` ikke er `NULL`. Andre metadatafelt tas
ikke med i denne vurderingen.

Gyldighetsperioden oppdateres uavhengig av `overwrite`. Dette innebærer
at også variabler som ikke får kopiert metadata fordi de allerede har
metadata, får oppdatert feltene `contains_data_from` og
`contains_data_until`.

Variabler i `ekskluder_variabler` er unntatt fra denne oppdateringen og
beholdes fullstendig uendret.

Standardperioden hentes fra filnavnet til `filsti_datadoc_egen`. Et
filnavn som inneholder `_p2025_v1` gir
`contains_data_from = "2025-01-01"` og
`contains_data_until = "2025-12-31"`.

Dersom enkelte variabler har en annen gyldighetsperiode, kan disse
oppgis i `dato_unntak`. Metadata kopieres på vanlig måte, men perioden
som er angitt i `dato_unntak`, erstatter standardperioden for de
aktuelle variablene.

Samme variabel kan ikke være oppgitt både i `ekskluder_variabler` og
`dato_unntak`.

Følgende kontroller utføres før filen endres:

- `overwrite` må være én enkelt logisk verdi;

- `short_name` må være unik i begge Datadoc-filene;

- `variabler` må være en navngitt tekstvektor dersom argumentet ikke er
  `NULL`;

- samme mottakervariabel kan ikke oppgis flere ganger i `variabler`;

- alle eksplisitt oppgitte variabler må finnes i de respektive
  Datadoc-filene;

- `ekskluder_variabler` må være en tekstvektor dersom argumentet ikke er
  `NULL`;

- alle variabler i `ekskluder_variabler` må finnes i mottakerfilen;

- filnavnet til mottakerfilen må inneholde en periode på formen
  `_pYYYY_vN`;

- `dato_unntak` må inneholde de nødvendige kolonnene;

- hver variabel kan bare forekomme én gang i `dato_unntak`;

- alle variabler i `dato_unntak` må finnes i mottakerfilen;

- samme variabel kan ikke forekomme både i `ekskluder_variabler` og
  `dato_unntak`; og

- datoene i `dato_unntak` må være gyldige datoer på formatet
  `"YYYY-MM-DD"`.

Den oppdaterte strukturen skrives til `filsti_datadoc_egen` med
[`jsonlite::write_json()`](https://jeroen.r-universe.dev/jsonlite/reference/read_json.html).
Den eksisterende filen overskrives.

## See also

[`copy_metadata_variable()`](copy_metadata_variable.md) for å kopiere
metadata bare for eksplisitt angitte variabler.

## Examples

``` r
if (FALSE) { # \dontrun{
# Kopier metadata og sett perioden til 2025 for alle aktuelle variabler
copy_metadata(
  filsti_datadoc_egen =
    "/buckets/data/resultat_p2025_v1__DOC.json",
  filsti_datadoc_original =
    "/buckets/data/resultat_p2024_v1__DOC.json"
)

# Kopier bare til variabler som ikke allerede har metadata
copy_metadata(
  filsti_datadoc_egen =
    "/buckets/data/resultat_p2025_v1__DOC.json",
  filsti_datadoc_original =
    "/buckets/data/resultat_p2024_v1__DOC.json",
  overwrite = FALSE
)

# Legg til eksplisitte koblinger mellom ulike kortnavn
copy_metadata(
  filsti_datadoc_egen =
    "/buckets/data/resultat_p2025_v1__DOC.json",
  filsti_datadoc_original =
    "/buckets/data/resultat_p2024_v1__DOC.json",
  variabler = c(
    kjoenn = "sex",
    bostedskommune = "kommune"
  )
)

# Ikke gjør noen endringer i enkelte variabler
copy_metadata(
  filsti_datadoc_egen =
    "/buckets/data/resultat_p2025_v1__DOC.json",
  filsti_datadoc_original =
    "/buckets/data/resultat_p2024_v1__DOC.json",
  ekskluder_variabler = c(
    "orgnr_frtk",
    "navn_frtk"
  )
)

# Angi egne gyldighetsperioder for enkelte variabler
dato_unntak <- data.frame(
  short_name = c(
    "orgnr_frtk",
    "navn_frtk"
  ),
  contains_data_from = c(
    "2024-01-01",
    "2020-01-01"
  ),
  contains_data_until = c(
    "2025-12-31",
    "2025-12-31"
  )
)

copy_metadata(
  filsti_datadoc_egen =
    "/buckets/data/resultat_p2025_v1__DOC.json",
  filsti_datadoc_original =
    "/buckets/data/resultat_p2024_v1__DOC.json",
  dato_unntak = dato_unntak
)
} # }
```
