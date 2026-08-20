# Kopier variabelmetadata mellom DataDoc-filer

Kopierer metadata for én eller flere variabler fra en original
DataDoc-fil til en annen DataDoc-fil.

## Usage

``` r
copy_metadata_variable(filsti_datadoc_egen, filsti_datadoc_original, variabler)
```

## Arguments

- filsti_datadoc_egen:

  En tekststreng med filstien til DataDoc-filen som skal oppdateres,
  eller til den tilhørende Parquet-filen. Filstien må slutte på `.json`
  eller `.parquet`.

- filsti_datadoc_original:

  En tekststreng med filstien til DataDoc-filen som metadataene skal
  kopieres fra, eller til den tilhørende Parquet-filen. Filstien må
  slutte på `.json` eller `.parquet`.

- variabler:

  En tekstvektor som angir hvilke variabler metadata skal kopieres for.
  En unavngitt verdi tolkes som at variabelen har samme `short_name` i
  begge filer. I en navngitt vektor angir navnet `short_name` i filen
  som skal oppdateres, mens verdien angir `short_name` i originalfilen.

## Value

Den oppdaterte DataDoc-strukturen som en liste. Den oppdaterte
strukturen skrives samtidig til DataDoc-filen som svarer til
`filsti_datadoc_egen`.

## Details

Filstiene kan oppgis enten som filstier til DataDoc-filer med
filendelsen `.json`, eller som filstier til Parquet-filer med
filendelsen `.parquet`. Parquet-filstier konverteres automatisk til
tilhørende DataDoc-filstier med [`datadoc_path()`](datadoc_path.md).

Hele metadataobjektet for hver valgt variabel erstattes. Variabelens
`short_name` beholdes imidlertid slik det er angitt i DataDoc-filen som
metadataene kopieres til.

Dersom en filsti slutter på `.parquet`, erstattes filendelsen med
`__DOC.json` ved hjelp av [`datadoc_path()`](datadoc_path.md). Filstier
som allerede slutter på `.json`, brukes uendret.

De endelige JSON-filstiene skrives ut før filene leses. Funksjonen
stopper med en feilmelding dersom én eller begge DataDoc-filene ikke
finnes. Alle manglende filer oppgis i samme feilmelding.

Begge DataDoc-filene leses med
[`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
med `simplifyVector = FALSE`.

For hver valgt variabel kopieres hele elementet fra `datadoc$variables`
i originalfilen til den tilsvarende variabelen i filen som skal
oppdateres. Feltet `short_name` erstattes deretter med variabelnavnet
som brukes i mottakerfilen.

Følgende kontroller utføres før filen endres:

- filstiene må være ikke-tomme tekststrenger som slutter på `.json`
  eller `.parquet`;

- begge DataDoc-filene må finnes;

- `variabler` må være en ikke-tom tekstvektor;

- samme variabel i mottakerfilen kan ikke oppgis flere ganger;

- `short_name` må være unik i begge DataDoc-filene; og

- alle oppgitte variabler må finnes i de respektive filene.

Den oppdaterte DataDoc-strukturen skrives tilbake med
[`jsonlite::write_json()`](https://jeroen.r-universe.dev/jsonlite/reference/read_json.html).
Den eksisterende DataDoc-filen som svarer til `filsti_datadoc_egen`,
overskrives.

## See also

[`datadoc_path()`](datadoc_path.md) for å opprette en DataDoc-filsti fra
en Parquet-filsti,
[`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
for å lese DataDoc-filene og
[`jsonlite::write_json()`](https://jeroen.r-universe.dev/jsonlite/reference/read_json.html)
for å skrive den oppdaterte filen.

## Examples

``` r
if (FALSE) { # \dontrun{
# Oppgi Parquet-filstier
copy_metadata_variable(
  filsti_datadoc_egen = "/buckets/data/egen_v1.parquet",
  filsti_datadoc_original = "/buckets/data/original_v1.parquet",
  variabler = c("kjoenn", "alder")
)

# Oppgi DataDoc-filstier
copy_metadata_variable(
  filsti_datadoc_egen = "/buckets/data/egen__DOC.json",
  filsti_datadoc_original = "/buckets/data/original__DOC.json",
  variabler = c(kjoenn = "sex")
)

# Kombiner Parquet- og DataDoc-filsti
copy_metadata_variable(
  filsti_datadoc_egen = "/buckets/data/egen.parquet",
  filsti_datadoc_original = "/buckets/data/original__DOC.json",
  variabler = c(
    kjoenn = "sex",
    "alder"
  )
)
} # }
```
