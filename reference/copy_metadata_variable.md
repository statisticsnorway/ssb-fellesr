# Kopier variabelmetadata mellom Datadoc-filer

Kopierer metadata for én eller flere variabler fra en original
Datadoc-fil til en annen Datadoc-fil.

## Usage

``` r
copy_metadata_variable(
  filsti_egen,
  filsti_original,
  variabler,
  overwrite = TRUE,
  quiet = TRUE
)
```

## Arguments

- filsti_egen:

  En tekststreng med filstien til Datadoc-filen som skal oppdateres,
  eller til den tilhørende Parquet-filen. Filstien må slutte på `.json`
  eller `.parquet`.

- filsti_original:

  En tekststreng med filstien til Datadoc-filen som metadataene skal
  kopieres fra, eller til den tilhørende Parquet-filen. Filstien må
  slutte på `.json` eller `.parquet`.

- variabler:

  En karaktervektor som angir hvilke variabler metadata skal kopieres
  for. En unavngitt verdi tolkes som at variabelen har samme
  `short_name` i begge filer. I en navngitt vektor angir navnet
  `short_name` i filen som skal oppdateres, mens verdien angir
  `short_name` i originalfilen.

- overwrite:

  Med `TRUE` (standard) erstattes dokumentasjonsfeltene. Med `FALSE`
  hoppes hele variabelen over når `name` ikke er `NULL`.

- quiet:

  Med `TRUE` (standard) skjules statusmeldinger.

## Value

Den oppdaterte Datadoc-strukturen som en liste. Den oppdaterte
strukturen skrives samtidig til Datadoc-filen som svarer til
`filsti_egen`.

## Details

Filstiene kan oppgis enten som filstier til Datadoc-filer med
filendelsen `.json`, eller som filstier til Parquet-filer med
filendelsen `.parquet`. Parquet-filstier konverteres automatisk til
tilhørende Datadoc-filstier med [`datadoc_path()`](datadoc_path.md).

Dokumentasjonsfeltene for hver valgt variabel kopieres. Mottakerens
`short_name`, ID, datatype, dataelementsti og pseudonymiseringsmetadata
beholdes. Ukjente felt beholdes fra mottakeren.

Dersom en filsti slutter på `.parquet`, erstattes filendelsen med
`__DOC.json` ved hjelp av [`datadoc_path()`](datadoc_path.md). Filstier
som allerede slutter på `.json`, brukes uendret.

Med `quiet = FALSE` skrives JSON-filstiene ut før filene leses.
Funksjonen stopper med en feilmelding dersom én eller begge
Datadoc-filene ikke finnes. Alle manglende filer oppgis i samme
feilmelding.

Begge Datadoc-filene leses med
[`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
med `simplifyVector = FALSE`.

Kjente dokumentasjonsfelt kopieres samlet for hver valgt variabel.
`overwrite = FALSE` gjør at hele variabelen hoppes over når mottakerens
`name` ikke er `NULL`. Andre felt brukes ikke til å avgjøre dette. Med
`overwrite = TRUE` erstattes dokumentasjonsfeltene som finnes i kilden,
også når kildeverdien er `NULL`. Felt som ikke finnes i kilden, beholdes
fra mottakeren. Tekniske felt overskrives aldri.

Følgende kontroller utføres før filen endres:

- filstiene må være ikke-tomme tekststrenger som slutter på `.json`
  eller `.parquet`;

- begge Datadoc-filene må finnes;

- `variabler` må være en ikke-tom karaktervektor;

- samme variabel i mottakerfilen kan ikke oppgis flere ganger;

- `short_name` må være unik i begge Datadoc-filene; og

- alle oppgitte variabler må finnes i de respektive filene.

Den oppdaterte Datadoc-strukturen skrives tilbake med
[`jsonlite::write_json()`](https://jeroen.r-universe.dev/jsonlite/reference/read_json.html).
Den eksisterende Datadoc-filen som svarer til `filsti_egen`,
overskrives.

## See also

[`datadoc_path()`](datadoc_path.md) for å opprette en Datadoc-filsti fra
en Parquet-filsti,
[`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
for å lese Datadoc-filene og
[`jsonlite::write_json()`](https://jeroen.r-universe.dev/jsonlite/reference/read_json.html)
for å skrive den oppdaterte filen.

## Examples

``` r
if (FALSE) { # \dontrun{
# Oppgi Parquet-filstier
copy_metadata_variable(
  filsti_egen = "/buckets/data/egen_v1.parquet",
  filsti_original = "/buckets/data/original_v1.parquet",
  variabler = c("kjoenn", "alder")
)

# Oppgi Datadoc-filstier
copy_metadata_variable(
  filsti_egen = "/buckets/data/egen__DOC.json",
  filsti_original = "/buckets/data/original__DOC.json",
  variabler = c(kjoenn = "sex")
)

# Kombiner Parquet- og Datadoc-filsti
copy_metadata_variable(
  filsti_egen = "/buckets/data/egen.parquet",
  filsti_original = "/buckets/data/original__DOC.json",
  variabler = c(
    kjoenn = "sex",
    "alder"
  )
)
} # }
```
