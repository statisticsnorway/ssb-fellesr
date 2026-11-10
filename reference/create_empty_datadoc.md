# Opprett et tomt Datadoc-dokument

Oppretter et nytt Datadoc-metadatadokument for en Parquet-fil.
Dokumentet opprettes på tilsvarende måte som når en udokumentert
Parquet-fil åpnes og lagres i Datadoc Editor uten at metadata fylles ut
manuelt. Valgfrie argumenter fyller ut tekstfelter ved opprettelsen.

## Usage

``` r
create_empty_datadoc(
  filsti,
  population_description = NULL,
  name = NULL,
  description = NULL,
  version_description = NULL,
  language_code = "nb",
  quiet = FALSE
)
```

## Arguments

- filsti:

  En tekststreng med full filsti til en Parquet-fil. Filen må ligge
  under en produktsti på formen `"/buckets/produkt/<statistikk>/..."`,
  der `<statistikk>` er statistikkens kortnavn.

- population_description:

  En tekststreng som beskriver populasjonen. Samme tekst legges i
  `population_description` på datasettnivå og for alle variabler.
  Standard er `NULL`, som lar feltene være uendret.

- name:

  En tekststreng med datasettets navn. Gjelder bare datasettnivå.
  Standard er `NULL`.

- description:

  En tekststreng med beskrivelse av datasettet. Gjelder bare
  datasettnivå. Standard er `NULL`.

- version_description:

  En tekststreng med beskrivelse av denne versjonen av datasettet.
  Gjelder bare datasettnivå. Standard er `NULL`.

- language_code:

  En tekststreng med språkkoden for tekstfeltene, for eksempel `"nb"`,
  `"nn"` eller `"en"`. Standard er `"nb"`.

- quiet:

  Logisk verdi. Standard er `FALSE`. Med `TRUE` skjules meldinger,
  advarsler og konsollutskrifter fra R og Python under kjøringen. Feil
  som stopper kjøringen, vises fortsatt.

## Value

Usynlig (`invisible`) tekststreng med filstien til det opprettede
Datadoc-dokumentet.

## Details

Funksjonen bruker Python-pakken `dapla-toolbelt-metadata` via
`reticulate` til å opprette Datadoc-dokumentet. Metadata som kan utledes
automatisk fra Parquet-filen og filstien, som variabelnavn, datatyper,
filnavn og datatilstand, fylles ut automatisk.

Statistikkens kortnavn hentes fra filstien. For eksempel gir
`"/buckets/produkt/speshelse/..."` kortnavnet `"speshelse"`.
Statistikkområdet (`subject_field`) hentes fra SSBs emnestruktur basert
på statistikkens primære plassering.

Feltene `metadata_created_by` og `metadata_last_updated_by` settes fra
miljøvariabelen `GIT_USER_MAIL`.

Metadatadokumentet lagres i samme mappe som Parquet-filen og får samme
filnavn med suffikset `"__DOC.json"`. For eksempel vil
`"data_p2025_v1.parquet"` gi `"data_p2025_v1__DOC.json"`.

Funksjonen stopper dersom inputfilen ikke er en Parquet-fil, dersom
statistikkens kortnavn ikke kan utledes fra filstien, eller dersom
Datadoc-filen allerede finnes.

Det gis en advarsel dersom statistikkområdet ikke kan bestemmes, eller
dersom miljøvariabelen `GIT_USER_MAIL` ikke er tilgjengelig.

Tekstfeltene lagres som språkposter med `languageCode` og
`languageText`. En eksisterende tekst på valgt språk erstattes når
argumentet er angitt; eventuelle andre språk beholdes. `NULL` betyr at
feltet beholdes slik Python-pakken opprettet det. Angitte tekster må
være én ikke-tom tekststreng og kan ikke være `NA`.

Populasjonsteksten kopieres til alle variabler, også administrative og
avledede variabler. Den beskriver en felles populasjon; eventuell
spørsmålsruting eller andre avgrensninger må dokumenteres separat.

Tekstfeltene fylles ut etter at Python-pakken har skrevet dokumentet.
Pakken kan derfor varsle om manglende obligatoriske metadata før disse
feltene fylles ut. Andre obligatoriske felter kan fortsatt mangle etter
opprettelsen.

## Examples

``` r
if (FALSE) { # \dontrun{
create_empty_datadoc(
  "/buckets/produkt/speshelse/klargjorte-data/data_p2025_v1.parquet"
)

create_empty_datadoc(
  filsti = "/buckets/produkt/royk/klargjorte-data/royk_p2026.parquet",
  population_description = "Beskriv undersøkelsens populasjon her.",
  name = "Røyk, alkohol og andre rusmidler 2026",
  description = "Klargjorte data fra undersøkelsen i 2026.",
  version_description = "Første klargjorte versjon.",
  language_code = "nb",
  quiet = TRUE
)
} # }
```
