# Oppdater variabelnavn og intervjumetadata i en Datadoc-fil

Henter variabelnavn fra en metadatatabell eller fra kolonneetiketter i
et datasett. Koblingen skjer på eksakt likhet med `short_name`.
Standardverdier for intervjuspørsmål fyller tomme felt for matchende
variabler. Utvalget kan avgrenses med `intervjuvariabler`.

## Usage

``` r
set_variable_names_from_labels(
  filsti,
  metadata,
  variabler = NULL,
  overwrite = FALSE,
  intervjuvariabler = variabler,
  unit_type = "20",
  temporality_type = "STATUS",
  data_source = "67",
  variable_role = "MEASURE",
  population_description = NULL,
  language = "nb",
  variabel_kolonne = "variabel",
  betydning_kolonne = "variable_text",
  clean_labels = TRUE,
  quiet = TRUE
)
```

## Arguments

- filsti:

  Filsti til Datadoc-filen (.json) eller til den tilhørende
  Parquet-filen. JSON-filen må allerede finnes.

- metadata:

  En metadatatabell med kolonnene angitt i `variabel_kolonne` og
  `betydning_kolonne`, filstien til en slik tabell i Parquet-format,
  eller et data.frame med kolonneetiketter i attributtet `label`.
  Gjentatte, identiske tekster for samme variabel er tillatt. Tomme
  etiketter brukes ikke som navn.

- variabler:

  Tekstvektor med kortnavn som skal oppdateres. NULL velger alle
  kortnavn som finnes i både kilden og Datadoc.

- overwrite:

  Logisk verdi. FALSE fyller bare manglende navn på valgt språk. TRUE
  tillater også å erstatte navnet på dette språket. Andre språk og
  utfylte fagfelt beholdes ved begge innstillinger.

- intervjuvariabler:

  Kortnavn på direkte intervjuspørsmål som skal få standardverdier.
  Standard er `variabler`. NULL velger alle variablene som matcher
  innenfor utvalget. Bruk
  [`character()`](https://rdrr.io/r/base/character.html) for bare navn.
  Alle intervjuvariabler må inngå i utvalget og finnes i begge kilder.

- unit_type:

  Enhetstype som tekstkode. Standard er "20" (person).

- temporality_type:

  Standard er "STATUS" for status på intervjutidspunktet. Andre
  muligheter er "ACCUMULATED", "FIXED" og "EVENT".

- data_source:

  Tekstkode fra KLASS 712. Standard er "67". Koden brukes direkte. NULL
  hopper over feltet. Verdien "SSB" støttes også og slår opp SSB i KLASS
  for datasettets sluttdato.

- variable_role:

  Standard er "MEASURE" for selve intervjusvaret.

- population_description:

  Eventuell dokumentert populasjonstekst for alle valgte
  intervjuspørsmål, på `language`. Standard NULL hopper over feltet.
  Funksjonen utleder ikke hvem som fikk spørsmålet. Manglende populasjon
  vil fortsatt kunne utløse valideringsadvarsler.

- language:

  Språk for navn og eventuell populasjonstekst. Standard er "nb".
  Tillatte verdier er "nb", "nn" og "en".

- variabel_kolonne:

  Kolonnen med kortnavn i en metadatatabell. Standard er `"variabel"`
  for bakoverkompatibilitet.

- betydning_kolonne:

  Kolonnen med spørsmålstekst. Standard er `"variable_text"`. Bruk
  `"sporsmaal_text"` for EHIS.

- clean_labels:

  Med `TRUE` (standard) renses etiketttekst for tagger.

- quiet:

  Med `TRUE` (standard) skjules statusmeldinger.

## Value

Det oppdaterte Datadoc-dokumentet, usynlig. Antall endrede variabler
meldes i konsollen. Ved null endringer skrives ikke filen.

## Details

Bruk NULL for et fagfelt som skal hoppes over. Send spørsmål med
forskjellige referanseperioder i separate kall. Funksjonen oppdaterer
variabelmetadata i JSON-filen. Øvrige deler av dokumentet beholdes, også
eventuell tidligere beregnet fullføringsprosent.

## Examples

``` r
if (FALSE) { # \dontrun{
# Fyll navn og tomme fagfelt for alle matchende variabler.
set_variable_names_from_labels(filsti, kodelister)
# Fyll bare navn.
set_variable_names_from_labels(
  filsti, kodelister, intervjuvariabler = character()
)
set_variable_names_from_labels(
  filsti, royk_med_labels,
  variabler = c("tob1", "tob2")
)
} # }
```
