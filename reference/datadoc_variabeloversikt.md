# Lag oversikt over variabler i en Datadoc-fil

Leser variabelmetadata fra en Datadoc-fil og lager en oversikt over
variablene. Dersom en variabel har en referanse til en
variabeldefinisjon, hentes supplerende metadata fra Vardef.

## Usage

``` r
datadoc_variabeloversikt(
  filsti,
  language = "nb",
  include_vardef = TRUE,
  status = "auto"
)
```

## Arguments

- filsti:

  Filsti til en Datadoc-fil eller tilhørende Parquet-fil.

- language:

  Språkkode: "nb", "nn" eller "en". Standardverdien er "nb".

- include_vardef:

  Logisk verdi. Med FALSE leses bare lokale metadata uten oppslag i
  Vardef. Standardverdien er TRUE.

- status:

  "auto" (standard), "ekstern" eller "intern". Med "auto" søkes det
  først i ekstern Vardef, deretter intern Vardef for definisjoner som
  ikke finnes eksternt.

## Value

En data.frame med én rad per variabel og følgende kolonner: short_name,
name, data_type, classification_uri, vardef_definition_uri, vardef_name,
vardef_short_name, vardef_classification_uri, contains_data_from og
contains_data_until.

## Details

Vardef-oppslag gjøres etter definition_uri uten filtrering på
gyldighetsdato. Dataperioden i Datadoc påvirker dermed ikke hvilken
Vardef-definisjon som kan hentes.

contains_data_from og contains_data_until beholdes fra Datadoc og kan
brukes ved senere oppslag i KLASS.

Både classification_uri og classification_reference fra Vardef støttes.
KLASS-referanser returneres som KLASS-ID.
