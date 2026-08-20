# Lag oversikt over variabler i en Datadoc-fil

Leser variabelmetadata fra en Datadoc-fil og lager en oversikt over
variablene. Dersom en variabel har en referanse til en
variabeldefinisjon, hentes supplerende metadata fra SSBs tjeneste for
variabeldefinisjoner.

## Usage

``` r
datadoc_variabeloversikt(filsti, language = "nb")
```

## Arguments

- filsti:

  En tekststreng med filstien til en Parquet-fil. Filstien til den
  tilhørende Datadoc-filen utledes ved hjelp av
  [`datadoc_path()`](datadoc_path.md).

- language:

  En tekststreng med språkkoden som skal brukes ved uthenting av navn og
  annen språkavhengig metadata. Standardverdien er `"nb"`.

## Value

En `data.frame` med én rad per variabel og følgende kolonner:

- `short_name`:

  Variabelens kortnavn fra Datadoc-filen.

- `name`:

  Variabelens navn på det valgte språket.

- `data_type`:

  Variabelens datatype.

- `classification_uri`:

  KLASS-ID hentet fra variabelens `classification_uri` i DataDoc-filen.

- `vardef_definition_uri`:

  ID-en til variabeldefinisjonen som ble hentet fra tjenesten for
  variabeldefinisjoner.

- `vardef_name`:

  Navnet fra variabeldefinisjonen på det valgte språket.

- `vardef_short_name`:

  Kortnavnet fra variabeldefinisjonen.

- `vardef_classification_uri`:

  KLASS-ID hentet fra `classification_uri` i variabeldefinisjonen.

- `contains_data_from`:

  Startdatoen for perioden variabelen inneholder data for.

- `contains_data_until`:

  Sluttdatoen for perioden variabelen inneholder data for.

## Details

Funksjonen forventer at Datadoc-filen har samme filsti som
Parquet-filen, men med filendelsen `.parquet` erstattet med
`__DOC.json`.

Språkavhengige felt kan være lagret som tekstvektorer, data frames eller
lister. Funksjonen forsøker først å hente tekst for språket angitt i
`language`. Dersom dette språket ikke finnes, brukes den første
tilgjengelige ikke-tomme teksten.

Verdiene i `classification_uri` omgjøres til KLASS-ID-er. Dersom URI-en
mangler eller ikke avsluttes med en numerisk ID, returneres
`NA_character_`.

Dersom en variabel inneholder en `definition_uri`, brukes sluttdatoen i
`contains_data_until` som gyldighetsdato ved oppslag i tjenesten for
variabeldefinisjoner. Dersom sluttdatoen mangler, brukes dagens dato.

Hver kombinasjon av variabeldefinisjons-ID og gyldighetsdato hentes bare
én gang. Dersom oppslaget feiler, vises en advarsel og de tilhørende
Vardef-feltene fylles med manglende verdier.

## See also

[`datadoc_path()`](datadoc_path.md) for å opprette filstien til
DataDoc-filen og
[`get_variable_definition_by_id()`](get_variable_definition_by_id.md)
for å hente én variabeldefinisjon.

## Examples

``` r
if (FALSE) { # \dontrun{
datadoc_variabeloversikt(
  filsti = "/buckets/data/personell_v1.parquet"
)

datadoc_variabeloversikt(
  filsti = "/buckets/data/personell_v1.parquet",
  language = "en"
)
} # }
```
