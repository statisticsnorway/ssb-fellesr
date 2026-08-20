# Hent KLASS-ID fra en variabeldefinisjon

Henter den numeriske KLASS-ID-en fra feltet `classification_uri` i en
variabeldefinisjon.

## Usage

``` r
vardef_get_klass_id(variable_definition)
```

## Arguments

- variable_definition:

  En liste eller et listelignende objekt som inneholder feltet
  `classification_uri`.

## Value

En tekststreng med KLASS-ID-en. Dersom `classification_uri` mangler, er
tom eller ikke inneholder en numerisk ID på slutten av adressen,
returneres `NA_character_`.

## Details

Funksjonen forventer at `classification_uri` avsluttes med en numerisk
KLASS-ID, eventuelt etterfulgt av en skråstrek.

Dersom `classification_uri` finnes, men ikke har forventet format, vises
en advarsel før `NA_character_` returneres.

## Examples

``` r
variable_definition <- list(
  classification_uri =
    "https://www.ssb.no/klass/klassifikasjoner/1"
)

vardef_get_klass_id(variable_definition)
#> [1] "1"

variable_definition <- list(
  classification_uri =
    "https://www.ssb.no/klass/klassifikasjoner/1/"
)

vardef_get_klass_id(variable_definition)
#> [1] "1"

variable_definition <- list(
  classification_uri = NULL
)

vardef_get_klass_id(variable_definition)
#> [1] NA
```
