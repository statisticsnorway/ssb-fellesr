# Hent variabeldefinisjon etter ID

Henter en variabeldefinisjon fra SSBs tjeneste for variabeldefinisjoner
basert på variabeldefinisjonens unike ID og en gyldighetsdato.

## Usage

``` r
get_variable_definition_by_id(
  id,
  date = format(Sys.Date(), "%Y-%m-%d"),
  language = "nb"
)
```

## Arguments

- id:

  En tekststreng med den unike ID-en til variabeldefinisjonen som skal
  hentes.

- date:

  En tekststreng med datoen variabeldefinisjonen skal være gyldig på,
  angitt på formatet `"YYYY-MM-DD"`. Standardverdien er dagens dato.

- language:

  En tekststreng med språkkoden som skal brukes for tekstinnholdet i
  responsen. Standardverdien er `"nb"`.

## Value

Et objekt opprettet fra JSON-responsen fra tjenesten. Returtypen
avhenger av strukturen i responsen og vil vanligvis være en liste eller
en data frame.

## Details

ID-en legges til URL-en til tjenesten, mens datoen sendes som
parameteren `date_of_validity`. Språket sendes i HTTP-hodet
`Accept-Language`.

Dersom tjenesten returnerer HTTP-status 404, vises en melding om at
variabeldefinisjonen ikke ble funnet. Funksjonen returnerer samtidig
innholdet i feilresponsen.

Andre HTTP-feil fører til at funksjonen stopper med en feilmelding.

## Examples

``` r
if (FALSE) { # \dontrun{
get_variable_definition_by_id(
  id = "91HwKSxr"
)

get_variable_definition_by_id(
  id = "91HwKSxr",
  date = "1814-12-31",
  language = "en"
)
} # }
```
