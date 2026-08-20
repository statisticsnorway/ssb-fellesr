# Hent variabeldefinisjon etter kortnavn

Henter en variabeldefinisjon fra SSBs tjeneste for variabeldefinisjoner
basert på variabelens kortnavn og en gyldighetsdato.

## Usage

``` r
get_variable_definition_by_shortname(
  short_name,
  date = format(Sys.Date(), "%Y-%m-%d"),
  language = "nb"
)
```

## Arguments

- short_name:

  En tekststreng med kortnavnet til variabeldefinisjonen som skal
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

Kortnavnet sendes til tjenesten som parameteren `short_name`, mens
datoen sendes som parameteren `date_of_validity`. Språket sendes i
HTTP-hodet `Accept-Language`.

Funksjonen stopper med en feilmelding dersom tjenesten returnerer en
HTTP-feil.

## Examples

``` r
if (FALSE) { # \dontrun{
get_variable_definition_by_shortname(
  short_name = "sesongjustering"
)

get_variable_definition_by_shortname(
  short_name = "sesongjustering",
  date = "2025-01-01",
  language = "en"
)
} # }
```
