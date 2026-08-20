# Hent variabeldefinisjoner

Henter variabeldefinisjoner fra SSBs tjeneste for variabeldefinisjoner.
Dersom en dato oppgis, returneres bare definisjoner som er gyldige på
denne datoen.

## Usage

``` r
get_all_variable_definitions(
  date = NULL,
  language = "nb",
  status = "ekstern",
  token = NULL
)
```

## Arguments

- date:

  `NULL`, et `Date`-objekt eller en tekststreng med dato på formatet
  `"YYYY-MM-DD"`. Dersom verdien er `NULL`, returneres alle
  tilgjengelige variabeldefinisjoner. Dersom en dato oppgis, returneres
  bare definisjoner der datoen ligger mellom `valid_from` og
  `valid_until`. Standardverdien er `NULL`.

- language:

  En tekststreng med språkkoden som skal brukes for tekstinnholdet i
  responsen. Gyldige verdier er `"nb"`, `"nn"` og `"en"`.
  Standardverdien er `"nb"`.

- status:

  En tekststreng som angir om den eksterne eller interne tjenesten skal
  brukes. Gyldige verdier er `"ekstern"` og `"intern"`. Standardverdien
  er `"ekstern"`.

- token:

  Et eventuelt bearer-token ved bruk av den interne tjenesten.
  Standardverdien er `NULL`.

## Value

En data frame eller liste opprettet fra JSON-responsen. Dersom `date` er
oppgitt, returneres en data frame med variabeldefinisjonene som er
gyldige på den angitte datoen.

## Details

Filtreringen på dato gjøres lokalt etter at variabeldefinisjonene er
hentet fra tjenesten. En definisjon regnes som gyldig når `valid_from`
er tidligere enn eller lik den oppgitte datoen, og `valid_until` enten
mangler eller er senere enn eller lik den oppgitte datoen.

Manglende verdi i `valid_from` tolkes som at definisjonen ikke har noen
nedre gyldighetsgrense. Manglende verdi i `valid_until` tolkes som at
definisjonen fortsatt er gyldig.

## Examples

``` r
if (FALSE) { # \dontrun{
# Hent alle tilgjengelige variabeldefinisjoner
get_all_variable_definitions()

# Hent alle tilgjengelige variabeldefinisjoner på engelsk
get_all_variable_definitions(
  language = "en"
)

# Hent definisjoner som er gyldige på en bestemt dato
get_all_variable_definitions(
  date = "2025-01-01"
)

# Datoen kan også oppgis som et Date-objekt
get_all_variable_definitions(
  date = as.Date("2025-01-01"),
  language = "en"
)
} # }
```
