# Legg til kolonner med verdietiketter

Oppretter nye kolonner med verdietiketter for merkede variabler, uten å
endre de opprinnelige variablene.

## Usage

``` r
add_labelled_columns(data, variables = NULL, postfix = "_labelled")
```

## Arguments

- data:

  Et datasett som inneholder én eller flere merkede variabler med
  verdietiketter.

- variables:

  En karaktervektor med navn på variablene det skal opprettes nye
  etikettkolonner for. Alle variablene må finnes i `data` og være
  merkede variabler som gjenkjennes av
  [`haven::is.labelled()`](https://haven.tidyverse.org/reference/labelled.html).
  Dersom `NULL`, opprettes nye kolonner for alle merkede variabler i
  `data`. Standardverdien er `NULL`.

- postfix:

  En tekststreng som legges til på slutten av navnet til de nye
  variablene. Standardverdien er `"_labelled"`.

## Value

Datasettet som ble oppgitt i `data`, med én ny kolonne for hver valgt
merket variabel. De opprinnelige variablene beholdes uendret. De nye
variablene er faktorer der verdietikettene brukes som faktorverdier.

## Details

Funksjonen konverterer de valgte variablene med
[`haven::as_factor()`](https://forcats.tidyverse.org/reference/as_factor.html)
og `levels = "labels"`. Dette innebærer at de nye kolonnene inneholder
verdietikettene, mens de opprinnelige kodene beholdes i de opprinnelige
variablene.

Dersom `variables = NULL`, identifiseres alle merkede variabler i
datasettet ved hjelp av
[`haven::is.labelled()`](https://haven.tidyverse.org/reference/labelled.html).
Dersom ingen merkede variabler finnes, returneres datasettet uendret og
det gis en advarsel.

Navnet på hver ny variabel består av det opprinnelige variabelnavnet
etterfulgt av verdien i `postfix`. Dersom for eksempel variabelen heter
`kjoenn` og `postfix = "_labelled"`, får den nye variabelen navnet
`kjoenn_labelled`.

Funksjonen gir en feil dersom:

- `variables` ikke er `NULL` eller en karaktervektor med minst ett
  variabelnavn.

- én eller flere av variablene i `variables` ikke finnes i `data`.

- én eller flere av variablene i `variables` ikke er merkede variabler.

- én eller flere av de nye variablene allerede finnes i `data`.

- `postfix` ikke er én enkelt tekststreng.

Det gis en advarsel dersom konverteringen introduserer nye
missing-verdier. Missing-verdier som allerede finnes i de opprinnelige
variablene utløser ikke advarselen. Nye missing-verdier kan blant annet
oppstå dersom enkelte verdier ikke har en tilhørende verdietikett.

## Examples

``` r
data <- data.frame(
  kjoenn = labelled::labelled(
    c(1, 2, 1),
    labels = c(
      Mann = 1,
      Kvinne = 2
    )
  ),
  region = labelled::labelled(
    c("01", "02", "01"),
    labels = c(
      Ost = "01",
      Vest = "02"
    )
  ),
  alder = c(30, 45, 52)
)

# Opprett etikettkolonner for alle merkede variabler
add_labelled_columns(
  data = data
)
#>   kjoenn region alder kjoenn_labelled region_labelled
#> 1      1     01    30            Mann             Ost
#> 2      2     02    45          Kvinne            Vest
#> 3      1     01    52            Mann             Ost

# Opprett etikettkolonne for én bestemt variabel
add_labelled_columns(
  data = data,
  variables = "kjoenn"
)
#>   kjoenn region alder kjoenn_labelled
#> 1      1     01    30            Mann
#> 2      2     02    45          Kvinne
#> 3      1     01    52            Mann

# Opprett etikettkolonner for flere bestemte variabler
add_labelled_columns(
  data = data,
  variables = c(
    "kjoenn",
    "region"
  )
)
#>   kjoenn region alder kjoenn_labelled region_labelled
#> 1      1     01    30            Mann             Ost
#> 2      2     02    45          Kvinne            Vest
#> 3      1     01    52            Mann             Ost

# Bruk et annet postfiks
add_labelled_columns(
  data = data,
  variables = "kjoenn",
  postfix = "_navn"
)
#>   kjoenn region alder kjoenn_navn
#> 1      1     01    30        Mann
#> 2      2     02    45      Kvinne
#> 3      1     01    52        Mann
```
