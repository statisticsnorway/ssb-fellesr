# Vis verdietiketter i et datasett

Erstatter verdiene i merkede variabler med tilhørende verdietiketter.
Variabler uten verdietiketter beholdes uendret.

## Usage

``` r
show_labels_df(data, labels = TRUE)
```

## Arguments

- data:

  Et datasett som kan inneholde variabler med verdietiketter.

- labels:

  En logisk verdi som angir om verdietikettene skal vises. Når verdien
  er `TRUE`, konverteres merkede variabler til faktorer med
  verdietikettene som faktorverdier. Når verdien er `FALSE`, returneres
  datasettet uendret. Standardverdien er `TRUE`.

## Value

Datasettet som ble oppgitt i `data`. Dersom `labels = TRUE`, er merkede
variabler konvertert til faktorer med verdietikettene som faktorverdier.

## Details

Funksjonen identifiserer merkede variabler ved hjelp av
[`haven::is.labelled()`](https://haven.tidyverse.org/reference/labelled.html)
og konverterer dem med
[`haven::as_factor()`](https://forcats.tidyverse.org/reference/as_factor.html).

Ved konverteringen brukes `levels = "labels"`, slik at faktorverdiene
består av verdietikettene og ikke de underliggende kodene. Dette
fungerer både for numeriske og tekstbaserte merkede variabler, inkludert
tekstkoder med ledende nuller, som `"01"` og `"02"`.

`labels` må være én enkelt logisk verdi og kan ikke være `NA`.

## Examples

``` r
data <- data.frame(
  kjoenn = labelled::labelled(
    c(1, 2, 1),
    labels = c(
      Mann = 1,
      Kvinne = 2
    )
  )
)

show_labels_df(
  data = data
)
#>   kjoenn
#> 1   Mann
#> 2 Kvinne
#> 3   Mann

show_labels_df(
  data = data,
  labels = FALSE
)
#>   kjoenn
#> 1      1
#> 2      2
#> 3      1
```
