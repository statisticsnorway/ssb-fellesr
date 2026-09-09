# Beregn dekning av verdietiketter

Beregner hvor stor andel av de observerte verdiene i hver variabel som
har en tilhørende verdietikett.

## Usage

``` r
value_label_coverage(data, labelled_only = FALSE)
```

## Arguments

- data:

  Et datasett som skal undersøkes for verdietiketter.

- labelled_only:

  En logisk verdi som angir om bare variabler som har minst én
  verdietikett skal inkluderes. Når verdien er `FALSE`, inkluderes alle
  variabler i datasettet. Standardverdien er `FALSE`.

## Value

En tibble med én rad per variabel og følgende kolonner:

- `variable`:

  Navnet på variabelen.

- `has_value_labels`:

  Logisk verdi som angir om variabelen har minst én registrert
  verdietikett.

- `n_values`:

  Antall observerte, ikke-manglende verdier i variabelen.

- `n_values_with_label`:

  Antall observerte verdier som har en tilhørende verdietikett.

- `pct_values_with_label`:

  Andel observerte verdier som har en tilhørende verdietikett, angitt i
  prosent og avrundet til to desimaler.

- `n_unique_values`:

  Antall unike observerte, ikke-manglende verdier.

- `n_unique_values_with_label`:

  Antall unike observerte verdier som har en tilhørende verdietikett.

- `pct_unique_values_with_label`:

  Andel unike observerte verdier som har en tilhørende verdietikett,
  angitt i prosent og avrundet til to desimaler.

## Details

Funksjonen beregner både dekning blant alle observerte verdier og blant
unike observerte verdier.

Verdietikettene hentes med
[`labelled::val_labels()`](https://larmarange.github.io/labelled/reference/val_labels.html).
Manglende verdier tas ikke med i beregningene.

En observert verdi regnes som merket dersom verdien finnes blant
verdiene som er registrert i variabelens verdietiketter.

`pct_values_with_label` beregnes på grunnlag av alle observerte verdier.
Dersom samme verdi forekommer flere ganger, teller hver forekomst
separat.

`pct_unique_values_with_label` beregnes derimot på grunnlag av de unike
observerte verdiene. Hver forskjellig verdi teller derfor bare én gang,
uavhengig av hvor ofte den forekommer i datasettet.

Dersom en variabel ikke inneholder noen observerte verdier, settes den
aktuelle prosentandelen til `NA`.

Når `labelled_only = TRUE`, inkluderes bare variabler som returneres av
[`vars_with_value_labels()`](vars_with_value_labels.md).

## See also

[`vars_with_value_labels()`](vars_with_value_labels.md) for å finne
variabler med verdietiketter,
[`values_without_labels()`](values_without_labels.md) for å finne
observerte verdier uten verdietikett og
[`labelled::val_labels()`](https://larmarange.github.io/labelled/reference/val_labels.html)
for å hente verdietikettene til en variabel.

## Examples

``` r
data <- tibble::tibble(
  kjoenn = labelled::labelled(
    c(1, 2, 1, 3, NA),
    labels = c(
      Mann = 1,
      Kvinne = 2
    )
  ),
  alder = c(35, 42, 28, 51, 37)
)

value_label_coverage(data)
#> # A tibble: 2 × 8
#>   variable has_value_labels n_values n_values_with_label pct_values_with_label
#>   <chr>    <lgl>               <int>               <int>                 <dbl>
#> 1 kjoenn   TRUE                    4                   3                    75
#> 2 alder    FALSE                   5                   0                     0
#> # ℹ 3 more variables: n_unique_values <int>, n_unique_values_with_label <int>,
#> #   pct_unique_values_with_label <dbl>

value_label_coverage(
  data,
  labelled_only = TRUE
)
#> # A tibble: 1 × 8
#>   variable has_value_labels n_values n_values_with_label pct_values_with_label
#>   <chr>    <lgl>               <int>               <int>                 <dbl>
#> 1 kjoenn   TRUE                    4                   3                    75
#> # ℹ 3 more variables: n_unique_values <int>, n_unique_values_with_label <int>,
#> #   pct_unique_values_with_label <dbl>
```
