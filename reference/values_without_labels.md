# Finn observerte verdier uten verdietikett

Finner observerte verdier i merkede variabler som ikke har en tilhørende
verdietikett.

## Usage

``` r
values_without_labels(data)
```

## Arguments

- data:

  Et datasett som skal undersøkes for manglende verdietiketter.

## Value

En tibble med én rad per observert verdi som mangler verdietikett, og
følgende kolonner:

- `variable`:

  Navnet på variabelen.

- `value_without_label`:

  Den observerte verdien som mangler verdietikett.

Dersom alle observerte verdier har verdietiketter, returneres en tom
tibble.

## Details

Funksjonen undersøker variablene som returneres av
[`vars_with_value_labels()`](vars_with_value_labels.md). Det betyr at
bare variabler som allerede har minst én verdietikett, blir kontrollert.

Manglende verdier fjernes før kontrollen. De gjenværende observerte
verdiene sammenlignes med verdiene som er registrert i
[`labelled::val_labels()`](https://larmarange.github.io/labelled/reference/val_labels.html).

Variabler som ikke har noen verdietiketter, tas ikke med i resultatet.
Funksjonen er derfor beregnet på å finne umerkede verdier i variabler
med en delvis definert verdietikettliste.

## See also

[`vars_with_value_labels()`](vars_with_value_labels.md) for å finne
variabler med verdietiketter og
[`labelled::val_labels()`](https://larmarange.github.io/labelled/reference/val_labels.html)
for å hente verdietikettene til en variabel.

## Examples

``` r
data <- data.frame(
  kjoenn = labelled::labelled(
    c(1, 2, 3, NA),
    labels = c(
      Mann = 1,
      Kvinne = 2
    )
  ),
  alder = c(35, 42, 28, 51)
)

values_without_labels(data)
#> # A tibble: 1 × 2
#>   variable value_without_label
#>   <chr>                  <dbl>
#> 1 kjoenn                     3
```
