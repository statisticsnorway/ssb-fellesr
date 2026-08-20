# Finn variabler med verdietiketter

Finner variabler i et datasett som har én eller flere verdietiketter.

## Usage

``` r
vars_with_value_labels(data)
```

## Arguments

- data:

  Et datasett som skal undersøkes for verdietiketter.

## Value

En tekstvektor med navnene på variablene som har minst én verdietikett.
Dersom ingen variabler har verdietiketter, returneres en tom
tekstvektor.

## Details

Verdietikettene for hver variabel hentes med
[`labelled::val_labels()`](https://larmarange.github.io/labelled/reference/val_labels.html).
En variabel inkluderes i resultatet dersom den har minst én registrert
verdietikett.

Kolonneetiketter, som hentes med
[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html),
tas ikke med i vurderingen.

## See also

[`labelled::val_labels()`](https://larmarange.github.io/labelled/reference/val_labels.html)
for å hente verdietiketter og
[`vars_without_labels()`](vars_without_labels.md) for å finne variabler
uten kolonneetikett.

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
  alder = c(35, 42, 28)
)

vars_with_value_labels(data)
#> [1] "kjoenn"
```
