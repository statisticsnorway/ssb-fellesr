# Finn variabler uten kolonneetikett

Finner variabler i et datasett som mangler en kolonneetikett.

## Usage

``` r
vars_without_labels(data)
```

## Arguments

- data:

  Et datasett som skal undersøkes for kolonneetiketter.

## Value

En tekstvektor med navnene på variablene som mangler kolonneetikett.
Dersom alle variablene har en kolonneetikett, returneres en tom
tekstvektor.

## Details

Kolonneetikettene hentes med
[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html).

En variabel regnes som å mangle kolonneetikett dersom etiketten:

- er `NULL`;

- har lengde null;

- bare inneholder manglende verdier; eller

- bare inneholder tom tekst eller mellomrom.

## See also

[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html)
for å hente eller angi kolonneetiketter.

## Examples

``` r
data <- data.frame(
  kjoenn = c(1, 2, 1),
  alder = c(35, 42, 28),
  inntekt = c(450000, 520000, 390000)
)

labelled::var_label(data$kjoenn) <- "Kjønn"
labelled::var_label(data$alder) <- "Alder i år"

vars_without_labels(data)
#> [1] "inntekt"
```
