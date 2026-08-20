# Vis kolonneetiketter ved utskrift av et datasett

Legger variabelens kolonneetikett til som en `pillar`-etikett, slik at
etiketten vises under kolonnenavnet når datasettet skrives ut som en
tibble.

## Usage

``` r
show_column_labels(data)
```

## Arguments

- data:

  Et datasett som kan inneholde variabler med kolonneetiketter.

## Value

Datasettet som ble oppgitt i `data`, med kolonneetikettene registrert i
variabelens `pillar`-attributt. Verdiene, kolonnenavnene og de
opprinnelige kolonneetikettene beholdes uendret.

## Details

Variabler som ikke har en kolonneetikett, beholdes uendret.

Kolonneetiketten for hver variabel hentes med
[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html).
Dersom variabelen har en ikke-tom etikett, lagres denne som elementet
`label` i variabelens `pillar`-attributt.

`pillar`-attributtet brukes ved utskrift av tibble-objekter og påvirker
ikke verdiene i datasettet. Funksjonen endrer heller ikke variabelens
opprinnelige `label`-attributt.

Eksisterende elementer i `pillar`-attributtet beholdes. Dersom
attributtet allerede inneholder et element med navnet `label`, erstattes
dette med variabelens gjeldende kolonneetikett.

## See also

[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html)
for å hente og angi kolonneetiketter.

## Examples

``` r
data <- tibble::tibble(
  kjoenn = c(1, 2, 1),
  alder = c(35, 42, 28)
)

labelled::var_label(data$kjoenn) <- "Kjønn"
labelled::var_label(data$alder) <- "Alder i år"

show_column_labels(data)
#> # A tibble: 3 × 2
#>   kjoenn      alder
#>    Kjønn Alder i år
#> 1      1         35
#> 2      2         42
#> 3      1         28
```
