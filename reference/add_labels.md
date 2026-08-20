# Legg til kolonneetiketter fra Datadoc

Legger til kolonneetiketter på variablene i et datasett basert på
variabelnavnene i den tilhørende DataDoc-filen.

## Usage

``` r
add_labels(data, filsti, show_labels = TRUE)
```

## Arguments

- data:

  Et datasett som skal få lagt til kolonneetiketter.

- filsti:

  En tekststreng med filstien til Parquet-filen som datasettet er lest
  fra. Filstien brukes til å finne den tilhørende DataDoc-filen.

- show_labels:

  En logisk verdi som angir om kolonneetikettene også skal vises ved
  utskrift av datasettet. Når verdien er `TRUE`, behandles datasettet
  med [`show_column_labels()`](show_column_labels.md). Standardverdien
  er `TRUE`.

## Value

Datasettet som ble oppgitt i `data`, med kolonneetiketter lagt til for
variabler som finnes både i datasettet og i DataDoc-filen.

## Details

Variabeloversikten hentes med
[`datadoc_variabeloversikt()`](datadoc_variabeloversikt.md). Verdiene i
kolonnen `name` brukes som kolonneetiketter, mens `short_name` brukes
til å koble etikettene til variablene i `data`.

Metadata for variabler som ikke finnes i `data`, ignoreres.

Kolonneetikettene legges til med
[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html).
Dersom `show_labels = TRUE`, registreres etikettene i tillegg som
`pillar`-etiketter med [`show_column_labels()`](show_column_labels.md),
slik at de vises under kolonnenavnene når datasettet skrives ut som en
tibble.

Meldinger som oppstår når variabeloversikten hentes, undertrykkes.
Advarsler og feil undertrykkes ikke.

`show_labels` må være én enkelt logisk verdi og kan ikke være `NA`.

## See also

[`datadoc_variabeloversikt()`](datadoc_variabeloversikt.md) for å hente
variabelmetadata,
[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html)
for å legge til kolonneetiketter og
[`show_column_labels()`](show_column_labels.md) for å vise etikettene
ved utskrift.

## Examples

``` r
if (FALSE) { # \dontrun{
data_med_labels <- add_labels(
  data = personell,
  filsti = "data/personell.parquet"
)

data_med_labels <- add_labels(
  data = personell,
  filsti = "data/personell.parquet",
  show_labels = FALSE
)
} # }
```
