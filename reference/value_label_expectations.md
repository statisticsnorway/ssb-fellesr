# Kontroller forventet dekning av verdietiketter

Lager en oversikt over hvilke variabler i et datasett som forventes å ha
verdietiketter, hvilke som faktisk har verdietiketter, og hvilke
variabler som mangler forventede verdietiketter.

## Usage

``` r
value_label_expectations(
  data,
  filsti = NULL,
  variables = NULL,
  character_variables = TRUE,
  missing_only = FALSE
)
```

## Arguments

- data:

  Et datasett som skal undersøkes for verdietiketter.

- filsti:

  `NULL` eller en tekststreng med filstien til en DataDoc JSON-fil.
  Dersom en filsti oppgis, hentes `vardef_id` og `klass_id` fra
  variabelmetadataene i DataDoc-filen. Standardverdien er `NULL`.

- variables:

  `NULL` eller en tekstvektor med navn på variabler som forventes å ha
  verdietiketter. Variabler som oppgis her, markeres som forventet å ha
  verdietiketter uavhengig av datatype. Standardverdien er `NULL`.

- character_variables:

  En logisk verdi som angir om alle tekstvariabler skal forventes å ha
  verdietiketter. Når verdien er `TRUE`, markeres alle variabler der
  [`is.character()`](https://rdrr.io/r/base/character.html) er `TRUE`.
  Standardverdien er `TRUE`.

- missing_only:

  En logisk verdi som angir om resultatet bare skal inneholde variabler
  som forventes å ha verdietiketter, men som mangler slike etiketter.
  Når verdien er `FALSE`, returneres alle variabler. Standardverdien er
  `FALSE`.

## Value

En tibble med én rad per variabel og følgende kolonner:

- `variable`:

  Navnet på variabelen.

- `type`:

  Den første klassen til variabelen, hentet fra
  [`class()`](https://rdrr.io/r/base/class.html).

- `should_have_value_labels`:

  Logisk verdi som angir om variabelen forventes å ha verdietiketter.

- `has_value_labels`:

  Logisk verdi som angir om variabelen faktisk har minst én registrert
  verdietikett.

- `missing_expected_value_labels`:

  Logisk verdi som er `TRUE` dersom variabelen forventes å ha
  verdietiketter, men ikke har noen registrerte verdietiketter.

- `vardef_id`:

  ID-en til variabeldefinisjonen hentet fra `definition_uri` i
  DataDoc-filen. Kolonnen inkluderes bare når `filsti` er oppgitt.

- `klass_id`:

  KLASS-ID-en hentet fra `classification_uri` i DataDoc-filen. Kolonnen
  inkluderes bare når `filsti` er oppgitt.

## Details

Forventningen kan baseres på variabeltype og på en eksplisitt angitt
tekstvektor med variabelnavn. Dersom en DataDoc-fil oppgis, hentes i
tillegg ID-er for variabeldefinisjoner og KLASS-klassifikasjoner.

En variabel regnes som å ha verdietiketter dersom
[`labelled::val_labels()`](https://larmarange.github.io/labelled/reference/val_labels.html)
returnerer minst én etikett.

Når `character_variables = TRUE`, forventes alle tekstvariabler å ha
verdietiketter. Variabler som oppgis eksplisitt i `variables`, forventes
også å ha verdietiketter, uavhengig av datatype.

Dersom både `character_variables = FALSE` og `variables = NULL`,
forventes ingen variabler å ha verdietiketter.

Når `filsti` er oppgitt, leses DataDoc-filen med
[`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).
Funksjonen bruker `short_name` til å koble metadataene i DataDoc-filen
til variablene i `data`.

ID-en i `definition_uri` og `classification_uri` hentes fra teksten
etter siste skråstrek (`/`) eller kolon (`:`). En eventuell avsluttende
skråstrek fjernes før ID-en hentes.

Dersom et av URI-feltene mangler i DataDoc-filen, settes den tilhørende
ID-en til `NA`.

Når `missing_only = TRUE`, filtreres resultatet slik at bare variabler
der `missing_expected_value_labels` er `TRUE`, returneres.

## See also

[`vars_with_value_labels()`](vars_with_value_labels.md) for å finne
variabler som har verdietiketter,
[`values_without_labels()`](values_without_labels.md) for å finne
observerte verdier uten verdietikett og
[`value_label_coverage()`](value_label_coverage.md) for å beregne
dekningen av verdietiketter.

## Examples

``` r
data <- tibble::tibble(
  kjoenn = labelled::labelled(
    c("1", "2", "1"),
    labels = c(
      Mann = "1",
      Kvinne = "2"
    )
  ),
  bosted = c("01", "02", "03"),
  alder = c(35, 42, 28)
)

value_label_expectations(data)
#> # A tibble: 3 × 5
#>   variable type   should_have_value_la…¹ has_value_labels missing_expected_val…²
#>   <chr>    <chr>  <lgl>                  <lgl>            <lgl>                 
#> 1 kjoenn   haven… TRUE                   TRUE             FALSE                 
#> 2 bosted   chara… TRUE                   FALSE            TRUE                  
#> 3 alder    numer… FALSE                  FALSE            FALSE                 
#> # ℹ abbreviated names: ¹​should_have_value_labels,
#> #   ²​missing_expected_value_labels

value_label_expectations(
  data,
  variables = "alder"
)
#> # A tibble: 3 × 5
#>   variable type   should_have_value_la…¹ has_value_labels missing_expected_val…²
#>   <chr>    <chr>  <lgl>                  <lgl>            <lgl>                 
#> 1 kjoenn   haven… TRUE                   TRUE             FALSE                 
#> 2 bosted   chara… TRUE                   FALSE            TRUE                  
#> 3 alder    numer… TRUE                   FALSE            TRUE                  
#> # ℹ abbreviated names: ¹​should_have_value_labels,
#> #   ²​missing_expected_value_labels

value_label_expectations(
  data,
  missing_only = TRUE
)
#> # A tibble: 1 × 5
#>   variable type   should_have_value_la…¹ has_value_labels missing_expected_val…²
#>   <chr>    <chr>  <lgl>                  <lgl>            <lgl>                 
#> 1 bosted   chara… TRUE                   FALSE            TRUE                  
#> # ℹ abbreviated names: ¹​should_have_value_labels,
#> #   ²​missing_expected_value_labels

if (FALSE) { # \dontrun{
value_label_expectations(
  data,
  filsti = "data/personell__DOC.json"
)
} # }
```
