# Finn variabler som mangler obligatorisk metadata

Lager en oversikt over hvilke variabler i en Datadoc-fil som mangler ett
eller flere obligatoriske metadatafelt.

## Usage

``` r
summarise_missing_required_metadata(filsti, only_incomplete = TRUE)
```

## Arguments

- filsti:

  En tekststreng med filstien til en Datadoc-fil eller den tilhørende
  Parquet-filen.

- only_incomplete:

  En logisk verdi som angir om bare variabler som mangler ett eller
  flere obligatoriske metadatafelt skal returneres. Når verdien er
  `TRUE`, returneres bare ufullstendig dokumenterte variabler. Når
  verdien er `FALSE`, returneres alle variabler. Standardverdien er
  `TRUE`.

## Value

En `data.frame` med én rad per variabel og følgende kolonner:

- `variable`:

  Variabelens kortnavn (`short_name`).

- `n_missing`:

  Antall obligatoriske metadatafelt som mangler.

- `missing_required_fields`:

  En kommaseparert tekststreng med navnene på de obligatoriske
  metadatafeltene som mangler. Verdien er `NA` dersom ingen
  obligatoriske felt mangler.

- `complete`:

  En logisk verdi som angir om alle obligatoriske metadatafelt er
  utfylt.

## Details

Funksjonen kontrollerer følgende obligatoriske, brukerutfylte
metadatafelt for hver variabel:

- `name`

- `is_personal_data`

- `unit_type`

- `variable_role`

- `data_source`

- `temporality_type`

Et metadatafelt regnes som manglende dersom verdien er `NULL`, har
lengde null, er `NA`, eller består av tom tekst eller bare mellomrom.

Logiske verdier behandles som gyldige verdier. Dette innebærer blant
annet at `FALSE` i `is_personal_data` ikke regnes som manglende
metadata.

Dersom `only_incomplete = TRUE`, filtreres variabler som har alle de
obligatoriske metadatafeltene utfylt bort fra resultatet.

## See also

[`datadoc_path()`](datadoc_path.md) for å konvertere mellom filstier til
Parquet- og Datadoc-filer.

## Examples

``` r
if (FALSE) { # \dontrun{
# Vis bare variabler som mangler obligatorisk metadata
summarise_missing_required_metadata(
  filsti = "data/personell__DOC.json"
)

# Vis alle variabler
summarise_missing_required_metadata(
  filsti = "data/personell__DOC.json",
  only_incomplete = FALSE
)
} # }
```
