# Sammenlign variabelmetadata mellom to DataDoc-filer

Sammenligner utvalgt variabelmetadata mellom to DataDoc-filer og viser
hvilke variabler som har ulik KLASS-referanse eller ulik tidsperiode for
dataene.

## Usage

``` r
compare_variable_metadata(filsti_1, filsti_2, only_differences = TRUE)
```

## Arguments

- filsti_1:

  Tekststreng. Filsti til den første DataDoc-filen eller Parquet-filen.

- filsti_2:

  Tekststreng. Filsti til den andre DataDoc-filen eller Parquet-filen.

- only_differences:

  Logisk verdi. Dersom `TRUE`, returneres kun variabler med ulik
  metadata. Dersom `FALSE`, returneres alle variabler som finnes i begge
  DataDoc-filene. Standard er `TRUE`.

## Value

En tibble med én rad per variabel som finnes i begge DataDoc-filene.
Resultatet inneholder følgende kolonner:

- `variable`: Variabelens kortnavn.

- `klass_id_1`: KLASS-id i den første DataDoc-filen.

- `klass_id_2`: KLASS-id i den andre DataDoc-filen.

- `ulik_klass_id`: Om KLASS-id er forskjellig.

- `contains_data_from_1`: Startdato for data i den første DataDoc-filen.

- `contains_data_from_2`: Startdato for data i den andre DataDoc-filen.

- `contains_data_until_1`: Sluttdato for data i den første
  DataDoc-filen.

- `contains_data_until_2`: Sluttdato for data i den andre DataDoc-filen.

- `ulik_tidsperiode`: Om start- eller sluttdato er forskjellig.

- `ulik_metadata`: Om KLASS-id eller tidsperiode er forskjellig.

Dersom `only_differences = TRUE`, inneholder resultatet kun rader der
`ulik_metadata` er `TRUE`.

## Details

Dersom en filsti peker til en Parquet-fil, brukes
[`datadoc_path()`](datadoc_path.md) til å finne tilhørende DataDoc-fil.

Funksjonen sammenligner følgende metadata for variabler som finnes i
begge DataDoc-filene:

- KLASS-id hentet fra `classification_uri`

- `contains_data_from`

- `contains_data_until`

Variabler som kun finnes i én av DataDoc-filene, tas ikke med i
sammenligningen.

Forskjeller i `contains_data_from` eller `contains_data_until`
oppsummeres i kolonnen `ulik_tidsperiode`. Kolonnen `ulik_metadata` er
`TRUE` dersom enten KLASS-id eller tidsperiode er forskjellig mellom
filene.

Manglende verdier behandles som en forskjell dersom metadata er utfylt i
den ene DataDoc-filen, men mangler i den andre.

## Examples

``` r
if (FALSE) { # \dontrun{
compare_variable_metadata(
  filsti_1 = "data/datasett_2025__DOC.json",
  filsti_2 = "data/datasett_2026__DOC.json"
)

compare_variable_metadata(
  filsti_1 = "data/datasett_2025.parquet",
  filsti_2 = "data/datasett_2026.parquet",
  only_differences = FALSE
)
} # }
```
