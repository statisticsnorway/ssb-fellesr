# Sjekk for endringer i verdier mellom siste og nyeste versjon av en fil

Funksjonen sammenligner innholdet (verdiene) i den siste versjonerte
filen med en ny fil for å se om det har skjedd endringer i dataene.

## Usage

``` r
sjekk_endring_verdier(filsti, versjon_1 = "uversjonert", versjon_2 = "siste")
```

## Arguments

- filsti:

  En karakterstreng som representerer filstien til den nyeste versjonen
  av filen. Funksjonen finner selv den siste versjonerte filen ved å
  bruke [`lag_versjonert_filsti`](lag_versjonert_filsti.md).

## Value

Funksjonen returnerer en logisk verdi:

- `TRUE`: Hvis det har skjedd endringer i verdiene mellom den siste
  versjonerte filen og den nye filen.

- `FALSE`: Hvis det ikke er noen endringer i verdiene.

## Details

Funksjonen bruker
[`dplyr::anti_join`](https://dplyr.tidyverse.org/reference/filter-joins.html)
for å sammenligne verdiene i den siste versjonerte filen med den nye
filen. Den sjekker om det finnes rader som er forskjellige mellom de to
datasettene. Hvis det finnes forskjeller, returnerer funksjonen `TRUE`.
Hvis ingen forskjeller oppdages, returneres `FALSE`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Sjekk om det er endringer i verdiene mellom siste versjon og ny fil
sjekk_endring_verdier("prosjekt/data.parquet")

} # }
```
