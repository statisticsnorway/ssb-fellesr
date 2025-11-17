# Sjekk for endringer i rader og kolonner mellom siste og nyeste versjon av en fil

Funksjonen sammenligner antall rader og kolonner, samt kolonnenavnene, i
den siste versjonerte filen med tilsvarende i en ny fil for å se om det
har skjedd endringer i struktur.

## Usage

``` r
sjekk_endring_rader_kolonner(
  filsti,
  versjon_1 = "uversjonert",
  versjon_2 = "siste"
)
```

## Arguments

- filsti:

  En karakterstreng som representerer filstien til den nyeste versjonen
  av filen. Funksjonen finner selv den siste versjonerte filen ved å
  bruke [`lag_versjonert_filsti`](lag_versjonert_filsti.md).

## Value

Funksjonen returnerer en logisk verdi:

- `TRUE`: Hvis det har skjedd endringer i antall rader, antall kolonner
  eller kolonnenavn mellom den siste versjonerte filen og den nye filen.

- `FALSE`: Hvis det ikke er noen endringer i rader eller kolonner.

## Details

Funksjonen åpner den siste versjonerte filen i en gitt sti ved hjelp av
[`lag_versjonert_filsti`](lag_versjonert_filsti.md) og sammenligner
antall rader og kolonner, samt kolonnene (navn og antall), med de i den
nye filen som er angitt av `filsti`. Det kontrolleres først om antallet
rader eller kolonner har endret seg. Deretter kontrolleres det om
navnene på kolonnene har endret seg. Hvis det er en endring i enten
antall rader, antall kolonner eller kolonnenavnene, returnerer
funksjonen `TRUE`. Hvis ingen endringer oppdages, returneres `FALSE`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Sjekk om det er endringer i rader eller kolonner mellom siste versjon og ny fil
sjekk_endring_rader_kolonner("prosjekt/data.parquet")

} # }
```
