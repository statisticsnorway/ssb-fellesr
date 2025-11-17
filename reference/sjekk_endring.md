# Sjekk for endringer i kolonner, datatyper eller verdier mellom siste og nyeste versjon av en fil

Funksjonen sjekker om det har skjedd noen endringer i kolonner,
datatyper eller verdier mellom den siste versjonerte filen og en ny fil.
Den kombinerer tre separate sjekker: for kolonner, datatyper og verdier.

## Usage

``` r
sjekk_endring(filsti, versjon_1 = "uversjonert", versjon_2 = "siste")
```

## Arguments

- filsti:

  En karakterstreng som representerer filstien til den nyeste versjonen
  av filen. Funksjonen finner selv den siste versjonerte filen ved å
  bruke [`lag_versjonert_filsti`](lag_versjonert_filsti.md).

## Value

Funksjonen returnerer en logisk verdi:

- `TRUE`: Hvis det har skjedd endringer i kolonnene, datatypene eller
  verdiene mellom den siste versjonerte filen og den nye filen.

- `FALSE`: Hvis det ikke er noen endringer i noen av disse områdene.

## Details

Funksjonen kombinerer sjekker fra
[`sjekk_endring_rader_kolonner`](sjekk_endring_rader_kolonner.md),
[`sjekk_endring_datatype`](sjekk_endring_datatype.md), og
[`sjekk_endring_verdier`](sjekk_endring_verdier.md). Den sjekker først
om det har skjedd endringer i kolonnene, deretter i datatypene, og til
slutt om det har skjedd endringer i verdiene mellom filene. Hvis det
oppdages en endring på noen av disse områdene, returnerer funksjonen
`TRUE`. Hvis ingen endringer oppdages, returneres `FALSE`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Sjekk om det er noen endringer mellom siste versjon og ny fil
sjekk_endring("prosjekt/data.parquet")

} # }
```
