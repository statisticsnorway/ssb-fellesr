# Sjekk for endringer i datatyper mellom siste og nyeste versjon av en fil

Funksjonen sammenligner datatypene til kolonnene i den siste versjonerte
filen med datatypene i en ny fil for å se om det har skjedd endringer i
datatypene for kolonnene.

## Usage

``` r
sjekk_endring_datatype(filsti, versjon_1 = "uversjonert", versjon_2 = "siste")
```

## Arguments

- filsti:

  En karakterstreng som representerer filstien til den nyeste versjonen
  av filen. Funksjonen finner selv den siste versjonerte filen ved å
  bruke [`lag_versjonert_filsti`](lag_versjonert_filsti.md).

## Value

Funksjonen returnerer en logisk verdi:

- `TRUE`: Hvis det har skjedd endringer i datatypene til kolonnene
  mellom den siste versjonerte filen og den nye filen.

- `FALSE`: Hvis det ikke er noen endringer i datatypene.

## Details

Funksjonen åpner den siste versjonerte filen i en gitt sti ved hjelp av
[`lag_versjonert_filsti`](lag_versjonert_filsti.md) og sammenligner
kolonnenavn og datatyper med de i den nye filen som er angitt av
`filsti`. Det kontrolleres om datatypene for kolonnene har endret seg
mellom den siste versjonen og den nye filen. Hvis det er en endring i
datatypene, returnerer funksjonen `TRUE`. Hvis ingen endringer oppdages,
returneres `FALSE`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Sjekk om det er endringer i datatypene mellom siste versjon og ny fil
sjekk_endring_datatype("prosjekt/data.parquet")

} # }
```
