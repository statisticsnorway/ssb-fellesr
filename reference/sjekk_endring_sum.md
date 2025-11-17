# Sjekk for endringer i sum for numeriske kolonner mellom siste og nyeste versjon av en fil

Denne funksjonen sjekker om det har skjedd noen endringer i summene av
de numeriske kolonnene mellom den siste versjonerte filen med en ny fil
for å se om det har skjedd endringer i dataene. Funksjonen sammenligner
datasett ved å beregne summen av numeriske kolonner i hvert sett, og
deretter sjekke om det finnes forskjeller.

## Usage

``` r
sjekk_endring_sum(filsti, versjon_1 = "uversjonert", versjon_2 = "siste")
```

## Arguments

- filsti:

  En tekststreng som spesifiserer filstien til den nye datasetten som
  skal sammenlignes. Funksjonen forventer at det finnes en tidligere
  versjonert fil på samme plassering.

## Value

En logisk verdi (`TRUE` eller `FALSE`). Returnerer `TRUE` hvis det er
endringer i summen av numeriske kolonner mellom de to datasettene, og
`FALSE` hvis det ikke er noen endringer.

## Details

Funksjonen benytter seg av Arrow-pakken for å åpne datasettene med lazy
loading og arsenal-pakken for å sammenligne dataene. Den håndterer
kolonner som er numeriske (int eller float) og summerer disse før
sammenligning. Dersom det er noen forskjeller i summene mellom de to
datasettene, returnerer funksjonen `TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Sjekk om det er endringer i verdiene mellom siste versjon og ny fil
sjekk_endring_sum("prosjekt/data.parquet")

} # }
```
