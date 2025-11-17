# Finn versjonsnummer fra et filnavn

Funksjonen finner og returnerer versjonsnummeret fra et filnavn som
følger en navnestandard der versjonsnummeret er spesifisert etter `_v`,
etterfulgt av et eller flere siffer. Dersom filen ikke er versjonert i
henhold til denne standarden, returnerer funksjonen `FALSE`.

## Usage

``` r
finn_versjon(fil)
```

## Arguments

- fil:

  En karakterstreng som representerer filnavnet hvor funksjonen skal
  søke etter et versjonsnummer.

## Value

Versjonsnummeret som et heltall (integer) dersom det finnes, ellers
`FALSE` dersom filen ikke følger versjonsstandard.

## Details

Funksjonen forventer at filnavnet inneholder et versjonsnummer etter
mønsteret `_v` fulgt av ett eller flere siffer. For eksempel vil et
filnavn som `rapport_v2.csv` returnere `2` som versjonsnummer.

Dersom filnavnet ikke inneholder et versjonsnummer i dette formatet, vil
funksjonen returnere `FALSE` og gi en advarsel om at filen ikke er
versjonert i henhold til navnestandarden. Se
[navnestandarden](https://manual.dapla.ssb.no/statistikkere/navnestandard.html)
for mer informasjon.

Funksjonen gir også en advarsel dersom filnavnet inneholder stor "V" i
stedet for liten "v", siden det forventes at versjonsnummeret skal
markeres med små bokstaver (`_v`).

## Examples

``` r
# Finn versjonsnummer fra et gyldig filnavn
finn_versjon("rapport_v2.csv")  # Returnerer 2
#> [1] 2

# Filnavn uten versjonsnummer (advarsel vil gis)
finn_versjon("rapport.csv")  # Returnerer advarsel og FALSE
#> Error in finn_versjon("rapport.csv"): Filen er ikke versjonert i henhold til navnestandarden. Se https://manual.dapla.ssb.no/statistikkere/navnestandard.html for mer informasjon.

# Filnavn med stor "V" (advarsel vil gis)
finn_versjon("rapport_V2.csv")  # Returnerer advarsel og FALSE
#> Error in finn_versjon("rapport_V2.csv"): Filnavnet inneholder stor 'V' i stedet for liten 'v'. Funksjonen forventer '_v' med små bokstaver.
```
