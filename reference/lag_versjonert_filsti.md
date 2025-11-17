# Lag versjonert filsti

Funksjonen genererer en filsti for en versjonert fil basert på et gitt
filnavn. Den kan enten finne siste versjon av en fil eller lage en ny
versjon av filen.

## Usage

``` r
lag_versjonert_filsti(fil, versjon = "siste")
```

## Arguments

- fil:

  En karakterstreng som representerer filstien til filen som skal
  versjoneres.

- versjon:

  Angir hvordan versjonsnummeret skal håndteres. Kan være `"siste"` for
  å hente siste versjon, `"ny"` for å lage en ny versjon, eller et
  heltall som spesifiserer en bestemt versjon. Standardverdien er
  `"siste"`.

## Value

En karakterstreng som representerer filstien til den versjonerte filen.
Funksjonen stopper med en feilmelding hvis den ikke kan finne eller
opprette en versjonert filsti i henhold til de angitte kriteriene.

## Details

Funksjonen tar et filnavn og sjekker om det er versjonert i henhold til
et mønster der versjonsnummeret er angitt etter `_v`, etterfulgt av ett
eller flere siffer (f.eks. `_v1`, `_v2`). Den kan enten hente siste
versjon, lage en ny versjon, eller returnere en spesifikk versjon av
filen hvis det er angitt.

Hvis filen ikke eksisterer i den angitte mappen, eller hvis den ikke
inneholder en gyldig versjon, vil funksjonen stoppe med en feilmelding.

Når `versjon = "ny"`, vil funksjonen generere et nytt filnavn med
versjonsnummer som er én høyere enn det høyeste eksisterende nummeret.
Hvis `versjon = "siste"`, returneres stien til den siste versjonen av
filen. Hvis et spesifikt versjonsnummer er oppgitt som et heltall, vil
funksjonen returnere stien til denne versjonen hvis den eksisterer.

## Examples

``` r
if (FALSE) { # \dontrun{
# Lag en ny versjon av filen "data.parquet" i mappen "prosjekt"
lag_versjonert_filsti("prosjekt/data_v1.parquet", versjon = "ny")

# Hent stien til siste versjon av filen "data.parquet"
lag_versjonert_filsti("prosjekt/data_v1.parquet", versjon = "siste")

# Hent stien til en spesifikk versjon (versjon 3) av filen "data.parquet"
lag_versjonert_filsti("prosjekt/data_v1.parquet", versjon = 3)

} # }
```
