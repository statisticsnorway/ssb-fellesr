# Lag logg for versjonering

Funksjonen oppdaterer en loggfil med informasjon om en ny kjøring der
flere filstier blir versjonert. Den sjekker først om det allerede finnes
en logg, og hvis det ikke er noen endringer siden forrige kjøring, vil
loggfilen ikke bli oppdatert.

## Usage

``` r
logg_kjoring(
  resultat,
  logg_fil = "versjonering_logg.json",
  arbeidsmappe,
  periode
)
```

## Arguments

- resultat:

  En liste som inneholder resultatet av kjøringen fra
  [`versjoner_filer`](versjoner_filer.md). Hver oppføring representerer
  filstiens oppdaterte versjon.

- logg_fil:

  En karakterstreng som representerer navnet på loggfilen. Standard er
  `"versjonering_logg.json"`.

- arbeidsmappe:

  En karakterstreng som representerer banen til arbeidsmappen der
  loggfilen skal lagres.

## Value

Returnerer den oppdaterte loggen som en liste. Hvis ingen endringer ble
oppdaget, returnerer funksjonen uten å oppdatere loggen.

## Details

Funksjonen logger hver kjøring av
[`versjoner_filer`](versjoner_filer.md) ved å sjekke om loggfilen
allerede eksisterer. Hvis det er første gang loggfilen opprettes, vil en
ny logg bli laget. Hvis det allerede finnes en logg, vil funksjonen
sjekke om resultatet av den nye kjøringen er identisk med den siste
kjøringen. Hvis ingen endringer er oppdaget (dvs. ingen oppdaterte
versjoner av filene), vil loggen ikke bli oppdatert. Hvis det er en
forskjell i resultatene, vil funksjonen legge til en ny kjøring med en
unik ID og tidsstempel.

Objektet `resultat` fra [`versjoner_filer`](versjoner_filer.md) er en
liste der hver filsti er oppdatert til å representere siste versjon.
Funksjonen skriver denne informasjonen til loggen hvis en endring
oppdages.

## See also

[`versjoner_filer`](versjoner_filer.md) for generering av resultatet.

## Examples

``` r
if (FALSE) { # \dontrun{
# Versjoner filstier og loggfør kjøringen
filstier <- c("data1.parquet", "data2.parquet")
resultat <- versjoner_filer(filstier)
logg <- logg_kjoring(resultat, arbeidsmappe = "prosjekt/logg")

# Hvis ingen endringer siden forrige kjøring, blir loggfilen ikke oppdatert
logg <- logg_kjoring(resultat, arbeidsmappe = "prosjekt/logg")

} # }
```
