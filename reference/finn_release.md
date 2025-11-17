# Finn en release fra logg basert på ulike kriterier

Funksjonen søker gjennom en loggfil for å finne en spesifikk release,
siste release, alle releaser innenfor et datointervall, alle releaser
for en gitt periode eller siste release innenfor en spesifisert periode.

## Usage

``` r
finn_release(
  release = NULL,
  dato = NULL,
  dato_start = NULL,
  dato_slutt = NULL,
  periode = NULL,
  arbeidsmappe,
  logg_fil = "versjonering_logg.json"
)
```

## Arguments

- release:

  En karakterstreng som spesifiserer hvilken release som skal hentes.
  Hvis `release = "siste"`, returneres den nyeste releasen basert på
  dato. Kombiner med `periode` for å hente den siste releasen innenfor
  en spesifikk periode.

- dato:

  En karakterstreng som spesifiserer en eksakt dato eller dato med tid
  (f.eks. "2023", `"2023-01-01"` eller `"2023-01-01 12:00:00"`). Hvis
  bare dato oppgis, returneres alle releaser fra denne dagen.

- dato_start:

  En karakterstreng som spesifiserer startdato for et datointervall
  (f.eks. "2023", `"2023-01-01"`). Brukes sammen med `dato_slutt` for å
  hente releaser innenfor et intervall.

- dato_slutt:

  En karakterstreng som spesifiserer sluttdato for et datointervall
  (f.eks. "2023", `"2023-01-31"`). Brukes sammen med `dato_start`.

- periode:

  En karakterstreng som spesifiserer en tidsperiode (f.eks. "2023",
  `"2023-Q1"`). Hvis `release = "siste"` er angitt, returneres den
  nyeste releasen innenfor den spesifiserte perioden.

- arbeidsmappe:

  En karakterstreng som representerer banen til arbeidsmappen der
  loggfilen ligger.

- logg_fil:

  En karakterstreng som representerer navnet på loggfilen. Standard er
  `"versjonering_logg.json"`.

## Value

Returnerer en liste over releaser som samsvarer med kriteriene, eller
`NULL` hvis ingen releaser ble funnet eller hvis loggfilen ikke
eksisterer.

## Details

Funksjonen leser inn en loggfil i JSON-format og søker etter en release
basert på de oppgitte kriteriene. Du kan søke etter:

- Spesifikk release: Angi `release` for å hente en bestemt release.

- Nyeste release: Angi `release = "siste"` for å hente den siste basert
  på tidsstempelet.

- Nyeste release innenfor en spesifikk periode: Angi både
  `release = "siste"` og `periode` for å hente den nyeste releasen
  innenfor perioden.

- Spesifikk dato: Angi `dato` for å hente releaser som ble logget på en
  gitt dato.

- Dato-intervall: Angi både `dato_start` og `dato_slutt` for å hente
  releaser innenfor et bestemt tidsrom.

Hvis flere kriterier er oppgitt (f.eks. både `release` og `dato`), vil
`release`-kriteriet ha prioritet.

## See also

[`logg_kjoring`](logg_kjoring.md) for å loggføre kjøringer i loggfilen.

## Examples

``` r
if (FALSE) { # \dontrun{
# Finn siste release
finn_release(release = "siste", arbeidsmappe = "prosjekt")

# Finn siste release i en spesifikk periode
finn_release(release = "siste", periode = "2021-Q1", arbeidsmappe = "prosjekt")

# Finn en spesifikk release
finn_release(release = "R1", arbeidsmappe = "prosjekt")

# Finn releaser fra en bestemt dato
finn_release(dato = "2023-01-01", arbeidsmappe = "prosjekt")

# Finn releaser innenfor et datointervall
finn_release(dato_start = "2023-01-01", dato_slutt = "2023-01-31", arbeidsmappe = "prosjekt")

} # }
```
