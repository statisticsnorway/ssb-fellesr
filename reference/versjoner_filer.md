# Versjonere filer

Funksjonen oppdaterer filstier for flere filer ved å lage en ny versjon
av filene dersom det er oppdaget endringer, eller returnerer stien til
den siste versjonerte filen hvis ingen endringer er oppdaget. Funksjonen
logger også resultatene av kjøringen ved hjelp av
[`logg_kjoring`](logg_kjoring.md), som lagrer informasjon om versjonerte
filer og kjøringsattributter som `kjoring_id` og `dato_tid` i en egen
loggfil.

## Usage

``` r
versjoner_filer(
  filstier,
  logg_fil = "versjonering_logg.json",
  arbeidsmappe,
  periode
)
```

## Arguments

- filstier:

  En karaktervektor som inneholder navnene på filstier som skal
  versjoneres. Funksjonen antar at filene er registrert som variable i
  det globale miljøet.

- logg_fil:

  Navnet på loggfilen der kjøringsinformasjon og versjonerte filer
  lagres. Standard er "versjonering_logg.json".

- arbeidsmappe:

  Stien til arbeidsmappen der loggfilen lagres.

## Value

En liste som inneholder filstiene til de siste versjonene av de
versjonerte filene.

## Details

Funksjonen itererer over en liste med filstier og sjekker først om en
versjonert fil allerede eksisterer. Hvis det ikke finnes en versjonert
fil opprettes versjon 1 (v1) automatisk. Dersom versjonerte filer
allerede eksisterer sjekkes det om det har skjedd endringer i kolonner,
datatyper eller verdier for hver fil (mot den siste versjonerte filen)
ved hjelp av [`sjekk_endring`](sjekk_endring.md). Hvis det er oppdaget
endringer, vil funksjonen opprette en ny versjon av filen. Hvis ingen
endringer oppdages, returneres stien til den siste versjonerte filen.
Etter hver kjøring, oppdateres loggfilen med informasjon om hvilke
versjoner av filene som har blitt brukt i kjøringen samt metadata om
kjøringen (`kjoring_id` og `dato_tid`) ved hjelp av
[`logg_kjoring`](logg_kjoring.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Lag versjonerte filstier for en liste med filnavn
filstier <- c("filsti1", "filsti2")
versjoner_filer(filstier)

} # }
```
