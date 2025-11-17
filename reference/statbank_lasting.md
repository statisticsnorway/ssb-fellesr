# Funksjon for aa laste opp data fra Jupyterlab til Statistikkbanken

Funksjonen `statbank_lasting` laster opp data fra Jupyterlab til
Statistikkbanken. Det er mulig aa laste opp objekter direkte eller ved
aa oppgi filsti til en eller flere .parquet-filer som er lagret i en
Google Cloud Storage bucket (DAPLA) eller paa Linux (produksjonssonen).

## Usage

``` r
statbank_lasting(
  lastefil,
  lastefilsti = "",
  tabell_id,
  laste_bruker,
  publiseringsdato,
  initialer = initialer_funk(),
  autooverskriv = 1,
  autogodkjenn = 2,
  boundary = 12345,
  ask = TRUE,
  username_encryptedpassword = "",
  validering = TRUE,
  db = NULL
)
```

## Arguments

- lastefil:

  Objekt eller en liste med objekter som inneholder dataene som skal
  lastes opp. Det er ogsaa mulig aa oppgi en karaktervektor med filnavn
  til .parquet-filer lagret i en Google Cloud Storage bucket (DAPLA)
  eller paa Linux (produksjonssonen). Filstien oppgis under
  `lastefilsti`.

- lastefilsti:

  Karaktervektor med filsti til en Google Cloud Storage bucket hvor
  lastefilene (.parquet) er plassert. Dersom man laster opp objekter
  direkte kan denne staa blank.

- tabell_id:

  Karaktervektor med tabell ID til tabellen som det skal lastes opp data
  til.

- laste_bruker:

  Karaktervektor med seksjonens lastebruker.

- publiseringsdato:

  Karaktervektor med publiseringsdato. Formatet skal vaere "YYYY-MM-DD".

- initialer:

  Karaktervektor med initialer til personen som laster opp dataene (og
  som mottar e-post med lastelogg). Initialer hentes fra miljoevariabel
  i Jupyter saa dersom det ikke er en annen person enn brukeren som
  kjoerer programmet som skal motta e-post kan denne staa blank.

- autooverskriv:

  Numerisk vektor. Standardverdi er satt til 1.

- autogodkjenn:

  Numerisk vektor. 0: manuell, 1: automatisk (umiddelbart), 2: JIT
  (just-in-time). Standardverdi er satt til 2.

- boundary:

  Numerisk vektor med tallverdien som skiller de ulike filene i
  opplastningen. Trenger ikke aa endres.

- ask:

  Boolsk. Hvis `TRUE` blir man spurt om passord til lastebrukeren. Hvis
  `FALSE` maa `username_encryptedpassword` foerst vaere laget med
  funksjonen `statbank_encrypt_request`.

- username_encryptedpassword:

  Passord som er encryptert.

- validering:

  Boolsk. Valideringssjekk for aa oppdage vanlige lastefeil foer
  tabellen lastes opp.

## Examples

``` r
if (FALSE) { # \dontrun{
transfer_log <- statbank_lasting(lastefil = roykalderkj1,
                                 tabell_id = "05307",
                                 laste_bruker = "LAST330",
                                 publiseringsdato = "2022-12-31")
transfer_log

transfer_log <- statbank_lasting(lastefil = "roykalderkj1.parquet",
                                 lastefilsti = "ssb-prod-spesh-personell-data-kilde",
                                 tabell_id = "05307",
                                 laste_bruker = "LAST330",
                                 publiseringsdato = "2022-12-31")
transfer_log
} # }
```
