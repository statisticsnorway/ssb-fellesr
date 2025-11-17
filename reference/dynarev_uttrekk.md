# Uttrekk fra Dynarev til R

Funksjonen `dynarev_uttrekk` henter inn data fra Dynarev til R så lenge
man oppgir delregisternummer (og ev. skjemanavn). Det er kun enheter som
er satt som aktive (AKTIV = 1) som blir lastet ned. Det er mulig å hente
skjemadata (variablene i selve skjemaet) og SFU-data
(enhetsinformasjon). Man kan også gjennomføre dublettsjekker og velge
mellom reviderte data og rådata.

## Usage

``` r
dynarev_uttrekk(
  delregnr,
  skjema = T,
  enhets_type = c("FRTK", "BEDR"),
  skjema_cols = T,
  sfu_cols = F,
  skjema_sfu_merge = F,
  dublettsjekk = F,
  con_ask = T,
  raadata = F
)
```

## Arguments

- delregnr:

  Numerisk vektor med delregisternummer.

- skjema:

  Boolsk/karaktervektor med skjemanavn. Hvis TRUE returneres alle skjema
  i valgt delregister.

- enhets_type:

  Karaktervektor med enhetstype (f.eks. BEDR og/eller FRTK).

- skjema_cols:

  Boolsk/karaktervektor. Hvis TRUE henter man alle variabler fra skjema.
  Hvis FALSE henter man ikke skjemadata, kun SFU-data (hvis
  `skjema_cols = TRUE`). Det er også mulig å lage en vektor/liste av
  variabelnavn, f.eks. `skjema_cols = c("variabel1", "variabel2")`,
  dersom man kun vil ha utvalgte variabler fra skjema.

- sfu_cols:

  Boolsk/karaktervektor. Hvis TRUE blir alle variabler fra SFU for valgt
  delregister og skjema inkludert. For å kun velge én eller flere
  variabler fra SFU skrives disse i en vektor/liste, f.eks.
  `sfu_cols = c("variabel1", "variabel2")`.

- skjema_sfu_merge:

  Boolsk. Hvis TRUE blir skjemadataene og SFU-dataene merget. Dersom
  FALSE blir dataene hentet som to separate datasett i en liste; \[1\]
  skjemadata og \[2\] SFU-data.

- dublettsjekk:

  Boolsk/karaktervektor. Hvis TRUE sjekkes det for dubletter i
  skjemadata etter ENHETS_ID. Dersom man ønsker å sjekke for dubletter
  etter én eller flere selvvalgte variabler skrives disse i en vektor,
  f.eks. `dublettsjekk = c("variabel1", "variabel2")`. Liste med to
  datasett returneres; \[1\] skjemadata og \[2\] dublettdata (dersom det
  finnes dubletter, hvis ikke er denne blank).

- con_ask:

  Boolsk/karaktervektor. Hvis TRUE får man opp en boks som spør etter
  Oracle-passord. Hvis FALSE spørres det ikke om passord. Ved å skrive
  `dynarev_uttrekk(con_ask = "con")` får man opp en boks som spør etter
  Oracle-passord og kun koblingen mot Oracle blir returnert (ikke data).
  Denne kan brukes dersom man skal lese inn flere skjema etter hverandre
  for å unngå å skrive inn passordet flere ganger.

- raadata:

  Boolsk. Hvis FALSE returneres reviderte data (fra FELT_VERDI), TRUE
  returnerer rådata.

## Value

Objekt (data.frame) med valgt data fra Dynarev, dersom enten
`skjema_cols` eller `sfu_cols = TRUE`. Om både `skjema_cols` og
`sfu_cols = TRUE` returneres en liste med to datasett \[1\]
`skjema_cols` og \[2\] `sfu_cols`. Om `skjema_cols`, `sfu_cols` og
`skjema_sfu_merge = TRUE` returneres det ett objekt (data.frame). Dersom
`con_ask = "con"` returneres kun kobling mot Oracle.

## Examples

``` r
if (FALSE) { # \dontrun{
dynarev <- dynarev_uttrekk(delregnr = 2421,
                          skjema = "HELSE41",
                          skjema_cols = T)
} # }
```
