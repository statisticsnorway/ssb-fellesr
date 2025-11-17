# Funksjon for aa hente uttaksbeskrivelsen til en statistikkbanktabell

Funksjonen `statbank_uttaksbeskrivelse` henter uttaksbeskrivelsen til en
statistikkbanktabell.

## Usage

``` r
statbank_uttaksbeskrivelse(
  tabell_id,
  laste_bruker,
  ask = TRUE,
  username_encryptedpassword = "",
  boundary = 12345,
  db = NULL
)
```

## Arguments

- tabell_id:

  Karaktervektor med tabell ID til tabellen som det skal lastes opp data
  til.

- laste_bruker:

  Karaktervektor med seksjonens lastebruker.

- ask:

  Boolsk. Hvis `TRUE` blir man spurt om passord til lastebrukeren. Hvis
  `FALSE` maa `username_encryptedpassword` foerst vaere laget med
  funksjonen `statbank_encrypt_request`.

- username_encryptedpassword:

  Passord som er encryptert.

- boundary:

  Numerisk vektor med tallverdien som skiller de ulike filene i
  opplastningen. Trenger ikke aa endres.

## Examples

``` r
if (FALSE) { # \dontrun{
uttaksbeskrivelse <- statbank_uttaksbeskrivelse("13772", "LAST330")

uttaksbeskrivelse$Huvudtabell
uttaksbeskrivelse$TabellId
uttaksbeskrivelse$DeltabellTitler$Filnavn
uttaksbeskrivelse$DeltabellTitler$Filtext
# Variabler
uttaksbeskrivelse$deltabller$deltabell
uttaksbeskrivelse$deltabller$variabler
uttaksbeskrivelse$deltabller$statistikkvariabler
uttaksbeskrivelse$deltabller$null_prikk_missing
uttaksbeskrivelse$deltabller$eksempel_linje

# Kodelister
uttaksbeskrivelse$kodelister$kodeliste
uttaksbeskrivelse$kodelister$SumIALtTotalKode
uttaksbeskrivelse$kodelister$koder
} # }
```
