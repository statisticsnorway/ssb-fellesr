% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DAPLA_statbank.R
\encoding{UTF-8}
\name{statbank_uttaksbeskrivelse}
\alias{statbank_uttaksbeskrivelse}
\title{Funksjon for aa hente uttaksbeskrivelsen til en statistikkbanktabell}
\usage{
statbank_uttaksbeskrivelse(
  tabell_id,
  laste_bruker,
  ask = TRUE,
  username_encryptedpassword = "",
  boundary = 12345,
  db = NULL
)
}
\arguments{
\item{tabell_id}{Karaktervektor med tabell ID til tabellen som det skal lastes opp data til.}

\item{laste_bruker}{Karaktervektor med seksjonens lastebruker.}

\item{ask}{Boolsk. Hvis \code{TRUE} blir man spurt om passord til lastebrukeren. Hvis \code{FALSE} maa \code{username_encryptedpassword} foerst vaere laget med funksjonen \code{statbank_encrypt_request}.}

\item{username_encryptedpassword}{Passord som er encryptert.}

\item{boundary}{Numerisk vektor med tallverdien som skiller de ulike filene i opplastningen. Trenger ikke aa endres.}
}
\description{
Funksjonen \code{statbank_uttaksbeskrivelse} henter uttaksbeskrivelsen til en statistikkbanktabell.
}
\examples{
\dontrun{
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
}
}
