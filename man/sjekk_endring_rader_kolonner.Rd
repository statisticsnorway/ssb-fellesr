% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/versjonering.R
\name{sjekk_endring_rader_kolonner}
\alias{sjekk_endring_rader_kolonner}
\title{Sjekk for endringer i rader og kolonner mellom siste og nyeste versjon av en fil}
\usage{
sjekk_endring_rader_kolonner(
  filsti,
  versjon_1 = "uversjonert",
  versjon_2 = "siste"
)
}
\arguments{
\item{filsti}{En karakterstreng som representerer filstien til den nyeste versjonen av filen.
Funksjonen finner selv den siste versjonerte filen ved å bruke \code{\link{lag_versjonert_filsti}}.}
}
\value{
Funksjonen returnerer en logisk verdi:
\itemize{
\item \code{TRUE}: Hvis det har skjedd endringer i antall rader, antall kolonner eller kolonnenavn mellom den siste versjonerte filen og den nye filen.
\item \code{FALSE}: Hvis det ikke er noen endringer i rader eller kolonner.
}
}
\description{
Funksjonen sammenligner antall rader og kolonner, samt kolonnenavnene, i den siste versjonerte filen
med tilsvarende i en ny fil for å se om det har skjedd endringer i struktur.
}
\details{
Funksjonen åpner den siste versjonerte filen i en gitt sti ved hjelp av \code{\link{lag_versjonert_filsti}} og sammenligner
antall rader og kolonner, samt kolonnene (navn og antall), med de i den nye filen som er angitt av \code{filsti}.
Det kontrolleres først om antallet rader eller kolonner har endret seg. Deretter kontrolleres det om navnene på kolonnene
har endret seg. Hvis det er en endring i enten antall rader, antall kolonner eller kolonnenavnene, returnerer funksjonen \code{TRUE}.
Hvis ingen endringer oppdages, returneres \code{FALSE}.
}
\examples{
\dontrun{
# Sjekk om det er endringer i rader eller kolonner mellom siste versjon og ny fil
sjekk_endring_rader_kolonner("prosjekt/data.parquet")

}
}
