# Eksporter data frame til tekstfil og lag SAS-program for innlesning

Funksjonen eksporterer en data frame til en tekstfil som kan leses av
SAS, og genererer samtidig et SAS-program for å lese inn dataene. Det
genererte SAS-programmet kan justeres for å inkludere spesifikke
SAS-innstillinger.

## Usage

``` r
write_sas_prog(
  data,
  filsti,
  filsti_innlesning = NULL,
  filnavn = NULL,
  libname = "wd"
)
```

## Arguments

- data:

  Datasettet som skal eksporteres til SAS-format. Dette kan være en
  data.frame eller tibble.

- filsti:

  En karakterstreng som spesifiserer banen der filene skal lagres (både
  datafilen og SAS-programmet).

- filsti_innlesning:

  En karakterstreng som spesifiserer banen der filen skal leses fra fra
  SAS-programmet. Dersom filen lages på Dapla og skal kjøres i SAS i
  produksjonsonen må mappen filen skal legges i spesifiseres her.

- filnavn:

  En karakterstreng som spesifiserer navnet på filene som skal
  genereres. Hvis ikke angitt, brukes navnet på objektet `data`.

- libname:

  En karakterstreng som spesifiserer navnet på SAS-libnavn som skal
  brukes i SAS-programmet. Standard er `"wd"`.

## Value

Ingen returverdi. Funksjonen lagrer en .txt-fil og et SAS-program på den
spesifiserte filstien. SAS-programmet må deretter åpnes og kjøres i SAS
og da blir det lagret en .sas7bdat-fil på samme område.

## Details

Funksjonen konverterer først alle faktorer i datasettet til
karakterstrenger og erstatter manglende verdier med tomme strenger.
Deretter lagres dataene som en `.txt`-fil og et tilsvarende SAS-program
som `.sas`-fil ved hjelp av
[`foreign::write.foreign`](https://rdrr.io/pkg/foreign/man/write.foreign.html).
Funksjonen leser så inn det genererte SAS-programmet og gjør
justeringer, som å legge til `encoding="utf-8"` og å sette opp et
SAS-libnavn.

## Examples

``` r
# Eksporter et datasett til SAS-format og generer et innlesningsprogram
write_sas_prog(mtcars, filsti = "output/")
#> Warning: cannot open file 'output/mtcars.txt': No such file or directory
#> Error in file(file, ifelse(append, "a", "w")): cannot open the connection

# Eksporter med spesifisert filnavn og libname
write_sas_prog(mtcars, filsti = "output/", filnavn = "biler", libname = "mylib")
#> Warning: cannot open file 'output/biler.txt': No such file or directory
#> Error in file(file, ifelse(append, "a", "w")): cannot open the connection
```
