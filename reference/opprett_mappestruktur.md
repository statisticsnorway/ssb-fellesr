# Opprett mappestruktur

Funksjonen oppretter en standard mappestruktur basert på en filsti til
en arbeidsmappe, og returnerer en liste som inneholder stiene til hver
undermappe. Det er også mulig å spesifisere en valgfri parameter
`periode` for å opprette en undermappe med det navnet i hver av
hovedmappene.

## Usage

``` r
opprett_mappestruktur(
  arbeidsmappe,
  mapper = c("inndata", "klargjorte-data", "statistikk", "utdata"),
  periode = NULL
)
```

## Arguments

- arbeidsmappe:

  En karakterstreng som spesifiserer hovedmappen der de nye undermappene
  skal opprettes. Hvis denne stien slutter med en skråstrek (`/`),
  fjernes den automatisk. Dersom `arbeidsmappe` ikke eksisterer, vil
  funksjonen automatisk opprette den.

- mapper:

  En karaktervektor som inneholder navnene på undermappene som skal
  opprettes. Standardverdien er
  `c("inndata", "klargjorte-data", "statistikk", "utdata")`. Det er
  mulig å legge til flere mapper (f.eks. "oppdrag").

- periode:

  En valgfri karakterstreng som spesifiserer en undermappe som opprettes
  i hver av hovedmappene. Hvis denne ikke er spesifisert (default er
  `NULL`), opprettes ingen slike undermapper.

## Value

En liste der hver nøkkel er navnet på en undermappe (med understrek i
stedet for bindestreker), og verdiene er de fulle stiene til de
respektive mappene.

## Details

Denne funksjonen går gjennom en liste av mapper, og for hver mappe:

- Fjerner en eventuell avsluttende skråstrek (`/`) fra hovedmappen.

- Oppretter `arbeidsmappe` dersom den ikke allerede finnes.

- Oppretter undermappen inne i arbeidsmappen dersom den ikke allerede
  eksisterer.

- Hvis `periode` er spesifisert, oppretter funksjonen også en undermappe
  med navnet spesifisert av `periode` i hver hovedmappe.

- Erstatt bindestreker i undermappenavn med understrek for å lage
  gyldige variabelnavn.

- Samler hver undermappe-sti i en liste og returnerer denne listen.

Variabelnavnene i listen har formatet `mappe_navn_mappe`, der eventuelle
bindestreker i mappe-navn erstattes med understrek.

## Examples

``` r
# Opprett standard mappestruktur i en mappe "prosjekt"
mapper <- opprett_mappestruktur("prosjekt")
print(mapper)
#> $inndata_mappe
#> [1] "prosjekt/inndata/"
#> 
#> $klargjorte_data_mappe
#> [1] "prosjekt/klargjorte-data/"
#> 
#> $statistikk_mappe
#> [1] "prosjekt/statistikk/"
#> 
#> $utdata_mappe
#> [1] "prosjekt/utdata/"
#> 

# Opprett en egendefinert mappestruktur
mapper <- opprett_mappestruktur("analyse", mapper = c("data", "output", "figurer"))
print(mapper)
#> $data_mappe
#> [1] "analyse/data/"
#> 
#> $output_mappe
#> [1] "analyse/output/"
#> 
#> $figurer_mappe
#> [1] "analyse/figurer/"
#> 

# Opprett mappestruktur med en spesifikk periode (f.eks. "2024" eller "2024-Q1")
mapper <- opprett_mappestruktur("prosjekt", periode = "2024")
print(mapper)
#> $inndata_mappe
#> [1] "prosjekt/inndata/2024/"
#> 
#> $klargjorte_data_mappe
#> [1] "prosjekt/klargjorte-data/2024/"
#> 
#> $statistikk_mappe
#> [1] "prosjekt/statistikk/2024/"
#> 
#> $utdata_mappe
#> [1] "prosjekt/utdata/2024/"
#> 

# Eksempel på hvordan stiene fra listen kan tilordnes til individuelle variabler
mappestruktur <- opprett_mappestruktur("prosjekt")

inndata_mappe <- mappestruktur$inndata_mappe
klargjorte_data_mappe <- mappestruktur$klargjorte_data_mappe
statistikk_mappe <- mappestruktur$statistikk_mappe
utdata_mappe <- mappestruktur$utdata_mappe
```
