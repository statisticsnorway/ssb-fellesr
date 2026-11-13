# Sett versjonsbeskrivelse i en Datadoc-fil

Legger til eller oppdaterer versjonsbeskrivelsen for et datasett i en
Datadoc-fil. Versjonsbeskrivelsen lagres med tilhørende språkkode.

## Usage

``` r
set_version_description(
  filsti,
  version_description,
  language = "nb",
  overwrite = FALSE
)
```

## Arguments

- filsti:

  Tekststreng. Filsti til Datadoc-filen som skal oppdateres.

- version_description:

  Tekststreng. Versjonsbeskrivelsen som skal lagres.

- language:

  Tekststreng. Språkkode for versjonsbeskrivelsen. Standard er `"nb"`.

- overwrite:

  Logisk verdi. Angir om en eksisterende versjonsbeskrivelse skal
  overskrives. Standard er `FALSE`.

## Value

Returnerer det oppdaterte Datadoc-objektet usynlig. Dersom en
eksisterende versjonsbeskrivelse ikke overskrives, returneres det
uendrede Datadoc-objektet usynlig.

## Details

Funksjonen leser Datadoc-filen, oppdaterer
`datadoc.dataset.version_description` og skriver den oppdaterte
dokumentasjonen tilbake til samme fil.

Versjonsbeskrivelsen lagres som en liste med `languageCode` og
`languageText`.

Bare teksten på valgt språk vurderes og oppdateres; andre språk
beholdes. Dersom teksten på valgt språk allerede er utfylt og
`overwrite = FALSE`, blir filen ikke endret, og funksjonen gir en
advarsel. Sett `overwrite = TRUE` for å erstatte en eksisterende
versjonsbeskrivelse.

## Examples

``` r
if (FALSE) { # \dontrun{
set_version_description(
  filsti = "data/datasett__DOC.json",
  version_description = "Oppdaterte tall for 2026."
)

set_version_description(
  filsti = "data/datasett__DOC.json",
  version_description = "Reviderte tall for 2026.",
  overwrite = TRUE
)

set_version_description(
  filsti = "data/dataset__DOC.json",
  version_description = "Updated figures for 2026.",
  language = "en"
)
} # }
```
