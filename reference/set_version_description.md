# Sett versjonsbeskrivelse i en DataDoc-fil

Legger til eller oppdaterer versjonsbeskrivelsen for et datasett i en
DataDoc-fil. Versjonsbeskrivelsen lagres med tilhørende språkkode.

## Usage

``` r
set_version_description(
  filsti_datadoc,
  version_description,
  language_code = "nb",
  overwrite = FALSE
)
```

## Arguments

- filsti_datadoc:

  Tekststreng. Filsti til DataDoc-filen som skal oppdateres.

- version_description:

  Tekststreng. Versjonsbeskrivelsen som skal lagres.

- language_code:

  Tekststreng. Språkkode for versjonsbeskrivelsen. Standard er `"nb"`.

- overwrite:

  Logisk verdi. Angir om en eksisterende versjonsbeskrivelse skal
  overskrives. Standard er `FALSE`.

## Value

Returnerer det oppdaterte DataDoc-objektet usynlig. Dersom en
eksisterende versjonsbeskrivelse ikke overskrives, returneres det
uendrede DataDoc-objektet usynlig.

## Details

Funksjonen leser DataDoc-filen, oppdaterer
`datadoc.dataset.version_description` og skriver den oppdaterte
dokumentasjonen tilbake til samme fil.

Versjonsbeskrivelsen lagres som en liste med `languageCode` og
`languageText`.

Dersom `version_description` allerede er utfylt og `overwrite = FALSE`,
blir filen ikke endret, og funksjonen gir en advarsel. Sett
`overwrite = TRUE` for å erstatte en eksisterende versjonsbeskrivelse.

## Examples

``` r
if (FALSE) { # \dontrun{
set_version_description(
  filsti_datadoc = "data/datasett__DOC.json",
  version_description = "Oppdaterte tall for 2026."
)

set_version_description(
  filsti_datadoc = "data/datasett__DOC.json",
  version_description = "Reviderte tall for 2026.",
  overwrite = TRUE
)

set_version_description(
  filsti_datadoc = "data/dataset__DOC.json",
  version_description = "Updated figures for 2026.",
  language_code = "en"
)
} # }
```
