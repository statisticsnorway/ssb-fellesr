# Legg til kolonneetiketter fra en metadatafil eller metadatatabell

Kobler variabelnavn i metadataene mot kolonnenavn i datasettet og legger
til beskrivende kolonneetiketter med
[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html).
Som standard brukes `varnavn` til koblingen og `sporsmaal_text` som
kolonneetikett.

## Usage

``` r
add_column_labels_from_file(
  data,
  kodelister,
  variabel_kolonne = "varnavn",
  betydning_kolonne = "sporsmaal_text",
  clean_labels = TRUE,
  show_labels = TRUE
)
```

## Arguments

- data:

  En data.frame eller tibble som skal få kolonneetiketter.

- kodelister:

  En data.frame eller tibble med variabelnavn og kolonneetiketter, eller
  en tekststreng med filstien til en Parquet-fil med disse
  opplysningene.

- variabel_kolonne:

  En tekststreng med navnet på kolonnen i `kodelister` som inneholder
  variabelnavnene. Disse kobles mot kolonnenavnene i `data`. Standard er
  `"varnavn"`.

- betydning_kolonne:

  En tekststreng med navnet på kolonnen i `kodelister` som inneholder
  kolonneetikettene, for eksempel spørsmålsteksten. Standard er
  `"sporsmaal_text"`.

- clean_labels:

  Logisk verdi. Standard er `TRUE`. Med `TRUE` fjernes HTML- og
  formateringstagger fra kolonneetikettene, og mellomrom ryddes opp.
  Teksten mellom taggene beholdes.

- show_labels:

  En logisk verdi som angir om kolonneetikettene også skal vises ved
  utskrift av datasettet. Når verdien er `TRUE`, behandles datasettet
  med [`show_column_labels()`](show_column_labels.md). Standardverdien
  er `TRUE`.

## Value

Datasettet med kolonneetiketter på variablene som har treff.

## Details

Bare de to angitte kolonnene i `kodelister` kreves. Funksjonen kan
derfor også brukes for variabler uten svarkoder.

Variabelnavnene i metadataene trimmes for innledende og avsluttende
mellomrom før kobling. Koblingen skiller mellom store og små bokstaver.
Variabler i metadataene som ikke finnes i `data`, ignoreres. Kolonner
uten treff eller med manglende eller tom etikett beholder eventuell
eksisterende kolonneetikett.

Innledende og avsluttende mellomrom fjernes fra etikettene uavhengig av
`clean_labels`. Ved treff med en ikke-tom etikett erstattes en eventuell
eksisterende kolonneetikett.

Med `clean_labels = TRUE` erstattes HTML- og formateringstagger som
`<br>`, `<gt>`, `<nt>` og `<rt>` med mellomrom. Gjentatte mellomrom,
tabulatorer og linjeskift samles til ett mellomrom. Mellomrom foran
tegnsetting fjernes, men mellomrom foran tre prikker beholdes.
Intervjuinstruksjoner og annen tekst mellom taggene beholdes.
Plassholdere som `^BedrNavn` og teksten `<.>` beholdes også.
HTML-entiteter som `&amp;` og `&nbsp;` dekodes ikke.

Gjentatte, identiske etiketter for samme variabel er tillatt. Ulike,
ikke-tomme etiketter for samme variabel gir en feil. Kontroll av
duplikater skjer etter trimming og eventuell rensing.

Funksjonen endrer bare kolonneetikettene. Kolonnenavn, verdier,
datatyper og verdietiketter beholdes. Ingen filer skrives.

## Examples

``` r
data <- data.frame(nrinnled = c("1", "2"))
kodelister <- data.frame(
  varnavn = "nrinnled",
  sporsmaal_text = "Status på IO"
)

resultat <- add_column_labels_from_file(data, kodelister)
labelled::var_label(resultat$nrinnled)
#> [1] "Status på IO"

# Fjern HTML fra kolonneetikettene:
kodelister$sporsmaal_text <- "Status<br><gt>på IO</gt>"
resultat <- add_column_labels_from_file(
  data,
  kodelister,
  clean_labels = TRUE
)
labelled::var_label(resultat$nrinnled) # "Status på IO"
#> [1] "Status på IO"

# Bruk andre kolonnenavn i metadataene:
kodelister <- data.frame(
  variabel = "nrinnled",
  variable_text = "Status på IO"
)
resultat <- add_column_labels_from_file(
  data,
  kodelister,
  variabel_kolonne = "variabel",
  betydning_kolonne = "variable_text"
)
```
