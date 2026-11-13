# Legg til verdietiketter fra en metadatafil eller metadatatabell

Kobler variabelnavn og svarkoder i metadataene mot datasettet og legger
til verdietiketter med
[`labelled::set_value_labels()`](https://larmarange.github.io/labelled/reference/val_labels.html).

## Usage

``` r
add_value_labels_from_file(
  data,
  kodelister,
  quiet = TRUE,
  variabel_kolonne = "varnavn",
  kode_kolonne = "kode",
  betydning_kolonne = "tekst",
  clean_labels = TRUE
)
```

## Arguments

- data:

  En data.frame eller tibble som skal få verdietiketter.

- kodelister:

  En data.frame, tibble eller filsti til en Parquet-fil med
  variabelnavn, svarkoder og tilhørende etiketter.

- quiet:

  Med `TRUE` (standard) skjules statusmeldinger. Advarsler om observerte
  verdier uten etikett og feil vises fortsatt.

- variabel_kolonne:

  Kolonnen med variabelnavn. Standard er `"varnavn"`.

- kode_kolonne:

  Kolonnen med svarkoder. Standard er `"kode"`.

- betydning_kolonne:

  Kolonnen med svarkodetekst. Standard er `"tekst"`.

- clean_labels:

  Med `TRUE` (standard) fjernes HTML- og formateringstagger, og
  mellomrom ryddes opp. Med `FALSE` beholdes taggene, også `<rt>`.
  Innledende og avsluttende mellomrom trimmes alltid.

## Value

Datasettet med verdietiketter på variablene som har treff.

## Details

Datatypen og verdiene i data beholdes. Numeriske variabler får numeriske
etikettkoder; tekstvariabler beholder tekstkoder og ledende nuller.
Koder som ikke kan tilpasses variabelens datatype, gir feil. Faktorer,
datoer, logiske variabler og andre spesialklasser støttes ikke av denne
funksjonen og må håndteres eksplisitt før etiketter legges på.

Bare variabler med treff behandles. Rader uten kode eller etikett
ignoreres. Identiske oppslag slås sammen; motstridende etiketter for
samme kode gir feil. Eksisterende verdietiketter erstattes ved gyldig
treff. Eksisterende kolonneetiketter beholdes. Observerte verdier uten
etikett beholdes og gir advarsel, også med `quiet = TRUE`. Vanlige `NA`
utelates fra kontrollen.

Rensingen beholder tekst mellom tagger, `^BedrNavn` og `<.>`.
HTML-entiteter som `&amp;` og `&nbsp;` dekodes ikke. Ingen filer
skrives.

## Examples

``` r
data <- data.frame(nrinnled = c(1L, 2L))
kodelister <- data.frame(
  varnavn = c("nrinnled", "nrinnled"), kode = c("1", "2"),
  tekst = c("Intervju", "<rt>Frafall</rt>")
)
resultat <- add_value_labels_from_file(data, kodelister)
labelled::val_labels(resultat$nrinnled)
#> Intervju  Frafall 
#>        1        2 
is.integer(resultat$nrinnled)
#> [1] TRUE

# Det tidligere formatet angis eksplisitt:
tidligere <- data.frame(variabel = "nrinnled", varnavn = "1",
                        sporsmaal_text = "Intervju")
resultat <- add_value_labels_from_file(
  data[1, , drop = FALSE], tidligere,
  variabel_kolonne = "variabel", kode_kolonne = "varnavn",
  betydning_kolonne = "sporsmaal_text"
)
```
