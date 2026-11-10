# Legg til verdietiketter fra en lokal kodeliste

Kobler kolonnenavn i data til variabelkolonnen i kodelisten. Som
standard brukes `variabel` til koblingen, `varnavn` som underliggende
verdi og `sporsmaal_text` som verdietikett. Kolonnenavnene kan
tilpasses. Taggene `<rt>` og `</rt>` fjernes fra etiketten.

## Usage

``` r
add_value_labels_from_file(
  data,
  kodelister,
  quiet = TRUE,
  variabel_kolonne = "variabel",
  kode_kolonne = "varnavn",
  betydning_kolonne = "sporsmaal_text"
)
```

## Arguments

- data:

  En data.frame eller tibble som skal få etiketter.

- kodelister:

  En data.frame eller tibble med kodelistene, eller en tekststreng med
  filstien til en Parquet-fil i samme format.

- quiet:

  Logisk verdi. Hvis FALSE, gis en advarsel når en variabel konverteres
  til tekst. Standard er TRUE.

- variabel_kolonne:

  Navnet på kolonnen med variabelnavnene som skal matches mot
  kolonnenavn i `data`. Standard er `"variabel"`.

- kode_kolonne:

  Navnet på kolonnen med svarkodene som skal få verdietiketter. Standard
  er `"varnavn"`.

- betydning_kolonne:

  Navnet på kolonnen med teksten til hver svarkode. Standard er
  `"sporsmaal_text"`.

## Value

Datasettet med verdietiketter på de aktuelle variablene.

## Details

Som i add_value_labels() behandles koder som tekst. Ledende nuller
bevares når kodene er lagret som tekst. Eksisterende verdietiketter
erstattes for variabler med en gyldig kodeliste. Variabler uten treff og
rader uten kode eller etikett hoppes over. To ulike etiketter for samme
variabel og kode gir en feil. Eksisterende kolonneetiketter bevares.

## Examples

``` r
data <- data.frame(nrinnled = c("1", "2"))
kodelister <- data.frame(
  variabel = c("nrinnled", "nrinnled"),
  varnavn = c("1", "2"),
  sporsmaal_text = c("intervju", "<rt>Frafall</rt>")
)
resultat <- add_value_labels_from_file(data, kodelister)
labelled::val_labels(resultat$nrinnled)
#> intervju  Frafall 
#>      "1"      "2" 
```
