# Legg til verdietiketter fra KLASS

Legger til verdietiketter på variabler i et datasett basert på
klassifikasjoner registrert i DataDoc eller Vardef.

## Usage

``` r
add_value_labels(data, filsti, language = "nb", quiet = TRUE)
```

## Arguments

- data:

  Et datasett, for eksempel en `data.frame` eller tibble, som skal få
  lagt til verdietiketter.

- filsti:

  En tekststreng med filsti til DataDoc-filen eller den tilhørende
  datafilen.

- language:

  En tekststreng som angir språk som skal brukes ved henting av
  metadata. Standard er `"nb"`.

- quiet:

  Logisk verdi. Dersom `TRUE`, undertrykkes alle advarsler som oppstår
  under kjøringen. Standard er `FALSE`.

## Value

Datasettet `data` med verdietiketter lagt til for variabler der en
gyldig klassifikasjon ble funnet. Variabler kan bli konvertert til
`character` dersom de opprinnelig har en annen datatype enn kodene fra
KLASS.

## Details

For hver variabel hentes KLASS-ID fra DataDoc dersom denne finnes.
Dersom KLASS-ID ikke er registrert i DataDoc, brukes eventuell
klassifikasjon fra Vardef. KLASS-ID fra DataDoc har dermed forrang.

Kodelisten hentes fra KLASS ved hjelp av
[`klassR::get_klass()`](https://statisticsnorway.github.io/ssb-klassr/reference/get_klass.html).
Dersom `contains_data_until` er registrert i DataDoc, brukes denne
datoen ved henting av kodelisten.

Variabler som ikke finnes i `data`, som mangler en gyldig KLASS-ID,
eller der kodelisten ikke kan hentes, hoppes over med en advarsel.

Kodene fra KLASS behandles som tekst. Dersom en variabel i `data` ikke
er av typen `character`, konverteres den til `character` før
verdietikettene legges til. Dette gir som standard en advarsel.

Ved å sette `quiet = TRUE` undertrykkes alle advarsler som oppstår under
kjøringen av funksjonen. Feil (`error`) undertrykkes ikke.

Funksjonen bruker
[`variables_with_classification_uri()`](variables_with_classification_uri.md)
til å finne variabler med tilknyttede klassifikasjoner.

Dersom både DataDoc og Vardef inneholder en klassifikasjon for samme
variabel, brukes klassifikasjonen fra DataDoc.

Verdietikettene legges til med
[`labelled::set_value_labels()`](https://larmarange.github.io/labelled/reference/val_labels.html),
der navnene fra KLASS brukes som etiketter og kodene som underliggende
verdier.

## Examples

``` r
if (FALSE) { # \dontrun{
data_med_labels <- add_value_labels(
  data = data,
  filsti = "data__DOC.json"
)

# Undertrykk advarsler
data_med_labels <- add_value_labels(
  data = data,
  filsti = "data__DOC.json",
  quiet = TRUE
)
} # }
```
