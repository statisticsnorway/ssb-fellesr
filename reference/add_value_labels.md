# Legg til verdietiketter fra en Datadoc-fil

Henter kodelister som er referert fra Datadoc eller Vardef. En direkte
KLASS-referanse i Datadoc har forrang. Språk og datoperiode brukes ved
oppslag; dagens dato brukes hvis sluttdato mangler.

## Usage

``` r
add_value_labels(
  data,
  filsti,
  language = "nb",
  quiet = TRUE,
  clean_labels = TRUE
)
```

## Arguments

- data:

  En data.frame eller tibble med numeriske eller tekstbaserte koder.

- filsti:

  Filsti til Datadoc-filen eller den tilhørende Parquet-filen.

- language:

  Språkkode: `"nb"` (standard), `"nn"` eller `"en"`.

- quiet:

  Med `TRUE` (standard) skjules statusmeldinger. Advarsler om manglende
  etiketter eller mislykkede oppslag og feil vises fortsatt.

- clean_labels:

  Med `TRUE` (standard) fjernes HTML- og formateringstagger fra
  etikettene og mellomrom ryddes opp.

## Value

Datasettet med verdietiketter på variabler med treff.

## Details

Datatypen, verdiene og kolonneetikettene beholdes. Kodene i kodelisten
tilpasses variabelens datatype; uforenlige koder gir feil. Numeriske
variabler får numeriske verdietikettkoder. Tekstkoder beholder ledende
nuller. Eksisterende verdietiketter erstattes ved gyldig treff.
Observerte verdier uten etikett beholdes og gir advarsel, også med
`quiet = TRUE`. Vanlige manglende verdier (`NA`) utelates fra
kontrollen.

## Examples

``` r
if (FALSE) { # \dontrun{
resultat <- add_value_labels(data, "data__DOC.json")
} # }
```
