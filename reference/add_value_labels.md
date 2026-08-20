# Legg til verdietiketter fra KLASS

Legger til verdietiketter på variabler i et datasett basert på
kodelister fra SSBs klassifikasjonssystem KLASS.

## Usage

``` r
add_value_labels(data, filsti, language = "nb")
```

## Arguments

- data:

  Et datasett som skal få lagt til verdietiketter.

- filsti:

  En tekststreng med filstien til Parquet-filen som datasettet er lest
  fra. Filstien brukes til å finne den tilhørende DataDoc-filen.

- language:

  En tekststreng med språkkoden som skal brukes ved uthenting av
  språkavhengig metadata. Standardverdien er `"nb"`.

## Value

Datasettet som ble oppgitt i `data`, med verdietiketter lagt til på
variabler med en gyldig kodeliste i KLASS. Variabler som ikke kan
behandles, returneres uendret.

## Details

Funksjonen finner variabler med en tilknyttet kodeliste gjennom
Datadoc-metadata eller en tilknyttet variabeldefinisjon. Kodelistene
hentes fra KLASS, og navnene i kodelisten legges til som verdietiketter
ved hjelp av
[`labelled::set_value_labels()`](https://larmarange.github.io/labelled/reference/val_labels.html).

Funksjonen bruker
[`variables_with_classification_uri()`](variables_with_classification_uri.md)
til å finne variabler med en tilknyttet kodeliste.

Dersom en KLASS-ID finnes både direkte i Datadoc-filen og i den
tilknyttede variabeldefinisjonen, brukes ID-en fra Datadoc-filen. ID-en
fra variabeldefinisjonen brukes dersom Datadoc-filen ikke inneholder en
KLASS-ID.

Kodelisten hentes med
[`klassR::get_klass()`](https://statisticsnorway.github.io/ssb-klassr/reference/get_klass.html).
Verdien i `contains_data_until` brukes som dato for oppslaget i KLASS.

En variabel hoppes over med en advarsel dersom:

- variabelen ikke finnes i `data`;

- det ikke finnes en KLASS-ID;

- KLASS-ID-en inneholder andre tegn enn sifre; eller

- kodelisten ikke kan hentes fra KLASS.

Verdiene i kodelistens kolonne `code` brukes som variabelverdier, mens
verdiene i kolonnen `name` brukes som verdietiketter.

## See also

[`variables_with_classification_uri()`](variables_with_classification_uri.md)
for å finne variabler med kodelister og
[`labelled::set_value_labels()`](https://larmarange.github.io/labelled/reference/val_labels.html)
for å legge til verdietiketter.

## Examples

``` r
if (FALSE) { # \dontrun{
data_med_labels <- add_value_labels(
  data = personell,
  filsti = "/buckets/data/personell_v1.parquet"
)

data_med_labels <- add_value_labels(
  data = personell,
  filsti = "/buckets/data/personell_v1.parquet",
  language = "en"
)
} # }
```
