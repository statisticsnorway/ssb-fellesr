# av duplikater. Plassholdere som `^BedrNavn` og teksten `<.>` beholdes.

av duplikater. Plassholdere som `^BedrNavn` og teksten `<.>` beholdes.

## Usage

``` r
add_column_labels_from_file(
  data,
  kodelister,
  variabel_kolonne = "varnavn",
  betydning_kolonne = "sporsmaal_text",
  clean_labels = FALSE
)
```

## Value

Datasettet med kolonneetiketter. Verdier, datatyper og verdietiketter
beholdes.

## Examples

``` r
data <- data.frame(nrinnled = c("1", "2"))
kodelister <- data.frame(varnavn = "nrinnled", sporsmaal_text = "Status på IO")
resultat <- add_column_labels_from_file(data, kodelister)
labelled::var_label(resultat$nrinnled)
#> [1] "Status på IO"

# Fjern HTML fra kolonneetikettene:
kodelister$sporsmaal_text <- "Status<br><gt>på IO</gt>"
resultat <- add_column_labels_from_file(data, kodelister, clean_labels = TRUE)
labelled::var_label(resultat$nrinnled) # "Status på IO"
#> [1] "Status på IO"

# Det tidligere formatet:
kodelister <- data.frame(variabel = "nrinnled", variable_text = "Status på IO")
resultat <- add_column_labels_from_file(
  data, kodelister,
  variabel_kolonne = "variabel",
  betydning_kolonne = "variable_text"
)
```
