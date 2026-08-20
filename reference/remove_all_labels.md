# Fjern alle etiketter fra et datasett

Fjerner verdietiketter og kolonneetiketter fra alle variabler i et
datasett.

## Usage

``` r
remove_all_labels(data)
```

## Arguments

- data:

  Et lokalt eller lazy datasett som etikettene skal fjernes fra.

## Value

En `data.frame` uten verdietiketter eller kolonneetiketter.

## Details

Dataene hentes først inn i minnet med
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html).
Funksjonen kan derfor brukes på både lokale datasett og lazy tabeller
som støttes av `dplyr`.

Verdietiketter fjernes med
[`haven::zap_labels()`](https://haven.tidyverse.org/reference/zap_labels.html),
mens kolonneetiketter fjernes med
[`haven::zap_label()`](https://haven.tidyverse.org/reference/zap_label.html).

For variabler av typen `haven_labelled_spss` vil brukerdefinerte
manglende verdier som standard omgjøres til vanlige `NA`-verdier når
verdietikettene fjernes.

Andre metadataattributter, som formater og kolonnebredder, fjernes ikke.

## See also

[`haven::zap_labels()`](https://haven.tidyverse.org/reference/zap_labels.html)
for å fjerne verdietiketter og
[`haven::zap_label()`](https://haven.tidyverse.org/reference/zap_label.html)
for å fjerne kolonneetiketter.

## Examples

``` r
data <- data.frame(
  kjoenn = labelled::labelled(
    c(1, 2, 1),
    labels = c(
      Mann = 1,
      Kvinne = 2
    )
  ),
  alder = c(35, 42, 28)
)

labelled::var_label(data$kjoenn) <- "Kjønn"
labelled::var_label(data$alder) <- "Alder i år"

labelled::var_label(
  data,
  unlist = FALSE
)
#> $kjoenn
#> [1] "Kjønn"
#> 
#> $alder
#> [1] "Alder i år"
#> 

data_uten_labels <- remove_all_labels(data)

labelled::var_label(
  data_uten_labels,
  unlist = FALSE
)
#> $kjoenn
#> NULL
#> 
#> $alder
#> NULL
#> 
```
