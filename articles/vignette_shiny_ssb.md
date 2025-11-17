# Kjøre shiny og Esquisse i Jupyter

## Kjøre shiny

R pakken [shiny](https://shiny.rstudio.com/) gjøre det lett å lage
interactive sider og dashboards. Appene kan kjøres fra Jupyter men stien
som det pekes til er for tiden feil internt i SSB. Som en kjapt løsning
har vi laget to “wrapper” funksjoner (**`runApp_ssb`** og
**`runExample`**) som kan brukes som gjøre shiny for SSB brukerne. Her
er de to wrapper funksjonene for shiny:

| Funksjon                                             | Beskrivelse                 |
|------------------------------------------------------|-----------------------------|
| [`runApp_ssb()`](../reference/runApp_ssb.md)         | Kjør en shiny app.          |
| [`runExample_ssb()`](../reference/runExample_ssb.md) | Kjør en eksempel shiny App. |

For å kjøre et eksempel shiny app:

``` r
library(shiny)
library(fellesr)

runExample_ssb("01_hello")
```

## Kjøre esquisser

R pakken
[esquisser](https://cran.r-project.org/web/packages/esquisse/vignettes/get-started.html)
er en interactive verktøy for å lage ggplot. Dette kan kjøre i Jupyte,
men igjen er stien som det pekes til feil. Wrapper funksjonen
**`esquisser_ssb`** peker til ritkig sted for appen. for example:

``` r
library(esquisse)
library(fellesr)
data(iris)

esquisser_ssb(iris)
```
