# SSB ggplot theme

## Visualisering med ggplot

R-pakken `ggplot` er et populær verktøy for visualisering av data. Det
er utrolig mange forskjellige type figurer som kan lages. Her skal vi
bruke `ggplot` et datasett som heter `mtcars` for å vise noen
visualisering med ggplot. Vi starter med å lage en stolpediagram som et
eksempel:

``` r
library(ggplot2)

ggplot(mtcars, aes(x=cyl, fill=as.factor(cyl))) + 
  geom_bar( )
```

![](vignette_SSB_theme_files/figure-html/unnamed-chunk-1-1.png)

## Bruk av theme_ssb

Vi har laget et tema (theme) for SSB for `ggplot` som heter
**`theme_ssb`**. Dette er tilgjengelig i [`fellesr`
pakken](https://statisticsnorway.github.io/fellesr/index.html). Theme
endre bakgrunn farge, font og tekst størrelse til å ligne på det vi
publiserer i notat og dokument-serie. Merk at fargene på stolpene ikke
endres!

``` r
library(fellesr)

ggplot(mtcars, aes(x=cyl, fill=as.factor(cyl))) + 
  geom_bar( ) + 
  theme_ssb()
```

    ## Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
    ## ℹ Please use the `linewidth` argument instead.
    ## ℹ The deprecated feature was likely used in the fellesr package.
    ##   Please report the issue to the authors.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](vignette_SSB_theme_files/figure-html/unnamed-chunk-2-1.png)

## Endre til SSB farge i figurer

For å endre fargene i en figur har vi laget noen SSB funksjoner som
fungere med `ggplot`.

#### Kategoriske data - scale_fill_ssb()

Vi kan endre til SSB farger ved bruk av
[`scale_fill_ssb()`](../reference/scale_fill_ssb.md)

``` r
ggplot(mtcars, aes(x=as.factor(cyl), fill=as.factor(cyl) )) + 
  geom_bar( ) +
  scale_fill_ssb() +
  theme_ssb()
```

![](vignette_SSB_theme_files/figure-html/unnamed-chunk-3-1.png)

Vi kan spesifisere fargevalg inn i \`scale_fill_ssb() funksjonen. Velg
mellom: ‘main’ (default), ‘greens’, ‘blues’, ‘primary’, ‘secondary’,
‘shade1’, ‘shade2’,‘shade3’, ‘shade4’, eller ‘shade5’.

``` r
ggplot(mtcars, aes(x=as.factor(cyl), fill=as.factor(cyl) )) + 
  geom_bar( ) +
  scale_fill_ssb('blues') +
  theme_ssb()
```

![](vignette_SSB_theme_files/figure-html/unnamed-chunk-4-1.png)

#### Numeriske data - scale_color_ssb()

For kontinuous numeriske data kan vi bruke
[`scale_color_ssb()`](../reference/scale_colour_ssb.md) for å endre
farger. For example

``` r
ggplot(mtcars, aes(x=hp, y=mpg, color=disp)) + 
  geom_point( ) +
  scale_colour_ssb('greens') +
  theme_ssb()
```

![](vignette_SSB_theme_files/figure-html/unnamed-chunk-5-1.png)

## Mer

Det er mulig å spesifisere farger enda mer. For mer detaljer og
eksempeler om hvordan å hente ut SSB farge fra KLASS ser dokumentasjon
om [SSB
fargepalett](https://statisticsnorway.github.io/fellesr/articles/vignette_SSB_fargepalett.html)
