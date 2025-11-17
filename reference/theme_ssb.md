# SSB theme for ggplot

SSB theme for ggplot

## Usage

``` r
theme_ssb()
```

## Value

theme object

## Examples

``` r
library(ggplot2)
ggplot(mtcars, aes(x=cyl, fill=as.factor(cyl))) +
geom_bar( ) +
theme_ssb()
#> Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
#> ℹ Please use the `linewidth` argument instead.
#> ℹ The deprecated feature was likely used in the fellesr package.
#>   Please report the issue to the authors.
```
