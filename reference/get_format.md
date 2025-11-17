# Laster et SSB-format objekt fra JSON

Laster et SSB-format objekt fra JSON

## Usage

``` r
get_format(filepath, is_range_format)
```

## Arguments

- filepath:

  Sti til JSON-filen.

- is_range_format:

  Boolsk verdi som angir om formatet er intervallbasert.

## Value

Et `ssb_format`-objekt.

## Examples

``` r
if (FALSE) { # \dontrun{
format <- get_format("path/to/file.json", TRUE)
} # }
```
