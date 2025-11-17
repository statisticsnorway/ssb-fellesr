# Convert `sfc` geometry columns into a WKB binary format

Convert `sfc` geometry columns into a WKB binary format

## Usage

``` r
encode_wkb(df)
```

## Arguments

- df:

  `sf` object

## Value

`data.frame` with binary geometry column(s)

## Details

Allows for more than one geometry column in `sfc` format
