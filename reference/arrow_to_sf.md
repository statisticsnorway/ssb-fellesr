# Helper function to convert 'data.frame' to `sf`

Helper function to convert 'data.frame' to `sf`

## Usage

``` r
arrow_to_sf(tbl, metadata)
```

## Arguments

- tbl:

  `data.frame` from reading an Arrow dataset

- metadata:

  `list` of validated geo metadata

## Value

object of `sf` with CRS and geometry columns
