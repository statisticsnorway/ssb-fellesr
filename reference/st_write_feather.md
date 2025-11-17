# Write `sf` object to Feather file

Convert a simple features spatial object from `sf` and write to a
Feather file using
[`write_feather`](https://arrow.apache.org/docs/r/reference/write_feather.html).
Geometry columns (type `sfc`) are converted to well-known binary (WKB)
format.

## Usage

``` r
st_write_feather(obj, dsn, ...)
```

## Arguments

- obj:

  object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html)

- dsn:

  data source name. A path and file name with .parquet extension

- ...:

  additional options to pass to
  [`write_feather`](https://arrow.apache.org/docs/r/reference/write_feather.html)

## Value

`obj` invisibly

## See also

[`write_feather`](https://arrow.apache.org/docs/r/reference/write_feather.html)

## Examples

``` r
# read spatial object
nc <- sf::st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)

# create temp file
tf <- tempfile(fileext = '.feather')
on.exit(unlink(tf))

# write out object
st_write_feather(obj = nc, dsn = tf)

# In Python, read the new file with geopandas.read_feather(...)
# read back into R
nc_f <- st_read_feather(tf)
```
