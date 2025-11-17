# Write `sf` object to Parquet file

Convert a simple features spatial object from `sf` and write to a
Parquet file using
[`write_parquet`](https://arrow.apache.org/docs/r/reference/write_parquet.html).
Geometry columns (type `sfc`) are converted to well-known binary (WKB)
format.

## Usage

``` r
st_write_parquet(obj, dsn, compression = "zstd", ...)
```

## Arguments

- obj:

  object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html)

- dsn:

  data source name. A path and file name with .parquet extension

- compression:

  algorithm. Default "zstd", recommended for geoparquet

- ...:

  additional options to pass to
  [`write_parquet`](https://arrow.apache.org/docs/r/reference/write_parquet.html)

## Value

`obj` invisibly

## See also

[`write_parquet`](https://arrow.apache.org/docs/r/reference/write_parquet.html)

## Examples

``` r
# read spatial object
nc <- sf::st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)

# create temp file
tf <- tempfile(fileext = '.parquet')
on.exit(unlink(tf))

# write out object
st_write_parquet(obj = nc, dsn = tf)

# In Python, read the new file with geopandas.read_parquet(...)
# read back into R
nc_p <- st_read_parquet(tf)
```
