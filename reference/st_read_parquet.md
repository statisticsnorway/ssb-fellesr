# Read a Parquet file to `sf` object

Read a Parquet file. Uses standard metadata information to identify
geometry columns and coordinate reference system information.

## Usage

``` r
st_read_parquet(dsn, col_select = NULL, props = NULL, ...)
```

## Arguments

- dsn:

  character file path to a data source

- col_select:

  A character vector of column names to keep. Default is `NULL` which
  returns all columns

- props:

  Now deprecated in
  [`read_parquet`](https://arrow.apache.org/docs/r/reference/read_parquet.html).

- ...:

  additional parameters to pass to
  [`ParquetFileReader`](https://arrow.apache.org/docs/r/reference/ParquetFileReader.html)

## Value

object of class [`sf`](https://r-spatial.github.io/sf/reference/sf.html)

## Details

Reference for the metadata used:
<https://github.com/geopandas/geo-arrow-spec>. These are standard with
the Python `GeoPandas` library.

## See also

[`read_parquet`](https://arrow.apache.org/docs/r/reference/read_parquet.html),
[`st_read`](https://r-spatial.github.io/sf/reference/st_read.html)

## Examples

``` r
# load Natural Earth low-res dataset.
# Created in Python with GeoPandas.to_parquet()
path <- system.file("extdata", package = "sfarrow")

world <- st_read_parquet(file.path(path, "world.parquet"))
#> Error in metadata$columns[[col]]$crs$id: $ operator is invalid for atomic vectors

world
#> Error: object 'world' not found
plot(sf::st_geometry(world))
#> Error: object 'world' not found
```
