# Create standardised geo metadata for Parquet files

Create standardised geo metadata for Parquet files

## Usage

``` r
create_metadata(df)
```

## Arguments

- df:

  object of class `sf`

## Value

JSON formatted list with geo-metadata

## Details

Reference for metadata standard:
<https://github.com/geopandas/geo-arrow-spec>. This is compatible with
`GeoPandas` Parquet files.
