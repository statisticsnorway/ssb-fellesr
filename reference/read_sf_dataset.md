# Read an Arrow multi-file dataset and create `sf` object

Read an Arrow multi-file dataset and create `sf` object

## Usage

``` r
read_sf_dataset(dataset, find_geom = FALSE)
```

## Arguments

- dataset:

  a `Dataset` object created by
  [`arrow::open_dataset`](https://arrow.apache.org/docs/r/reference/open_dataset.html)
  or an `arrow_dplyr_query`

- find_geom:

  logical. Only needed when returning a subset of columns. Should all
  available geometry columns be selected and added to to the dataset
  query without being named? Default is `FALSE` to require geometry
  column(s) to be selected specifically.

## Value

object of class [`sf`](https://r-spatial.github.io/sf/reference/sf.html)

## Details

This function is primarily for use after opening a dataset with
[`arrow::open_dataset`](https://arrow.apache.org/docs/r/reference/open_dataset.html).
Users can then query the `arrow Dataset` using `dplyr` methods such as
[`filter`](https://dplyr.tidyverse.org/reference/filter.html) or
[`select`](https://dplyr.tidyverse.org/reference/select.html). Passing
the resulting query to this function will parse the datasets and create
an `sf` object. The function expects consistent geographic metadata to
be stored with the dataset in order to create
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) objects.

## See also

[`open_dataset`](https://arrow.apache.org/docs/r/reference/open_dataset.html),
[`st_read`](https://r-spatial.github.io/sf/reference/st_read.html),
[`st_read_parquet`](st_read_parquet.md)

## Examples

``` r
# read spatial object
nc <- sf::st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)

# create random grouping
nc$group <- sample(1:3, nrow(nc), replace = TRUE)

# use dplyr to group the dataset. %>% also allowed
nc_g <- dplyr::group_by(nc, group)

# write out to parquet datasets
tf <- tempfile()  # create temporary location
on.exit(unlink(tf))
# partitioning determined by dplyr 'group_vars'
write_sf_dataset(nc_g, path = tf)

list.files(tf, recursive = TRUE)
#> [1] "group=1/part-0.parquet" "group=2/part-0.parquet" "group=3/part-0.parquet"

# open parquet files from dataset
ds <- arrow::open_dataset(tf)

# create a query. %>% also allowed
q <- dplyr::filter(ds, group == 1)

# read the dataset (piping syntax also works)
nc_d <- read_sf_dataset(dataset = q)

nc_d
#> Simple feature collection with 33 features and 15 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -83.98855 ymin: 33.94867 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
#> First 10 features:
#>     AREA PERIMETER CNTY_ CNTY_ID       NAME  FIPS FIPSNO CRESS_ID BIR74 SID74
#> 1  0.114     1.442  1825    1825       Ashe 37009  37009        5  1091     1
#> 2  0.070     2.968  1831    1831  Currituck 37053  37053       27   508     1
#> 3  0.124     1.428  1837    1837     Stokes 37169  37169       85  1612     1
#> 4  0.114     1.352  1838    1838    Caswell 37033  37033       17  1035     2
#> 5  0.153     1.616  1839    1839 Rockingham 37157  37157       79  4449    16
#> 6  0.072     1.085  1842    1842      Vance 37181  37181       91  2180     4
#> 7  0.064     1.213  1892    1892      Avery 37011  37011        6   781     0
#> 8  0.086     1.267  1893    1893     Yadkin 37197  37197       99  1269     1
#> 9  0.128     1.554  1897    1897   Franklin 37069  37069       35  1399     2
#> 10 0.142     1.640  1913    1913       Nash 37127  37127       64  4021     8
#>    NWBIR74 BIR79 SID79 NWBIR79 group                       geometry
#> 1       10  1364     0      19     1 MULTIPOLYGON (((-81.47276 3...
#> 2      123   830     2     145     1 MULTIPOLYGON (((-76.00897 3...
#> 3      160  2038     5     176     1 MULTIPOLYGON (((-80.02567 3...
#> 4      550  1253     2     597     1 MULTIPOLYGON (((-79.53051 3...
#> 5     1243  5386     5    1369     1 MULTIPOLYGON (((-79.53051 3...
#> 6     1179  2753     6    1492     1 MULTIPOLYGON (((-78.49252 3...
#> 7        4   977     0       5     1 MULTIPOLYGON (((-81.94135 3...
#> 8       65  1568     1      76     1 MULTIPOLYGON (((-80.49554 3...
#> 9      736  1863     0     950     1 MULTIPOLYGON (((-78.25455 3...
#> 10    1851  5189     7    2274     1 MULTIPOLYGON (((-78.18693 3...
plot(sf::st_geometry(nc_d))

```
