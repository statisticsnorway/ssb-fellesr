# Write `sf` object to an Arrow multi-file dataset

Write `sf` object to an Arrow multi-file dataset

## Usage

``` r
write_sf_dataset(
  obj,
  path,
  format = "parquet",
  partitioning = dplyr::group_vars(obj),
  ...
)
```

## Arguments

- obj:

  object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html)

- path:

  string path referencing a directory for the output

- format:

  output file format ("parquet" or "feather")

- partitioning:

  character vector of columns in `obj` for grouping or the
  [`dplyr::group_vars`](https://dplyr.tidyverse.org/reference/group_data.html)

- ...:

  additional arguments and options passed to
  [`arrow::write_dataset`](https://arrow.apache.org/docs/r/reference/write_dataset.html)

## Value

`obj` invisibly

## Details

Translate an `sf` spatial object to `data.frame` with WKB geometry
columns and then write to an `arrow` dataset with partitioning. Allows
for `dplyr` grouped datasets (using
[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)) and
uses those variables to define partitions.

## See also

[`write_dataset`](https://arrow.apache.org/docs/r/reference/write_dataset.html),
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
#> Simple feature collection with 29 features and 15 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -83.9547 ymin: 33.88199 xmax: -75.7637 ymax: 36.58965
#> Geodetic CRS:  NAD27
#> First 10 features:
#>     AREA PERIMETER CNTY_ CNTY_ID        NAME  FIPS FIPSNO CRESS_ID BIR74 SID74
#> 1  0.114     1.442  1825    1825        Ashe 37009  37009        5  1091     1
#> 2  0.153     2.206  1832    1832 Northampton 37131  37131       66  1421     9
#> 3  0.097     1.670  1833    1833    Hertford 37091  37091       46  1452     7
#> 4  0.081     1.288  1880    1880     Watauga 37189  37189       95  1323     1
#> 5  0.063     1.000  1881    1881  Perquimans 37143  37143       72   484     1
#> 6  0.086     1.267  1893    1893      Yadkin 37197  37197       99  1269     1
#> 7  0.128     1.554  1897    1897    Franklin 37069  37069       35  1399     2
#> 8  0.170     1.680  1903    1903    Guilford 37081  37081       41 16184    23
#> 9  0.142     1.640  1913    1913        Nash 37127  37127       64  4021     8
#> 10 0.059     1.319  1927    1927    Mitchell 37121  37121       61   671     0
#>    NWBIR74 BIR79 SID79 NWBIR79 group                       geometry
#> 1       10  1364     0      19     1 MULTIPOLYGON (((-81.47276 3...
#> 2     1066  1606     3    1197     1 MULTIPOLYGON (((-77.21767 3...
#> 3      954  1838     5    1237     1 MULTIPOLYGON (((-76.74506 3...
#> 4       17  1775     1      33     1 MULTIPOLYGON (((-81.80622 3...
#> 5      230   676     0     310     1 MULTIPOLYGON (((-76.48053 3...
#> 6       65  1568     1      76     1 MULTIPOLYGON (((-80.49554 3...
#> 7      736  1863     0     950     1 MULTIPOLYGON (((-78.25455 3...
#> 8     5483 20543    38    7089     1 MULTIPOLYGON (((-79.53782 3...
#> 9     1851  5189     7    2274     1 MULTIPOLYGON (((-78.18693 3...
#> 10       1   919     2       4     1 MULTIPOLYGON (((-82.11885 3...
plot(sf::st_geometry(nc_d))

```
