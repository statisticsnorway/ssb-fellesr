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
#> Simple feature collection with 37 features and 15 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.95718 ymax: 36.58965
#> Geodetic CRS:  NAD27
#> First 10 features:
#>     AREA PERIMETER CNTY_ CNTY_ID        NAME  FIPS FIPSNO CRESS_ID BIR74 SID74
#> 1  0.114     1.442  1825    1825        Ashe 37009  37009        5  1091     1
#> 2  0.153     2.206  1832    1832 Northampton 37131  37131       66  1421     9
#> 3  0.062     1.547  1834    1834      Camden 37029  37029       15   286     0
#> 4  0.118     1.421  1836    1836      Warren 37185  37185       93   968     4
#> 5  0.114     1.352  1838    1838     Caswell 37033  37033       17  1035     2
#> 6  0.153     1.616  1839    1839  Rockingham 37157  37157       79  4449    16
#> 7  0.143     1.663  1840    1840   Granville 37077  37077       39  1671     4
#> 8  0.109     1.325  1841    1841      Person 37145  37145       73  1556     4
#> 9  0.190     2.204  1846    1846     Halifax 37083  37083       42  3608    18
#> 10 0.044     1.158  1887    1887      Chowan 37041  37041       21   751     1
#>    NWBIR74 BIR79 SID79 NWBIR79 group                       geometry
#> 1       10  1364     0      19     1 MULTIPOLYGON (((-81.47276 3...
#> 2     1066  1606     3    1197     1 MULTIPOLYGON (((-77.21767 3...
#> 3      115   350     2     139     1 MULTIPOLYGON (((-76.00897 3...
#> 4      748  1190     2     844     1 MULTIPOLYGON (((-78.30876 3...
#> 5      550  1253     2     597     1 MULTIPOLYGON (((-79.53051 3...
#> 6     1243  5386     5    1369     1 MULTIPOLYGON (((-79.53051 3...
#> 7      930  2074     4    1058     1 MULTIPOLYGON (((-78.74912 3...
#> 8      613  1790     4     650     1 MULTIPOLYGON (((-78.8068 36...
#> 9     2365  4463    17    2980     1 MULTIPOLYGON (((-77.33221 3...
#> 10     368   899     1     491     1 MULTIPOLYGON (((-76.68874 3...
plot(sf::st_geometry(nc_d))

```
