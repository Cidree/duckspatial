# Calculates the length of geometries

Calculates the length of geometries from a DuckDB table or a `sf` object
Returns the result as an `sf` object with a length column or creates a
new table in the database. Note: Length units depend on the CRS of the
input geometries (e.g., meters for projected CRS, or degrees for
geographic CRS).

## Usage

``` r
ddbs_length(
  x,
  conn = NULL,
  name = NULL,
  new_column = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- x:

  An `sf` spatial object. Alternatively, it can be a string with the
  name of a table with geometry column within the DuckDB database
  `conn`. Data is returned from this object.

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

- new_column:

  Name of the new column to create on the input data. If NULL, the
  function will return a vector with the result

- crs:

  The coordinates reference system of the data. Specify if the data
  doesn't have a `crs_column`, and you know the CRS.

- crs_column:

  a character string of length one specifying the column storing the CRS
  (created automatically by
  [`ddbs_write_vector`](https://cidree.github.io/duckspatial/reference/ddbs_write_vector.md)).
  Set to `NULL` if absent.

- overwrite:

  Boolean. whether to overwrite the existing table if it exists.
  Defaults to `FALSE`. This argument is ignored when `name` is `NULL`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

an `sf` object or `TRUE` (invisibly) for table creation

## Examples

``` r
## load packages
library(duckspatial)
library(sf)

# create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

## read data
rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial"))
#> Reading layer `rivers' from data source 
#>   `/home/runner/work/_temp/Library/duckspatial/spatial/rivers.geojson' 
#>   using driver `GeoJSON'
#> Simple feature collection with 100 features and 1 field
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 2766878 ymin: 2222357 xmax: 3578648 ymax: 2459939
#> Projected CRS: ETRS89-extended / LAEA Europe

## store in duckdb
ddbs_write_vector(conn, rivers_sf, "rivers")
#> ✔ Table rivers successfully imported

## calculate length (returns sf object with length column)
ddbs_length("rivers", conn)
#>   [1]  34232.5206  29223.3570  22409.6485  27511.0723  14817.0831  40245.7499
#>   [7]  37421.6029  38787.4428  48753.1900  28956.8522  25919.4653  36597.7982
#>  [13]  36540.8556  49333.9047  55617.2751  41800.1935  38226.8089  53867.9142
#>  [19]  46182.5143  48424.6809  33236.1217 106821.9772  86470.3410  38880.7081
#>  [25]  39549.6752  26118.7215  17747.3794  16524.1617  22831.1831  53384.3893
#>  [31]  17442.6563   6789.6030  12059.9283  26856.6566  63429.8095  13767.1861
#>  [37]  23738.5150  35694.8459  19646.1745  10029.0179  13968.4457  41043.7032
#>  [43]  48714.4882  37133.2198  43436.8699  23642.8664  14212.4786   8445.8296
#>  [49]  97428.9007  33310.8820  10918.3777  73866.4040   5397.1562  31303.2187
#>  [55]  46783.2287  42065.3367  23982.1267   6855.8189  97390.1661  46243.7102
#>  [61]  15542.0333  31208.0987  30773.7428  40455.5784  10754.9779  39136.8449
#>  [67]  65827.5937   3693.5384  17902.1442   4323.2089 120300.4900   1959.2291
#>  [73]  60752.0324   9065.0840   6452.4055  42056.1917  18207.3070  39043.6943
#>  [79]  10745.6292  24112.5296   9128.6008  18416.8778  13895.3803  16908.1064
#>  [85]   6310.0966   4381.0841   7922.9705  12546.7703  11175.7684  31755.4344
#>  [91]  36926.0258  48195.5038  40373.2595  10658.1011  25550.2617  11961.7774
#>  [97]  39554.3265   4249.1463   8271.2878    619.3338

## calculate length with custom column name
ddbs_length("rivers", conn, new_column = "length_meters")
#> ✔ Query successful
#> Simple feature collection with 100 features and 2 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 2766878 ymin: 2222357 xmax: 3578648 ymax: 2459939
#> Projected CRS: ETRS89-extended / LAEA Europe
#> First 10 features:
#>         RIVER_NAME length_meters                       geometry
#> 1       Rio Garona      34232.52 LINESTRING (3563589 2240292...
#> 2    Bidasoa Ibaia      29223.36 LINESTRING (3373540 2299136...
#> 3     Baztan Ibaia      22409.65 LINESTRING (3373540 2299136...
#> 4       Rio Urumea      27511.07 LINESTRING (3359001 2301989...
#> 5       Oria Ibaia      14817.08 LINESTRING (3346144 2312252...
#> 6       Oria Ibaia      40245.75 LINESTRING (3346144 2312252...
#> 7      Urola Ibaia      37421.60 LINESTRING (3317563 2294789...
#> 8       Deba Ibaia      38787.44 LINESTRING (3300422 2296792...
#> 9      Rio Nervion      48753.19 LINESTRING (3263336 2300537...
#> 10 Ibaizabal Ibaia      28956.85 LINESTRING (3299820 2310528...

## create a new table with length calculations
ddbs_length("rivers", conn, name = "rivers_with_length")
#>   [1]  34232.5206  29223.3570  22409.6485  27511.0723  14817.0831  40245.7499
#>   [7]  37421.6029  38787.4428  48753.1900  28956.8522  25919.4653  36597.7982
#>  [13]  36540.8556  49333.9047  55617.2751  41800.1935  38226.8089  53867.9142
#>  [19]  46182.5143  48424.6809  33236.1217 106821.9772  86470.3410  38880.7081
#>  [25]  39549.6752  26118.7215  17747.3794  16524.1617  22831.1831  53384.3893
#>  [31]  17442.6563   6789.6030  12059.9283  26856.6566  63429.8095  13767.1861
#>  [37]  23738.5150  35694.8459  19646.1745  10029.0179  13968.4457  41043.7032
#>  [43]  48714.4882  37133.2198  43436.8699  23642.8664  14212.4786   8445.8296
#>  [49]  97428.9007  33310.8820  10918.3777  73866.4040   5397.1562  31303.2187
#>  [55]  46783.2287  42065.3367  23982.1267   6855.8189  97390.1661  46243.7102
#>  [61]  15542.0333  31208.0987  30773.7428  40455.5784  10754.9779  39136.8449
#>  [67]  65827.5937   3693.5384  17902.1442   4323.2089 120300.4900   1959.2291
#>  [73]  60752.0324   9065.0840   6452.4055  42056.1917  18207.3070  39043.6943
#>  [79]  10745.6292  24112.5296   9128.6008  18416.8778  13895.3803  16908.1064
#>  [85]   6310.0966   4381.0841   7922.9705  12546.7703  11175.7684  31755.4344
#>  [91]  36926.0258  48195.5038  40373.2595  10658.1011  25550.2617  11961.7774
#>  [97]  39554.3265   4249.1463   8271.2878    619.3338

## calculate length in a sf object (without a connection)
ddbs_length(rivers_sf)
#>   [1]  34232.5206  29223.3570  22409.6485  27511.0723  14817.0831  40245.7499
#>   [7]  37421.6029  38787.4428  48753.1900  28956.8522  25919.4653  36597.7982
#>  [13]  36540.8556  49333.9047  55617.2751  41800.1935  38226.8089  53867.9142
#>  [19]  46182.5143  48424.6809  33236.1217 106821.9772  86470.3410  38880.7081
#>  [25]  39549.6752  26118.7215  17747.3794  16524.1617  22831.1831  53384.3893
#>  [31]  17442.6563   6789.6030  12059.9283  26856.6566  63429.8095  13767.1861
#>  [37]  23738.5150  35694.8459  19646.1745  10029.0179  13968.4457  41043.7032
#>  [43]  48714.4882  37133.2198  43436.8699  23642.8664  14212.4786   8445.8296
#>  [49]  97428.9007  33310.8820  10918.3777  73866.4040   5397.1562  31303.2187
#>  [55]  46783.2287  42065.3367  23982.1267   6855.8189  97390.1661  46243.7102
#>  [61]  15542.0333  31208.0987  30773.7428  40455.5784  10754.9779  39136.8449
#>  [67]  65827.5937   3693.5384  17902.1442   4323.2089 120300.4900   1959.2291
#>  [73]  60752.0324   9065.0840   6452.4055  42056.1917  18207.3070  39043.6943
#>  [79]  10745.6292  24112.5296   9128.6008  18416.8778  13895.3803  16908.1064
#>  [85]   6310.0966   4381.0841   7922.9705  12546.7703  11175.7684  31755.4344
#>  [91]  36926.0258  48195.5038  40373.2595  10658.1011  25550.2617  11961.7774
#>  [97]  39554.3265   4249.1463   8271.2878    619.3338
```
