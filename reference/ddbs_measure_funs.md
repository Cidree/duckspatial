# Calculate geometric measurements

Compute area, length, perimeter, or distance of geometries with
automatic method selection based on the coordinate reference system
(CRS).

## Usage

``` r
ddbs_area(
  x,
  new_column = "area",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_length(
  x,
  new_column = "length",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_perimeter(
  x,
  new_column = "perimeter",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_distance(
  x,
  y,
  dist_type = NULL,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  id_x = NULL,
  id_y = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- x:

  Input geometry (sf object, duckspatial_df, or table name in DuckDB)

- new_column:

  Name of the new column to create on the input data. Ignored with
  `mode = "sf"`.

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

- mode:

  Character. Controls the return type. Options:

  - `"duckspatial"` (default): Lazy spatial data frame backed by
    dbplyr/DuckDB

  - `"sf"`: Eagerly collected sf object (uses memory)

  Can be set globally via
  [`ddbs_options`](https://cidree.github.io/duckspatial/reference/ddbs_options.md)`(mode = "...")`
  or per-function via this argument. Per-function overrides global
  setting.

- overwrite:

  Boolean. whether to overwrite the existing table if it exists.
  Defaults to `FALSE`. This argument is ignored when `name` is `NULL`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

- y:

  Second input geometry for distance calculations (sf object,
  duckspatial_df, or table name)

- dist_type:

  Character. Distance type to be calculated. By default it uses the best
  option for the input CRS (see details).

- conn_x:

  A `DBIConnection` object to a DuckDB database for the input `x`. If
  `NULL` (default), it is resolved from `conn` or extracted from `x`.

- conn_y:

  A `DBIConnection` object to a DuckDB database for the input `y`. If
  `NULL` (default), it is resolved from `conn` or extracted from `y`.

- id_x:

  Character; optional name of the column in `x` whose values will be
  used to name the list elements. If `NULL`, integer row numbers of `x`
  are used.

- id_y:

  Character; optional name of the column in `y` whose values will
  replace the integer indices returned in each element of the list.

## Value

For `ddbs_area`, `ddbs_length`, and `ddbs_perimeter`:

- `mode = "duckspatial"` (default): A `duckspatial_df` (lazy spatial
  data frame) backed by dbplyr/DuckDB.

- `mode = "sf"`: An eagerly collected vector in R memory.

- When `name` is provided: writes the table in the DuckDB connection and
  returns `TRUE` (invisibly).

For `ddbs_distance`: A `units` matrix in meters with dimensions nrow(x),
nrow(y).

## Details

These functions automatically select the appropriate calculation method
based on the input CRS:

**For EPSG:4326 (geographic coordinates):**

- Uses `ST_*_Spheroid` functions (e.g., `ST_Area_Spheroid`,
  `ST_Length_Spheroid`)

- Leverages GeographicLib library for ellipsoidal earth model
  calculations

- Highly accurate but slower than planar calculations

- For `ddbs_distance` with POINT geometries: defaults to `"haversine"`

- For `ddbs_distance` with other geometries: defaults to `"spheroid"`

**For projected CRS (e.g., UTM, Web Mercator):**

- Uses planar `ST_*` functions (e.g., `ST_Area`, `ST_Length`)

- Faster performance with accurate results in meters

- For `ddbs_distance`: defaults to `"planar"`

**Distance calculation methods** (`dist_type` argument):

- `NULL` (default): Automatically selects best method for input CRS

- `"planar"`: Planar distance (for projected CRS)

- `"geos"`: Planar distance using GEOS library (for projected CRS)

- `"haversine"`: Great circle distance (requires EPSG:4326 and POINT
  geometries)

- `"spheroid"`: Ellipsoidal model using GeographicLib (most accurate,
  slowest)

**Distance type requirements:**

- `"planar"` and `"geos"`: Require projected coordinates (not degrees)

- `"haversine"` and `"spheroid"`: Require POINT geometries and EPSG:4326

## Performance

Speed comparison (fastest to slowest):

1.  Planar calculations on projected CRS

2.  Haversine (spherical approximation)

3.  Spheroid functions (ellipsoidal model)

## References

<https://geographiclib.sourceforge.io/>

## Examples

``` r
if (FALSE) { # \dontrun{
library(duckspatial)
library(dplyr)

# Create a DuckDB connection
conn <- ddbs_create_conn(dbdir = "memory")

# ===== AREA CALCULATIONS =====

# Load polygon data
countries_ddbs <- ddbs_open_dataset(
  system.file("spatial/countries.geojson", package = "duckspatial")
) |>
  ddbs_transform("EPSG:3857") |> 
  filter(NAME_ENGL != "Antarctica")

# Store in DuckDB
ddbs_write_table(conn, countries_ddbs, "countries")

# Calculate area (adds a new column - area by default)
ddbs_area("countries", conn)

# Calculate area with custom column name
ddbs_area("countries", conn, new_column = "area_sqm")

# Create new table with area calculations
ddbs_area("countries", conn, name = "countries_with_area", new_column = "area_sqm")

# Calculate area from sf object directly
ddbs_area(countries_ddbs)

# Calculate area using dplyr syntax
countries_ddbs |> 
  mutate(area = ddbs_area(geom))

# Calculate total area 
countries_ddbs |> 
  mutate(area = ddbs_area(geom)) |> 
  summarise(
    area = sum(area),
    geom = ddbs_union(geom)
  )

# ===== LENGTH CALCULATIONS =====

# Load line data
rivers_ddbs <- sf::read_sf(
  system.file("spatial/rivers.geojson", package = "duckspatial")
) |> 
  as_duckspatial_df()

# Store in DuckDB
ddbs_write_table(conn, rivers_ddbs, "rivers")

# Calculate length (add a new column - length by default)
ddbs_length("rivers", conn)

# Calculate length with custom column name
ddbs_length(rivers_ddbs, new_column = "length_meters")

# Calculate length by river name
rivers_ddbs |> 
  ddbs_union_agg("RIVER_NAME") |> 
  ddbs_length()

# Add length within dplyr
rivers_ddbs |> 
  mutate(length = ddbs_length(geometry))


# ===== PERIMETER CALCULATIONS =====

# Calculate perimeter (returns sf object with perimeter column)
ddbs_perimeter(countries_ddbs)

# Calculate perimeter within dplyr
countries_ddbs |> 
  mutate(perim = ddbs_perimeter(geom))


# ===== DISTANCE CALCULATIONS =====

# Create sample points in EPSG:4326
n <- 10
points_sf <- data.frame(
  id = 1:n,
  x = runif(n, min = -180, max = 180),
  y = runif(n, min = -90, max = 90)
) |>
  ddbs_as_spatial(coords = c("x", "y"), crs = "EPSG:4326")

# Option 1: Using sf objects (auto-selects haversine for EPSG:4326 points)
dist_matrix <- ddbs_distance(x = points_sf, y = points_sf)
head(dist_matrix)

# Option 2: Explicitly specify distance type
dist_matrix_harv <- ddbs_distance(
  x = points_sf,
  y = points_sf,
  dist_type = "haversine"
)

# Option 3: Using DuckDB tables
ddbs_write_table(conn, points_sf, "points", overwrite = TRUE)
dist_matrix_sph <- ddbs_distance(
  conn = conn,
  x = "points",
  y = "points",
  dist_type = "spheroid"  # Most accurate for geographic coordinates
)
head(dist_matrix_sph)

# Close connection
ddbs_stop_conn(conn)
} # }
```
