# sf methods for duckspatial_df

These methods provide sf compatibility for duckspatial_df objects,
allowing them to work with sf functions like st_crs(), st_geometry(),
etc.

## Usage

``` r
# S3 method for class 'duckspatial_df'
st_crs(x, ...)

# S3 method for class 'duckspatial_df'
st_geometry(obj, ...)

# S3 method for class 'duckspatial_df'
st_bbox(obj, ...)

# S3 method for class 'duckspatial_df'
st_as_sf(x, ...)

# S3 method for class 'duckspatial_df'
print(x, ..., n = 10)
```
