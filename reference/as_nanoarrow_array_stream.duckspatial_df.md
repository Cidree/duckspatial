# Convert a duckspatial_df to a nanoarrow_array_stream

Convert a duckspatial_df to a nanoarrow_array_stream

## Usage

``` r
as_nanoarrow_array_stream.duckspatial_df(x, ..., schema = NULL, native = FALSE)
```

## Arguments

- x:

  A `duckspatial_df` object

- ...:

  Additional arguments passed to downstream methods

- schema:

  Optional target schema for the entire stream.

- native:

  If TRUE, transforms WKB to a "Native" GeoArrow layout (e.g., Point,
  Polygon) using optimized Arrow-to-Arrow kernels. This layout is
  optimized for high-performance rendering in tools like Deck.GL.

## Value

A `nanoarrow_array_stream`
