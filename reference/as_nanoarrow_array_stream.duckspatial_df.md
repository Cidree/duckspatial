# Convert a duckspatial_df to a nanoarrow_array_stream

Convert a duckspatial_df to a nanoarrow_array_stream

## Usage

``` r
as_nanoarrow_array_stream.duckspatial_df(
  x,
  ...,
  schema = NULL,
  native = FALSE,
  chunk_size = 1e+06
)
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

- chunk_size:

  Maximum number of rows in each Arrow record batch.

## Value

A `nanoarrow_array_stream`
