# duckspatial

> As of **{duckspatial} v1.0.0**, the package uses the native CRS
> support provided by the DuckDB Spatial extension (DuckDB ≥ 1.5). The
> previous workaround of storing CRS information in a dedicated column
> has been removed, and the `crs` and `crs_column` arguments are no
> longer supported.
>
> Spatial tables are now represented using the `duckspatial_df` class, a
> lazy spatial table backed by a temporary DuckDB table. This allows
> spatial workflows to remain in DuckDB until the results are explicitly
> materialized with
> [`ddbs_collect()`](https://cidree.github.io/duckspatial/reference/ddbs_collect.md),
> enabling efficient processing without eagerly loading data into R.
>
> This release also introduces a number of new spatial functions and API
> improvements, providing a more consistent and fully native integration
> with DuckDB’s spatial capabilities.

**{duckspatial}** provides fast, memory-efficient functions for
analysing and manipulating large spatial vector datasets in R. It
bridges [DuckDB’s spatial
extension](https://duckdb.org/docs/stable/core_extensions/spatial/functions)
with R’s spatial ecosystem — in particular **{sf}** — so you can
leverage DuckDB’s analytical power without leaving your familiar R
workflow.

### How it works

Starting from v1.0.0, {duckspatial} introduces a native S3 class called
`duckspatial_df`: a `tibble`-like object with a geometry column that
lives **outside R’s memory**. Data is read and evaluated lazily (similar
to how `duckplyr` handles lazy tables) and is only loaded into R when
you explicitly materialize it (e.g. with
[`ddbs_collect()`](https://cidree.github.io/duckspatial/reference/ddbs_collect.md)).

When the first `duckspatial_df` is created (either by reading a file or
converting an `sf` object) a temporary view is registered in a default
DuckDB connection with the spatial extension enabled. All spatial
operations run inside that connection, letting DuckDB apply its own
query optimizations before any data reaches R.

### Naming conventions

All spatial functions follow the `ddbs_*()` prefix (*DuckDB Spatial*),
and their names deliberately mirror the **{sf}** package, so users
already familiar with `sf` can get started immediately.

## Installation

Install the stable release from CRAN:

``` r
# install.packages("pak")
pak::pak("duckspatial")
```

Install the latest GitHub version (more features, fewer accumulated
bugs):

``` r
pak::pak("Cidree/duckspatial")
```

Install the development version (may be unstable):

``` r
pak::pak("Cidree/duckspatial@dev")
```

## Core idea: flexible spatial workflows

A central design principle of {duckspatial} is that the same spatial
operation can be used in different ways depending on how your data is
stored and how you want to manage memory and performance.

Most functions support four complementary input/output combinations:

| Input                   | Output                  |
|-------------------------|-------------------------|
| `duckspatial_df` / `sf` | `duckspatial_df` / `sf` |
| `duckspatial_df` / `sf` | DuckDB table            |
| DuckDB table            | `duckspatial_df` / `sf` |
| DuckDB table            | DuckDB table            |

This means you can keep data inside DuckDB for as long as possible,
pulling results into R only when you need them. See the [Get Started
vignette](https://Cidree.github.io/duckspatial/articles/duckspatial.html)
for worked examples of each workflow.

## Contributing

Bug reports, feature requests, and pull requests are very welcome!

- [Raise an issue](https://github.com/Cidree/duckspatial/issues)
- [Open a pull request](https://github.com/Cidree/duckspatial/pulls)
