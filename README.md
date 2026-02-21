
<!-- README.md is generated from README.Rmd. Please edit that file -->

# duckspatial <a href="https://cidree.github.io/duckspatial/"><img src="man/figures/logo.png" alt="duckspatial website" align="right" height="138"/></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/duckspatial)](https://CRAN.R-project.org/package=duckspatial)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/Cidree/duckspatial/graph/badge.svg)](https://app.codecov.io/gh/Cidree/duckspatial)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Last Month
Downloads](https://cranlogs.r-pkg.org/badges/last-month/duckspatial?color=green)](https://CRAN.R-project.org/package=duckspatial)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/duckspatial?color=green)](https://CRAN.R-project.org/package=duckspatial)
[![check](https://github.com/Cidree/duckspatial/workflows/check/badge.svg)](https://github.com/Cidree/duckspatial/actions)

<!-- badges: end -->

> Important note: The {duckspatial} R package is undergoing major
> changes in preparation for v1.0.0. This release will align with DuckDB
> v1.5 and its native CRS support in the Spatial extension.
>
> Previously, {duckspatial} relied on a workaround to store CRS
> information in a dedicated column (named “crs_duckspatial” by default
> and referenced via the crs_column argument). With native CRS support
> now available, this workaround is no longer needed and will be
> removed. As a result, the crs and crs_column arguments are deprecated
> as of v0.9.0.
>
> The upcoming version also introduces a new native spatial class,
> duckspatial_df. This represents a lazy spatial table backed by a
> temporary DuckDB view, allowing workflows to remain fully lazy until
> results are explicitly materialized with ddbs_collect(). In practice,
> this means you can work with {duckspatial} without eagerly loading
> data into R.
>
> Finally, v1.0.0 will include many new functions and broader API
> improvements.

## Overview

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
you explicitly materialise it (e.g. with `ddbs_collect()`).

When the first `duckspatial_df` is created (either by reading a file or
converting an `sf` object) a temporary view is registered in a default
DuckDB connection with the spatial extension enabled. All spatial
operations run inside that connection, letting DuckDB apply its own
query optimisations before any data reaches R.

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
