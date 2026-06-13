# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**duckspatial** is an R package that bridges DuckDB's spatial extension with R's spatial ecosystem (primarily `sf`). It exposes 100+ functions under the `ddbs_*` naming convention and introduces a `duckspatial_df` class — a lazy spatial data frame backed by temporary DuckDB tables/views with `dbplyr`.

Functions of the DuckDB's spatial extension can be consulted [here](https://duckdb.org/docs/current/core_extensions/spatial/functions).

Key design principle: operations stay in DuckDB until the user explicitly collects results, enabling out-of-memory spatial workflows.

## Common Commands

```r
# Install dependencies
devtools::install_deps(dependencies = TRUE)

# Load package during development
devtools::load_all()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-ddbs_measure.R")

# Run a specific test by name
testthat::test_file("tests/testthat/test-ddbs_ops_unary.R", filter = "buffer")

# Check the full package
devtools::check(vignettes = FALSE)

# Regenerate documentation
devtools::document()

# Build pkgdown site
pkgdown::build_site()
```

## Architecture

### The `duckspatial_df` Class

The central abstraction. Defined in `R/duckspatial_df.R` and given `dplyr` S3 methods in `R/duckspatial_df_dplyr_methods.R`. A `duckspatial_df` is a `tbl_duckdb_connection` with extra metadata (CRS, geometry column name). Operations on it generate temporary tables that live in a default's DuckDB connection, and it's only materialized in R as `sf` with `collect()` or `ddbs_collect()`.

Constructor: `new_duckspatial_df()`. Conversion from sf: `as_duckspatial_df()`. Check: `is_duckspatial_df()`.

### Function Naming

All exported functions use `ddbs_*` prefix (mirrors `st_*` in the `sf` package). Internal helpers use `.ddbs_*` or follow standard R conventions.

### Template System

Spatial operations avoid code duplication via template functions in `R/ddbs_templates.R`. For example, `template_unary_ops()` generates the body for unary geometry operations (buffer, centroid, etc.). When adding a new unary/binary spatial op, use the existing templates rather than writing boilerplate from scratch.

### CRS Handling

CRS is stored natively in DuckDB >= 1.5 via `GEOMETRY` column metadata. The logic is split between:
- `R/ddbs_crs.R` — `ddbs_crs()` S3 generic with methods for all input types
- `R/crs_persistence.R` — persistence, reading, and backwards compatibility logic

Always propagate CRS through operations; inspect these files when debugging CRS round-trip issues.

### SQL Generation

DBI/dbplyr handles lazy query building. `glue::glue()` is used for SQL string interpolation in template functions. All spatial SQL targets DuckDB's spatial extension syntax.

### Key File Map

| Area | Files |
|---|---|
| Class definition | `duckspatial_df.R`, `duckspatial_df_dplyr_methods.R` |
| CRS | `ddbs_crs.R`, `crs_persistence.R` |
| Unary ops (buffer, centroid, …) | `ddbs_ops_unary.R`, `ddbs_ops_unary_line.R` |
| Binary ops (intersection, union, …) | `ddbs_ops_binary.R`, `ddbs_union.R` |
| Measurements (area, distance, …) | `ddbs_measure.R` |
| Affine transforms | `ddbs_affine.R` |
| Spatial joins/predicates | `ddbs_join.R`, `ddbs_predicates.R` |
| Geometry validation | `ddbs_geom_validation.R` |
| Templates (code reuse) | `ddbs_templates.R` |
| DB utilities / assertions | `db_utils.R` |
| Options | `ddbs_options.R` |

## Tests

Tests use **testthat edition 3**. `tests/testthat/setup.R` loads shared spatial fixtures:
- `countries`, `argentina`, `rivers`, `points` — from `inst/spatial/`
- NC dataset from the `sf` package
- Each fixture is available as both `sf` and `duckspatial_df` variants (e.g., `nc` and `nc_ddbs`)

`OMP_THREAD_LIMIT=2` is set in `setup.R` to keep DuckDB parallelism deterministic during tests.

## Adding a New Function

When implementing a new exported function, complete **all four steps** before considering the task done.

### 1. Implementation and documentation

Write the function in the R file that best matches its category (see Key File Map above). Add full `roxygen2` documentation using the template tags already present in the file (`@template x`, `@template conn_null`, etc.) — reuse them rather than duplicating the parameter prose. Run `devtools::document()` afterwards.

### 2. Unit tests

Create or append to `tests/testthat/test-<file>.R` — use the same filename stem as the R source file. Follow the `describe()` / `it()` pattern used throughout the package. At minimum cover:

- Expected output class for each input type (`duckspatial_df`, `sf`, character table name)
- Each output mode (`duckspatial_df` by default, `sf` when `mode = "sf"`)
- Message behaviour (`quiet = FALSE` / `quiet = TRUE`, `name` triggers a message)
- Any function-specific correctness checks (e.g., CRS is actually set, coordinates match)
- Error cases: wrong `x` type, missing `conn` for character input, bad `overwrite`/`quiet` types, non-existent table name, multi-value `name`

### 3. NEWS.md

Add one bullet under `## NEW FEATURES` of the **topmost** (development) version block in `NEWS.md`. Match the style of existing entries:

```md
* `ddbs_new_function()`: one-sentence description of what it does (#issue_number if applicable).
```

### 4. _pkgdown.yml

Read `_pkgdown.yml` to find the most appropriate `reference:` section and add the function name to its `contents:` list. If no existing section fits, create a new one following the same `title` / `desc` / `contents` structure.
