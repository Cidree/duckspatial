# Report duckspatial configuration status

Displays useful information about the current configuration, including
global options and the status of the default DuckDB connection.

## Usage

``` r
ddbs_sitrep()
```

## Value

Invisibly returns a list with the current status configuration.

## Examples

``` r
ddbs_sitrep()
#> 
#> ── duckspatial Status Report ───────────────────────────────────────────────────
#> 
#> ── Global Options ──
#> 
#> • Output Type: "duckspatial_df"
#> 
#> ── Default Connection ──
#> 
#> ! No default connection active (will be created on demand).
```
