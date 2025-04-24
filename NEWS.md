
# duckspatial 0.2.0

## NEW FEATURES

* `ddbs_read_vector()`: gains a new argument `clauses` to modify the query from the table (e.g. "WHERE ...", "ORDER BY...")

## NEW FUNCTIONS

* `ddbs_list_tables()`: lists table schemas and tables insdie the database

* `ddbs_glimpse()`: check first rows of a table

* `ddbs_buffer()`: calculates the buffer around the input geometry

* `ddbs_centroid()`: calculates the centroid of the input geometry


## IMPROVEMENTS

* `ddbs_intersection()`: overwrite argument defaults to `FALSE` instead of `NULL`

* Better schemas management. Added support for all functions.

# duckspatial 0.1.0

* Initial CRAN submission.
