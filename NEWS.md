# duckspatial 0.2.0999 dev

## MINOR CHANGES

* All functions now have a parameter `quiet` that allows users to supress informational messages. Closed [#3](https://github.com/Cidree/duckspatial/issues/3)


# duckspatial 0.2.0

## NEW FEATURES

* `ddbs_read_vector()`: gains a new argument `clauses` to modify the query from the table (e.g. "WHERE ...", "ORDER BY...")

## NEW FUNCTIONS

* `ddbs_list_tables()`: lists table schemas and tables inside the database

* `ddbs_glimpse()`: check first rows of a table

* `ddbs_buffer()`: calculates the buffer around the input geometry

* `ddbs_centroid()`: calculates the centroid of the input geometry

* `ddbs_difference()`: calculates the geometric difference between two objects


## IMPROVEMENTS

* `ddbs_intersection()`: overwrite argument defaults to `FALSE` instead of `NULL`

* Better schemas management. Added support for all functions.

# duckspatial 0.1.0

* Initial CRAN submission.
