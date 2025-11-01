#' @details Spatial Join Predicates:
#'
#' A spatial predicate is really just a function that evaluates some spatial
#' relation between two geometries and returns true or false, e.g., “does a
#' contain b” or “is a within distance x of b”. Here is a quick overview of the
#' most commonly used ones, taking two geometries a and b:
#'
#' - `"ST_Intersects"`: Whether a intersects b
#' - `"ST_Contains"`: Whether a contains b
#' - `"ST_ContainsProperly"`: Whether a contains b without b touching a's boundary
#' - `"ST_Within"`: Whether a is within b
#' - `"ST_Overlaps"`: Whether a overlaps b
#' - `"ST_Touches"`: Whether a touches b
#' - `"ST_Equals"`: Whether a is equal to b
#' - `"ST_Crosses"`: Whether a crosses b
#' - `"ST_Covers"`: Whether a covers b
#' - `"ST_CoveredBy"`: Whether a is covered by b
#' - `"ST_DWithin"`: x)	Whether a is within distance x of b
