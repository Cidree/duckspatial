#' @param x Input spatial data. Can be:
#'   \itemize{
#'     \item A \code{duckspatial_df} object (lazy spatial data frame via dbplyr)
#'     \item An \code{sf} object
#'     \item A \code{tbl_lazy} from dbplyr
#'     \item A character string naming a table/view in \code{conn}
#'   }
#'   Data is returned from this object.
