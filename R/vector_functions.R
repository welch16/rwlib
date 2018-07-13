##' Removes the names of the vector
##'
##' \code{remove_names} is a function to remove the names of a vector.
##'
##' @param x Vector to remove names
##'
##' @examples
##' x = c("a" = 1,"b" = 3)
##' remove_names(x)
##'
remove_names <- function(x)
{
  names(x) = NULL
  x
}
