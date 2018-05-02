##' @import tidyverse
NULL

##' Obtains the subset of strings that don't have the pattern
##'
##' \code{stri_subset} is a wrapper of \code{str_subset} that obtains the
##' subset of strings that don't contain the pattern
##'
##' @param string Input vector. Either a character vector, or something coercible to one.
##' @param pattern Pattern to look for.
##' @examples
##'
##'  fruit <- c("apple", "banana", "pear", "pinapple")
##'  stri_subset(fruit, "p")
##'
##' @export
##'
stri_subset <- function(string,pattern)
{
  string[ purrr::negate(stringr::str_detect)(string,pattern)]
}
