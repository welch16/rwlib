##' @importFrom purrr map2
NULL

##' saves a list of plots with the same parameters
##'
##' @param filenames a vector with the filenames for the plots
##' @param plots a list of plots of the same length than \code{filenames}
##' @return nothing but it saves a bunch of plots
##' @export
save_list <- function(filenames, plots, ...)
{
  stopifnot(is.list(plots))
  stopifnot(length(filenames) == length(plots))

  purrr::map2(
    filenames,
    plots,
    save_plot, ...)

}
