

##' extract the legend of a \code{ggplot} object
##'
##' @param plot a \code{ggplot} object
##' @export
##'
extract_legend <- function(plot)
{
  legend = ggplotGrob(plot)$grobs

  legend[[which(sapply(legend,function(x)x$name) == "guide-box")]]

}
