##' @import grid
NULL

##' Multiple plot function
##'
##'  ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
##'  @param cols number of columns in layout
##'  @param layout a matrix specifying the layout. If present, \code{cols} is
##'  ignored.
##'  If the layout is something like \code{matrix(c(1,2,3,3), nrow=2, byrow=TRUE)},
##'  # then plot 1 will go in the upper left, 2 will go in the upper right, and
##'  # 3 will go all the way across the bottom.
##'  @export
##'  taken from https://support.bioconductor.org/p/68650/
##'  link to highlight axis too http://rpubs.com/lgadar/matrix-visualizations
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
