##' @import Vennerable
NULL


##' plot_venn function
##'
##' @description A wrapper of the plot Vennerable function to change colors, and white fill
##' @param venn a venn diagram generated with compute.Venn function
##' @param colors a vector of the same length as sets used to generated the venn object
##'
##' @return a plot with the venn diagram
##' @export
##' @param colors
##' @examples
##' \dontrun{
##'
##' v1 = lit_snps %>% map(as.character) %>%
##'     Venn() %>% compute.Venn(doWeights = FALSE)
##'
##' plot_venn(v1,c("red","black"))
##'
##' }


plot_venn <- function(venn , colors)
{
  gp = VennThemes(venn)

  ## add white fills to everything
  gp[["Face"]] = map(gp[["Face"]],
                     function(x){
                       x$fill = "white"
                       x
                     })

  ## change colors
  gp[["Set"]] = map2(gp[["Set"]],colors,
                     function(x,y){
                       x$col = y
                       x
                     })

  ## channge text colors

  gp[["SetText"]] = map2(gp[["SetText"]],colors,
                         function(x,y){
                           x$col = y
                           x
                         })


  grid.newpage()
  plot(venn,gp = gp, show = list(Universe = FALSE))


}
