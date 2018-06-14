##' @importFrom scales trans_new
NULL

##' scales to extend small numeric values
##'
##' @param base the base of the logarithm
##' @export
##'
minus_log_trans = function(base = exp(1))
  scales::trans_new("minus_log",
                    transform = function(x)-log(x,base) ,
                    inverse = function(x) exp(-x* log(base)))
