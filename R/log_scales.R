##' @importFrom scales trans_new
NULL

##' Convert values between 0 and 1 to log scales and changes its sign
##' @param x a numeric value
##' @param base base used for log
##'
##' @return a numeric value
##' @export
##'
minus_log = function(x,base = exp(1))-log(x,base)


##' Converts positive values to values between 0 and 1
##' @param x a numeric value
##' @param base base used for log
##'
##' @return a numeric value
##' @export
##'
inverse_minus_log = function(x,base = exp(1))exp(-x * log(base))

##' scales to extend small numeric values
##'
##' @param base the base of the logarithm
##' @export
##'
minus_log_trans = function(base = exp(1))
  scales::trans_new("minus_log",
                    minus_log(base = base),
                    inverse_minus_log(base = base))
