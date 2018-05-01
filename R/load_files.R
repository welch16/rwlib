##' Tidy load from file to list
##'
##' To avoid use commands of the form \code{load(file)}, this function
##' loads the file into an environment and then return the environment.
##'
##' @param file string with the name of an RData file
##' @param env an \code{environment}, to which the file is going to be load into. By default the function creates a new one.
##' @examples
##'
##' \dontrun{
##'
##' data(iris)
##' my_file = tempfile(fileext = ".RData")
##' save(iris, file = my_file)
##' load2env(my_file)
##'
##' }
##' @export
load2env <- function(file,env = new.env())
{
  load(file,envir = env)
  env
}
