
##' scale_as_matrix
##' @name scale_as_matrix
##' @description  Adapted version of \code{scale} for tibble objects
##'
##' @param data a tibble containing a matrix of data
##' @param key_col Strings giving names of key and value columns to create
##' @param value_col
##' @param center either a logical value or numeric-alike vector of length equal to the number of columns in \code{data} - 1, where `numeric-alike` means that \code{as.numeric(.)} will be appled successfully is \code{is.numeric(.)} is not true.
##' @param scale either a logical value or a numeric-alike vector of length equal to the number of columns of \code{data} - 1.
##' @return A tibble with the same shape as \code{data}, with the columns scaled.
##' @export
##'
scale_as_matrix = function(data,key_col,value_col,scale = TRUE,center = TRUE)
{
  extra_col = names(data)
  extra_col = extra_col[!extra_col %in% c(key_col,value_col)]

  data %>%
    spread_(  key_col = key_col,value_col = value_col) %>%
    as.data.frame() %>%
    column_to_rownames(extra_col) %>%
    as.matrix() %>%
    scale(scale = scale,center = center) %>%
    as.data.frame() %>%
    rownames_to_column(extra_col) %>%
    as_tibble() %>%
    gather_(key_col = key_col,value_col = value_col ,
            gather_cols = select_(data,.dots = key_col) %>%
              unique() %>% pull(key_col))
}
