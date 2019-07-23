##' @importFrom RANN nn2
NULL

##' init of knn impute, if the variable is numeric then imputes with mean of all values
##' otherwise takes the most repeated levels.
##'
##' @param var a variable with missing values as NA
##' @return the var vector with imputed values
knn_impute_init <- function(var)
{
  if(is.numeric(var)){
    imp <- var
    imp[is.na(imp)] <- mean(var,na.rm = TRUE)
  }else{
    imp <- var
    imp[is.na(imp)] <- most_repeated(var)
  }
  imp
}

##' returns the most repeated value in var
##'
##' @param var a vector
##' @return the most repeated value
most_repeated <- function(var)
{
  tt <- table(var)
  names(tt)[which.max(tt)]
}


##' imputes only one variable with knn
##'
##' @param var a string indicating the variable to be imputed
##' @param my_data a tibble / data.frame containing the dataset with missing values
##' @param my_data_impute a tibble / data.frame containing the dataset with filled missing values
##' @param k the number of neighbors
impute_variable <- function(var , my_data, my_data_impute, k)
{
  na_idx <- my_data %>% pluck(var) %>% is.na() %>% which()

  train <- my_data_impute %>% slice(-na_idx)
  test <- my_data_impute %>% slice(na_idx) %>% select(-!! sym(var))

  resp <- train %>% pluck(var)
  train <- train %>% select(-one_of(var))

  train %<>% mutate_if(is.character,list(~ factor(.)))
  test %<>% mutate_if(is.character, list(~factor(.)))

  knn_search <- RANN::nn2( train, query = test, k = k )
  neighbor_values <- matrix(resp[knn_search$nn.idx], ncol = k)

  if( is.factor(my_data[[var]])){
    imputed_values <- apply(neighbor_values, 1, most_repeated)
  }else{
    imputed_values <- rowMeans(neighbor_values)
  }

  my_data_impute[[var]][na_idx] <- imputed_values
  my_data_impute
}


calculate_loss <- function(var,old_var)
{

  if(is.numeric(var)){
    mean((var - old_var)^2)
  }else{
    mean(var != old_var)
  }
}


##' imputes missing value in a tibble with a mice inspired
##' knn algorithm
##'
##' @param my_data a tibble / data.frame containing the dataset with missing values
##' @param k the number of neighbors
##' @param tol convergence tolerange
##' @param max_iter max number of iterations
##' @param verbose a logical value indicating if should prinnt the iterations
##'
##' @return a data.frame / tibble with imputed entries where my_data has NA values
##' @export
impute_knn <- function(my_data, k , tol, max_iter ,verbose = FALSE)
{
  ## init by most repeated if categorical / mean if numerical
  my_data_impute <- my_data %>% mutate_all( list( ~ knn_impute_init(.)))

  any_na <- function(.)any(is.na(.))
  vars <- my_data %>% select_if( any_na) %>% names()

  old <- my_data_impute

  iter <- 0
  loss <- tol + 1

  while( loss >= tol & iter <= max_iter) {
    if(verbose) message(iter)
    for(var in vars){
      if(verbose) message(var)
      my_data_impute <- impute_variable(var,my_data,my_data_impute,k)
    }
    loss <- map2_dbl(
      old %>% as.list(),
      my_data_impute %>% as.list(),
      calculate_loss)

    loss %<>% mean()

    old <- my_data_impute
    iter <- iter + 1
  }

  my_data_impute


}

