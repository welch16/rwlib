% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knn_impute.R
\name{impute_knn}
\alias{impute_knn}
\title{imputes missing value in a tibble with a mice inspired
knn algorithm}
\usage{
impute_knn(my_data, k, tol, max_iter, verbose = FALSE)
}
\arguments{
\item{my_data}{a tibble / data.frame containing the dataset with missing values}

\item{k}{the number of neighbors}

\item{tol}{convergence tolerange}

\item{max_iter}{max number of iterations}

\item{verbose}{a logical value indicating if should prinnt the iterations}
}
\value{
a data.frame / tibble with imputed entries where my_data has NA values
}
\description{
imputes missing value in a tibble with a mice inspired
knn algorithm
}
