#' Generate Sample Variance-Covariance Matrix from the Wishart Distribution
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param n Positive integer.
#'   `n` variates.
#' @param sigmacap Numeric matrix.
#'   Parameter.
#'   Covariance matrix \eqn{\boldsymbol{\Sigma}}.
#' @param df Positive integer.
#'   Parameter.
#'   Degrees of freedom
#' @param vector Logical.
#'   If `vector = TRUE`,
#'   returns the half-vectorization of the covariance matrix
#'   for each `n` variate.
#'   If `vector = FALSE`,
#'   returns the covariance matrix
#'   for each `n` variate.
#'
#' @returns A list (`vector = FALSE`) or matrix (`vector = TRUE`).
#'
#' @examples
#' sigmacap <- matrix(
#'   data = c(
#'     1, 0.5, 0.5, 1
#'   ),
#'   nrow = 2
#' )
#'
#' x <- rvcov_wishart(
#'   n = 100,
#'   sigmacap = sigmacap,
#'   df = 100,
#'   vector = TRUE
#' )
#'
#' colMeans(x)
#' @export
#' @family Multivariate Normal Distribution
#' @keywords multiNorm
rvcov_wishart <- function(n,
                          sigmacap,
                          df,
                          vector = FALSE) {
  stopifnot(
    is.matrix(sigmacap),
    is.vector(n),
    is.vector(df),
    is.logical(vector),
    length(n) == 1,
    length(df) == 1,
    length(vector) == 1
  )
  n <- as.integer(n)
  output <- stats::rWishart(
    n = n,
    df = df,
    Sigma = sigmacap
  )
  output
  foo <- function(x) {
    if (vector) {
      return(
        vech(
          as.matrix(
            output[, , x]
          ) / df
        )
      )
    } else {
      return(
        as.matrix(
          output[, , x]
        ) / df
      )
    }
  }
  output <- lapply(
    X = seq_len(n),
    FUN = foo
  )
  if (vector) {
    return(
      do.call(
        what = "rbind",
        args = output
      )
    )
  } else {
    return(output)
  }
}
