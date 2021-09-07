#' Generate Sample Covariances
#' from the Wishart Distribution - Half-Vectorization
#' of the Covariance Matrix Input
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param rcap Positive integer.
#'   `R` variates.
#' @param x Numeric vector.
#'   Parameter.
#'   Half-vectorization of the covariance matrix
#'   \eqn{ \mathrm{vech} \left( \boldsymbol{\Sigma} \right)}.
#' @param df Positive integer.
#'   Parameter.
#'   Degrees of freedom.
#' @param vector Logical.
#'   If `vector = TRUE`,
#'   returns the half-vectorization of the covariance matrix
#'   for each `R` variate.
#'   If `vector = FALSE`,
#'   returns the covariance matrix
#'   for each `R` variate.
#'
#' @details
#' # Dependencies
#' * [sym_of_vech()]
#' * [vech()] (test)
#' * [vechnames()] (test)
#'
#' @returns A list (`vector = FALSE`) or matrix (`vector = TRUE`).
#'
#' @examples
#' x <- rvcov_wishart_of_vechsigmacap(
#'   rcap = 100,
#'   x = c(1, 0.5, 1),
#'   df = 100,
#'   vector = TRUE
#' )
#'
#' colMeans(x)
#' @export
#' @family Multivariate Normal Distribution
#' @keywords multiNorm
rvcov_wishart_of_vechsigmacap <- function(rcap,
                                          x,
                                          df,
                                          vector = FALSE) {
  stopifnot(
    is.vector(x),
    is.vector(rcap),
    is.vector(df),
    is.logical(vector),
    length(rcap) == 1,
    length(df) == 1,
    length(vector) == 1
  )
  sigmacap <- sym_of_vech(x)
  rcap <- as.integer(rcap)
  output <- stats::rWishart(
    n = rcap,
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
    X = seq_len(rcap),
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
