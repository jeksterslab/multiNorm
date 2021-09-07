#' Generate Sample Variance-Covariance Matrix
#' from the Wishart Distribution
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param rcap Positive integer.
#'   `R` variates.
#' @param sigmacap Numeric matrix.
#'   Parameter.
#'   Covariance matrix
#'   \eqn{\boldsymbol{\Sigma}}.
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
#'   rcap = 100,
#'   sigmacap = sigmacap,
#'   df = 100,
#'   vector = TRUE
#' )
#'
#' colMeans(x)
#' @export
#' @family Multivariate Normal Distribution
#' @keywords multiNorm
rvcov_wishart <- function(rcap,
                          sigmacap,
                          df,
                          vector = FALSE) {
  stopifnot(
    is.matrix(sigmacap),
    is.vector(rcap),
    is.vector(df),
    is.logical(vector),
    length(rcap) == 1,
    length(df) == 1,
    length(vector) == 1
  )
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
