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
#' @param list Logical.
#'   If `list = TRUE`,
#'   returns a list where each element is a covariance matrix.
#'   If `list = FALSE`,
#'   returns a matrix where each row corresponds to
#'   the vectorization of the output matrix (`vec = TRUE`) or
#'   the half-vectorization of the output matrix (`vec = FALSE`)
#' @param vec Logical.
#'   This is only evaluated when `list = FALSE`.
#'   If `vec = TRUE`,
#'   returns the vectorization of the covariance matrix
#'   for each `R` variate.
#'   If `vec = FALSE`,
#'   returns the half-covariance matrix
#'   for each `R` variate.
#'
#' @returns A list (`vec = FALSE`) or matrix (`vec = TRUE`).
#'
#' @examples
#' sigmacap <- matrix(
#'   data = c(
#'     1, 0.5, 0.5, 1
#'   ),
#'   nrow = 2
#' )
#'
#' rvcov_wishart(
#'   rcap = 5,
#'   sigmacap = sigmacap,
#'   df = 100,
#'   list = FALSE
#' )
#' @export
#' @family Multivariate Normal Distribution Functions
#' @keywords multiNorm
rvcov_wishart <- function(rcap,
                          sigmacap,
                          df,
                          list = TRUE,
                          vec = TRUE) {
  stopifnot(
    is.matrix(sigmacap),
    is.vector(rcap),
    is.vector(df),
    is.logical(vec),
    is.logical(list),
    length(rcap) == 1,
    length(df) == 1,
    length(vec) == 1,
    length(list) == 1
  )
  rcap <- as.integer(rcap)
  output <- stats::rWishart(
    n = rcap,
    df = df,
    Sigma = sigmacap
  )
  output <- lapply(
    X = seq_len(rcap),
    FUN = function(x) {
      return(
        as.matrix(
          output[, , x]
        ) / df
      )
    }
  )
  if (list) {
    return(output)
  } else {
    if (vec) {
      output <- lapply(
        X = output,
        FUN = as.vector
      )
    } else {
      output <- lapply(
        X = output,
        FUN = function(x) {
          return(x[lower.tri(x, diag = TRUE)])
        }
      )
    }
    return(
      do.call(
        what = "rbind",
        args = output
      )
    )
  }
}
