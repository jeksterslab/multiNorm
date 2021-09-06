#' Generate Sample Covariances from the Wishart Distribution - vech of Sigma
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param n Positive integer.
#'   `n` variates.
#' @param x Numeric vector.
#'   Half-vectorization of the covariance matrix.
#' @param df Positive integer.
#'   Degrees of freesom.
#' @param vector Logical.
#'   If `vector = TRUE`,
#'   returns the half-vectorization of the covariance matrix
#'   for each `n` variate.
#'   If `vector = FALSE`,
#'   returns the covariance matrix
#'   for each `n` variate.
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
#'   n = 100,
#'   x = c(1, 0.5, 1),
#'   df = 100,
#'   vector = TRUE
#' )
#'
#' colMeans(x)
#' @export
#' @family Multivariate Normal Distribution
#' @keywords multiNorm
rvcov_wishart_of_vechsigmacap <- function(n,
                                          x,
                                          df,
                                          vector = FALSE) {
  stopifnot(
    is.vector(x),
    is.vector(n),
    is.vector(df),
    is.logical(vector),
    length(n) == 1,
    length(df) == 1,
    length(vector) == 1
  )
  sigmacap <- sym_of_vech(x)
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
