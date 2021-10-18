#' Generate Sample Variance-Covariance Matrix
#' from the Multivariate Normal Distribution
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param rcap Positive integer.
#'   `R` variates.
#' @param sigmacap Numeric matrix.
#'   Parameter.
#'   Covariance matrix
#'   \eqn{\boldsymbol{\Sigma}}.
#' @param gammacap Numeric matrix.
#'   Parameter.
#'   Asymptotic covariance matrix of the covariance matrix
#'   \eqn{\boldsymbol{\Gamma}}.
#' @param n Positive integer.
#'   Sample size.
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
#' @examples
#' sigmacap <- matrix(
#'   data = c(1, 0.5, 0.5, 1),
#'   nrow = 2
#' )
#' gammacap <- matrix(
#'   data = c(
#'     2.00, 1.00, 0.50,
#'     1.00, 1.25, 1.00,
#'     0.50, 1.00, 2.00
#'   ),
#'   nrow = 3
#' )
#'
#' rvcov_mvn_chol(
#'   rcap = 5,
#'   sigmacap = sigmacap,
#'   gammacap = gammacap,
#'   n = 100
#' )
#' @export
#' @family Multivariate Normal Distribution Functions
#' @keywords multiNorm random mvn
rvcov_mvn_chol <- function(rcap,
                           sigmacap,
                           gammacap,
                           n,
                           list = TRUE,
                           vec = TRUE) {
  stopifnot(
    is.matrix(sigmacap),
    is.vector(rcap),
    is.vector(n),
    is.logical(vec),
    is.logical(list),
    length(rcap) == 1,
    length(n) == 1,
    length(vec) == 1,
    length(list) == 1
  )
  rcap <- as.integer(rcap)
  n <- as.integer(n)
  vechsigmacap <- vech(sigmacap)
  diags <- diag_of_vech(
    vechsigmacap,
    loc = TRUE
  )
  xstar <- rmvn_chol(
    n = rcap,
    mu = vechsigmacap,
    sigmacap = gammacap / n, # divided by n is essential here
    data_frame = FALSE
  )
  check_neg <- function(x, vec) {
    if (any(x[diags] <= 0)) {
      rerun <- TRUE
      while (rerun) {
        x <- as.vector(
          rmvn_chol(
            n = 1,
            mu = vechsigmacap,
            sigmacap = gammacap / n,
            data_frame = FALSE
          )
        )
        rerun <- any(x[diags] <= 0)
      }
    }
    if (list) {
      x <- sym_of_vech(x)
    } else {
      if (vec) {
        x <- as.vector(sym_of_vech(x))
      }
    }
    return(x)
  }
  output <- lapply(
    X = as.data.frame(t(xstar)),
    FUN = check_neg,
    vec = vec
  )
  names(output) <- NULL
  if (!list) {
    output <- do.call(
      what = "rbind",
      args = output
    )
  }
  return(output)
}
