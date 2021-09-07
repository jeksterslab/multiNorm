#' Generate Data from the Multivariate Normal Distribution
#' Using the Cholesky Decomposition
#' -
#' \eqn{\boldsymbol{\theta}}
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param n Positive integer.
#'   `n` variates.
#' @param x Numeric vector.
#'   Vector of parameters
#'   \eqn{
#'     \boldsymbol{\theta}
#'     =
#'     \{
#'       \boldsymbol{\mu}
#'       \mathrm{vech} \left( \boldsymbol{\Sigma} \right)
#'     \}^{\prime}
#'   }.
#' @param varnames Character vector
#'   Optional variable names.
#' @param data_frame Logical.
#'   If `data_frame = TRUE`,
#'   returns a `data.frame`.
#'   If `data_frame = FALSE`,
#'   returns a `matrix`.
#'
#' @returns A matrix (`data_frame = FALSE`) or data.frame (`data_frame = TRUE`).
#'
#' @examples
#'
#' x <- rmvn_chol_of_theta(
#'   n = 100,
#'   x = c(0, 0, 1, 0.5, 1)
#' )
#'
#' colMeans(x)
#' cov(x)
#' @export
#' @family Multivariate Normal Distribution
#' @keywords multiNorm
rmvn_chol_of_theta <- function(n,
                               x,
                               varnames = NULL,
                               data_frame = FALSE) {
  stopifnot(is.vector(x))
  q <- length(x)
  k <- floor(0.5 * (sqrt(8 * q + 9) - 3))
  mu <- x[1:k]
  sigmacap <- sym_of_vech(x[(k + 1):q])
  qcap <- chol(sigmacap)
  zcap <- matrix(
    data = stats::rnorm(
      n = n * k
    ),
    nrow = n,
    ncol = k
  )
  ones <- matrix(
    data = 1,
    nrow = n,
    ncol = 1
  )
  output <- (
    zcap %*% qcap + (
      ones %*% mu
    )
  )
  if (!is.null(varnames)) {
    stopifnot(length(varnames) == k)
    colnames(output) <- varnames
  }
  if (data_frame) {
    output <- as.data.frame(output)
  }
  return(output)
}
