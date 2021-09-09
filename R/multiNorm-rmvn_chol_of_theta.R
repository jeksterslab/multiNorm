#' Generate Data
#' from the Multivariate Normal Distribution
#' Using the Cholesky Decomposition - Parameter Vector theta Input
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
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
#' @inheritParams rmvn_chol
#'
#' @returns A matrix (`data_frame = FALSE`) or data.frame (`data_frame = TRUE`).
#'
#' @examples
#' x <- rmvn_chol_of_theta(
#'   n = 100,
#'   x = c(0, 0, 1, 0.5, 1)
#' )
#'
#' colMeans(x)
#' cov(x)
#' @export
#' @family Multivariate Normal Distribution Functions
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
