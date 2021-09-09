#' Generate Sample Mean Vector
#' from the Multivariate Normal Distribution
#' Using the Cholesky Decomposition - Parameter Vector theta Input
#'
#' @details
#' # Dependencies
#' [rmeans_mvn_chol()]
#' [rmvn_chol()]
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param n Positive integer.
#'   Sample size.
#' @inheritParams rmeans_mvn_chol
#' @inheritParams rmvn_chol_of_theta
#'
#' @inherit rmeans_mvn_chol return
#'
#' @examples
#' x <- rmeans_mvn_chol_of_theta(
#'   rcap = 100,
#'   n = 100,
#'   x = c(0, 0, 1, 0.5, 1)
#' )
#'
#' colMeans(x)
#' cov(x)
#' @export
#' @family Multivariate Normal Distribution Functions
#' @keywords multiNorm
rmeans_mvn_chol_of_theta <- function(rcap,
                                     x,
                                     n,
                                     varnames = NULL,
                                     list = FALSE) {
  stopifnot(is.vector(x))
  q <- length(x)
  k <- floor(0.5 * (sqrt(8 * q + 9) - 3))
  mu <- x[1:k]
  sigmacap <- sym_of_vech(x[(k + 1):q])
  return(
    rmeans_mvn_chol(
      rcap = rcap,
      mu = mu,
      sigmacap = sigmacap,
      n = n,
      varnames = varnames,
      list = list
    )
  )
}
