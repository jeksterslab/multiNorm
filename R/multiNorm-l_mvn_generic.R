#' Log of the Likelihood of the Multivariate Normal Distribution - Generic
#'
#' Calculates the log of the likelihood function
#' of the multivariate normal distribution.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric vector of length `k`.
#'   The ith vector of observations.
#'   Or numeric matrix of size `n` by `k`.
#' @param mu Numeric vector of length `k`.
#'   Mean vector.
#' @param sigmacap Numeric matrix of size `k` by `k`.
#'   Covariance matrix.
#'
#' @returns A vector.
#'
#' @examples
#' n <- 100
#' mu <- c(0, 0)
#' sigmacap <- matrix(
#'   data = c(
#'     1, 0.5, 0.5, 1
#'   ),
#'   nrow = 2
#' )
#'
#' xcap <- rmvn_chol(
#'   n = n,
#'   mu = mu,
#'   sigmacap = sigmacap
#' )
#'
#' l_mvn_generic(
#'   x = xcap[1, ],
#'   mu = mu,
#'   sigmacap = sigmacap
#' )
#'
#' l_mvn_generic(
#'   x = xcap,
#'   mu = mu,
#'   sigmacap = sigmacap
#' )
#' @importFrom stats mahalanobis
#' @export
#' @family Multivariate Normal Distribution Functions
#' @keywords multiNorm likelihood
l_mvn_generic <- function(x,
                          mu,
                          sigmacap) {
  stopifnot(
    is.vector(x) || is.matrix(x) || is.data.frame(x),
    is.vector(mu),
    is.matrix(sigmacap)
  )
  k <- dim(sigmacap)[1]
  constant <- k * log(2 * pi)
  term_01 <- log(det(sigmacap))
  if (k == 1) {
    qcap <- 1 / as.vector(sigmacap)
  } else {
    qcap <- solve(sigmacap)
  }
  if (is.vector(x)) {
    # d <- x - mu
    # if (k == 1) {
    #  term_02 <- d^2 * qcap
    # } else {
    #  term_02 <- crossprod(d, qcap) %*% d
    # }
    term_02 <- stats::mahalanobis(
      x,
      center = mu,
      cov = qcap,
      inverted = TRUE
    )
    return(
      as.vector(
        -0.5 * (
          constant + term_01 + term_02
        )
      )
    )
  } else {
    stopifnot(
      k == dim(x)[2]
    )
    n <- dim(x)[1]
    term_02 <- sum(
      stats::mahalanobis(
        x = x,
        center = mu,
        cov = qcap,
        inverted = TRUE
      )
    )
    return(
      as.vector(
        -0.5 * (
          (
            n * constant
          ) + (
            n * term_01
          ) + term_02
        )
      )
    )
  }
}
