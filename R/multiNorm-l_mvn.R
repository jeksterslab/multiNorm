#' Log of the Likelihood of the Multivariate Normal Distribution
#'
#' Calculates the log of the likelihood function
#' of the multivariate normal distribution.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param theta Numeric vector.
#'   Parameter vector
#'   \eqn{
#'     \boldsymbol{\theta}
#'     =
#'     \left\{
#'     \boldsymbol{\mu},
#'     \mathrm{vech}
#'     \left(
#'     \boldsymbol{\Sigma}
#'     \right)
#'     \right\}^{\prime}
#' }
#' @param x Numeric vector of length `k`.
#'   The ith vector of observations.
#'   Or numeric matrix of size `n` by `k`.
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
#' theta <- c(
#'   mu,
#'   linearAlgebra::vech(sigmacap)
#' )
#' xcap <- rmvn_chol(
#'   n = n,
#'   mu = mu,
#'   sigmacap = sigmacap
#' )
#'
#' l_mvn(
#'   x = xcap[1, ],
#'   theta = theta
#' )
#'
#' l_mvn(
#'   x = xcap,
#'   theta = theta
#' )
#' @export
#' @family Multivariate Normal Distribution Functions
#' @keywords multiNorm likelihood
l_mvn <- function(theta,
                  x) {
  theta <- mvn_theta_helper(theta)
  return(
    l_mvn_generic(
      x = x,
      mu = theta$mu,
      sigmacap = theta$sigmacap
    )
  )
}
