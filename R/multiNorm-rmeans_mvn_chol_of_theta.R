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
#' rmeans_mvn_chol_of_theta(
#'   rcap = 5,
#'   n = 100,
#'   x = c(0, 0, 1, 0.5, 1)
#' )
#' @export
#' @family Multivariate Normal Distribution Functions
#' @keywords multiNorm
rmeans_mvn_chol_of_theta <- function(rcap,
                                     x,
                                     n,
                                     varnames = NULL,
                                     list = FALSE) {
  theta <- theta_helper(x)
  return(
    rmeans_mvn_chol(
      rcap = rcap,
      mu = theta$mu,
      sigmacap = theta$sigmacap,
      n = n,
      varnames = varnames,
      list = list
    )
  )
}
