#' Gradient Vector of the Multivariate Normal Distribution - Generic
#'
#' Calculates gradient vector of the log of the likelihood function
#' of the multivariate normal distribution
#' for the ith observation.
#'
#' @param x Numeric vector of length `k`.
#'   The ith vector of observations.
#' @param mu Numeric vector.
#'   Parameter.
#'   Mean vector
#'   \eqn{\boldsymbol{\mu}}.
#' @param sigmacap Numeric matrix.
#'   Parameter.
#'   Covariance matrix
#'   \eqn{\boldsymbol{\Sigma}}.
#' @param qcap Numeric matrix.
#'   Inverse of the covariance matrix
#'   \eqn{\mathbf{Q}}.
#'
#' @returns A vector.
#'
#' @examples
#' n <- 5
#' mu <- c(0, 0)
#' sigmacap <- matrix(
#'   data = c(
#'     1, 0.5, 0.5, 1
#'   ),
#'   nrow = 2
#' )
#'
#' xcap <- as.data.frame(
#'   t(
#'     rmvn_chol(
#'       n = n,
#'       mu = mu,
#'       sigmacap = sigmacap
#'     )
#'   )
#' )
#'
#' lapply(
#'   X = xcap,
#'   FUN = grad_l_mvn_generic,
#'   mu = mu,
#'   sigmacap = sigmacap
#' )
#' @export
#' @family Multivariate Normal Distribution Functions
#' @keywords multiNorm derivatives
grad_l_mvn_generic <- function(x,
                               mu,
                               sigmacap,
                               qcap = NULL) {
  stopifnot(
    is.vector(x) || is.matrix(x) || is.data.frame(x),
    is.vector(mu),
    is.matrix(sigmacap)
  )
  d <- x - mu
  k <- dim(sigmacap)[1]
  if (k == 1) {
    sigmacap <- as.vector(sigmacap)
    if (is.null(qcap)) {
      qcap <- 1 / sigmacap
    }
    wrt_mu <- d * qcap # d / sigmacap
    wrt_sigmacap <- -1 * (
      (
        0.5 * qcap # 0.5 / sigmacap
      ) - (
        (0.5 * d^2) * qcap^2 # (0.5 * d^2) / sigmacap^2
      )
    )
  } else {
    if (is.null(qcap)) {
      qcap <- chol2inv(chol(sigmacap))
    }
    wrt_mu <- as.vector(qcap %*% d)
    # D' vec(d(l)/d(sigmacap))
    wrt_sigmacap <- as.vector(
      t(dcap(k)) %*% as.vector(
        -1 * (
          (
            0.5 * qcap
          ) - (
            0.5 * (
              (
                qcap %*% d
              ) %*% crossprod(
                d,
                qcap
              )
            )
          )
        )
      )
    )
    # wrt_sigmacap <- matrix(
    #  data = as.vector(
    #    -1 * (
    #      (
    #        0.5 * qcap
    #      ) - (
    #        0.5 * (
    #          (qcap %*% d) %*% crossprod(d, qcap)
    #        )
    #      )
    #    )
    #  ),
    #  nrow = k,
    #  ncol = k
    # )
    # diags <- diag(wrt_sigmacap)
    # wrt_sigmacap <- wrt_sigmacap + wrt_sigmacap
    # diag(wrt_sigmacap) <- diags
    ## vech sigmacap
    # wrt_sigmacap <- vech(wrt_sigmacap)
  }
  return(
    c(
      wrt_mu,
      wrt_sigmacap
    )
  )
}
