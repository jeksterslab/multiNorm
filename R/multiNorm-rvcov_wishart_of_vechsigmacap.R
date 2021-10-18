#' Generate Sample Covariances
#' from the Wishart Distribution - Half-Vectorization
#' of the Covariance Matrix Input
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric vector.
#'   Parameter.
#'   Half-vectorization of the covariance matrix
#'   \eqn{ \mathrm{vech} \left( \boldsymbol{\Sigma} \right)}.
#' @inheritParams rvcov_wishart
#'
#' @details
#' # Dependencies
#' * [sym_of_vech()]
#'
#' @returns A list (`list = TRUE`) or matrix (`list = FALSE`).
#'
#' @examples
#' rvcov_wishart_of_vechsigmacap(
#'   rcap = 5,
#'   x = c(1, 0.5, 1),
#'   df = 100,
#'   list = FALSE
#' )
#' @export
#' @family Multivariate Normal Distribution Functions
#' @keywords multiNorm random wishart
rvcov_wishart_of_vechsigmacap <- function(rcap,
                                          x,
                                          df,
                                          list = TRUE,
                                          vec = FALSE) {
  return(
    rvcov_wishart(
      rcap = rcap,
      sigmacap = sym_of_vech(x),
      df = df,
      list = list,
      vec = vec
    )
  )
}
