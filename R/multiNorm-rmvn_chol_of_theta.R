#' Generate Data
#' from the Multivariate Normal Distribution
#' Using the Cholesky Decomposition - Parameter Vector theta Input
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams mvn_theta_helper
#' @inheritParams rmvn_chol
#'
#' @returns A matrix (`data_frame = FALSE`) or data.frame (`data_frame = TRUE`).
#'
#' @examples
#' rmvn_chol_of_theta(
#'   n = 5,
#'   x = c(0, 0, 1, 0.5, 1)
#' )
#' @export
#' @family Multivariate Normal Distribution Functions
#' @keywords multiNorm random mvn
rmvn_chol_of_theta <- function(n,
                               x,
                               varnames = NULL,
                               data_frame = FALSE) {
  theta <- mvn_theta_helper(x)
  return(
    rmvn_chol(
      n = n,
      mu = theta$mu,
      sigmacap = theta$sigmacap,
      varnames = varnames,
      data_frame = data_frame
    )
  )
}
