#' Generate Data
#' from the Multivariate Normal Distribution
#' Using the Cholesky Decomposition - Strict Half-Vectorization
#' of the Correlation Matrix Input
#'
#' @details
#' # Dependencies
#' * [sym_of_vechs()]
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric vector.
#'   Parameter.
#'   Strict half-vectorization
#'   of the correlation matrix
#'   \eqn{\mathrm{vechs} \left( \mathbf{P} \right)}.
#' @inheritParams rmvn_chol
#'
#' @returns A matrix (`data_frame = FALSE`) or data.frame (`data_frame = TRUE`).
#'
#' @examples
#' rmvn_chol_of_vechsrhocap(
#'   n = 5,
#'   x = 0.5
#' )
#' @export
#' @family Multivariate Normal Distribution Functions
#' @keywords multiNorm random mvn
rmvn_chol_of_vechsrhocap <- function(n,
                                     x,
                                     varnames = NULL,
                                     data_frame = FALSE) {
  return(
    rmvn_chol_of_rhocap(
      n = n,
      x = sym_of_vechs(x, diags = 1),
      varnames = varnames,
      data_frame = data_frame
    )
  )
}
