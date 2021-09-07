#' Generate Data
#' from the Multivariate Normal Distribution
#' Using the Cholesky Decomposition - Strict Half-Vectorization
#' of the Correlation Matrix Input
#'
#' @details
#' # Dependencies
#' * [sym_of_vechs()]
#' * [vechs()] (test)
#' * [vechsnames()] (test)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param n Positive integer.
#'   `n` variates.
#' @param x Numeric vector.
#'   Parameter.
#'   Strict half-vectorization
#'   of the correlation matrix
#'   \eqn{\mathrm{vechs} \left( \mathbf{P} \right)}.
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
#' x <- rmvn_chol_of_vechsrhocap(
#'   n = 100,
#'   x = 0.5
#' )
#'
#' colMeans(x)
#' cov(x)
#' @export
#' @family Multivariate Normal Distribution Functions
#' @keywords multiNorm
rmvn_chol_of_vechsrhocap <- function(n,
                                     x,
                                     varnames = NULL,
                                     data_frame = FALSE) {
  stopifnot(
    is.vector(n),
    length(n) == 1
  )
  stopifnot(
    !any(
      abs(as.vector(x)) > 1
    )
  )
  x <- sym_of_vechs(x, diags = 1)
  n <- as.integer(n)
  k <- dim(x)[1]
  qcap <- chol(x)
  zcap <- matrix(
    data = stats::rnorm(
      n = n * k
    ),
    nrow = n,
    ncol = k
  )
  output <- zcap %*% qcap
  if (!is.null(varnames)) {
    stopifnot(length(varnames) == k)
    colnames(output) <- varnames
  }
  if (data_frame) {
    output <- as.data.frame(output)
  }
  return(output)
}
