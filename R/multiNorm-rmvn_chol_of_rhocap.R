#' Generate Data
#' from the Multivariate Normal Distribution
#' Using the Cholesky Decomposition - Correlation Matrix Input
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix.
#'   Parameter.
#'   Correlation matrix
#'   \eqn{\mathbf{P}}.
#' @inheritParams rmvn_chol
#'
#' @returns A matrix (`data_frame = FALSE`) or data.frame (`data_frame = TRUE`).
#'
#' @examples
#' rhocap <- matrix(
#'   data = c(
#'     1, 0.5, 0.5, 1
#'   ),
#'   nrow = 2
#' )
#'
#' rmvn_chol_of_rhocap(
#'   n = 5,
#'   x = rhocap
#' )
#' @export
#' @family Multivariate Normal Distribution Functions
#' @keywords multiNorm
rmvn_chol_of_rhocap <- function(n,
                                x,
                                varnames = NULL,
                                data_frame = FALSE) {
  stopifnot(
    is.matrix(x),
    length(n) == 1
  )
  n <- as.integer(n)
  k <- dim(x)[1]
  stopifnot(
    k == dim(x)[2],
    x == t(x)
  )
  # test x is a correlation matrix
  stopifnot(
    all(
      diag(x) == 1
    )
  )
  stopifnot(
    !any(
      abs(as.vector(x)) > 1
    )
  )
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
