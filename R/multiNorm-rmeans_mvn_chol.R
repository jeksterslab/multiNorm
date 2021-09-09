#' Generate Sample Mean Vector
#' from the Multivariate Normal Distribution
#' Using the Cholesky Decomposition
#'
#' @details
#' # Dependencies
#' [rmvn_chol()]
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param rcap Positive integer.
#'   `R` variates.
#' @param n Positive integer.
#'   Sample size.
#' @param list Logical.
#'   If `list = TRUE`,
#'   returns a list where each element is a vector of means.
#'   If `list = FALSE`,
#'   returns a matrix where each row is a vector of means.
#' @inheritParams rmvn_chol
#'
#' @returns A list (`list = TRUE`) or a matrix (`list = FALSE`).
#'
#' @examples
#' mu <- c(0, 0)
#' sigmacap <- matrix(
#'   data = c(
#'     1, 0.5, 0.5, 1
#'   ),
#'   nrow = 2
#' )
#'
#' x <- rmeans_mvn_chol(
#'   rcap = 100,
#'   mu = mu,
#'   sigmacap = sigmacap,
#'   n = 100
#' )
#'
#' colMeans(x)
#' @export
#' @family Multivariate Normal Distribution Functions
#' @keywords multiNorm
rmeans_mvn_chol <- function(rcap,
                            mu,
                            sigmacap,
                            n,
                            varnames = NULL,
                            list = FALSE) {
  n <- as.integer(n)
  output <- rmvn_chol(
    n = rcap,
    mu = mu,
    sigmacap = (sigmacap / n), # this should always be divided by n
    varnames = varnames,
    data_frame = FALSE
  )
  if (list) {
    output <- lapply(
      X = seq_len(rcap),
      FUN = function(i) {
        output <- as.vector(output[i, ])
        if (!is.null(varnames)) {
          names(output) <- varnames
        }
        return(output)
      }
    )
  }
  return(output)
}
