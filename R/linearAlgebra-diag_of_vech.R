#' Diagonals of A from vech(A)
#'
#' Diagonals of a matrix from its half-vectorization.
#'
#' Generates a vector of length \eqn{m}
#' of diagonal elements or location in the input vector
#' of an \eqn{m \times m} symmetric matrix.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Vector.
#' @param loc Logical.
#' If `loc = TRUE`, return the location of the diagonal elements
#' in the input vector.
#' If `loc = FALSE`, return the values of the diagonal elements.
#'
#' @references
#' [Wikipedia: Half-vectorization](https://en.wikipedia.org/wiki/Vectorization_(mathematics)#Half-vectorization)
#'
#' @returns A vector.
#'
#' @examples
#' A <- matrix(
#'   data = c(
#'     1.0, 0.5, 0.4,
#'     0.5, 1.0, 0.6,
#'     0.4, 0.6, 1.0
#'   ),
#'   ncol = 3
#' )
#' vechA <- c(1.0, 0.5, 0.4, 1.0, 0.6, 1.0)
#'
#' diag_of_vech(vechA, loc = FALSE)
#' diag_of_vech(vechA, loc = TRUE)
#' @export
#' @family Linear Algebra Functions
#' @keywords linearAlgebra symmetric
diag_of_vech <- function(x,
                         loc = FALSE) {
  # m = m by m dimensions of the symmetric matrix
  # length(x) = m(m + 1) / 2 solve for m
  stopifnot(
    is.vector(x),
    is.logical(loc)
  )
  m <- floor(0.5 * (sqrt(1 + 8 * length(x)) - 1))
  if (length(x) == 1) {
    if (loc) {
      return(1)
    } else {
      return(x[1])
    }
  }
  i <- sapply(
    X = seq_len(m),
    FUN = function(i, m) {
      m * (i - 1) + 1 - (
        (
          (i - 2) * (i - 1)
        ) / 2
      )
    },
    m = m
  )
  if (loc) {
    return(i)
  } else {
    return(x[i])
  }
}
