## ---- test-linearAlgebra-diag_of_vech
x_i <- c(1.0, 0.5, 0.4, 1.0, 0.6, 1.0)
answer_i <- c(1.0, 1.0, 1.0)
result_i <- diag_of_vech(
  x_i,
  loc = FALSE
)
testthat::test_that("test-linearAlgebra-diag_of_vech 3 by 3 loc = FALSE", {
  testthat::expect_equal(
    result_i,
    answer_i
  )
})
answer_i <- c(1, 4, 6)
result_i <- diag_of_vech(
  x_i,
  loc = TRUE
)
testthat::test_that("test-linearAlgebra-diag_of_vech 3 by 3 loc = TRUE", {
  testthat::expect_equal(
    result_i,
    answer_i
  )
})
x_i <- c("a", "b", "c")
answer_i <- c("a", "c")
result_i <- diag_of_vech(
  x_i,
  loc = FALSE
)
testthat::test_that("test-linearAlgebra-diag_of_vech 2 by 2 with loc = FALSE", {
  testthat::expect_equal(
    result_i,
    answer_i
  )
})
x_i <- "a"
answer_i <- "a"
result_i <- diag_of_vech(
  x_i,
  loc = FALSE
)
testthat::test_that("test-linearAlgebra-diag_of_vech 1 by 1", {
  testthat::expect_equal(
    result_i,
    answer_i
  )
})
answer_i <- 1
result_i <- diag_of_vech(
  x_i,
  loc = TRUE
)
testthat::test_that("test-linearAlgebra-diag_of_vech 1 by 1", {
  testthat::expect_equal(
    result_i,
    answer_i
  )
})
n_i <- 100
k_i <- sample(
  1:10,
  size = 1
)
mu_i <- rep(
  x = 0,
  times = k_i
)
sigmacap_i <- matrix(
  runif(
    n = 1,
    min = 0,
    max = 1
  ),
  nrow = k_i,
  ncol = k_i
)
diag(sigmacap_i) <- 1
x_i <- matrix(
  data = rnorm(
    n = n_i * k_i
  ),
  nrow = n_i,
  ncol = k_i
) %*% (
  chol(sigmacap_i)
) + (
  matrix(
    data = 1,
    nrow = n_i,
    ncol = 1
  ) %*% mu_i
)
answer_i <- diag(
  cov(x_i)
)
result_i <- diag_of_vech(
  vech(
    cov(x_i),
    names = FALSE
  )
)
testthat::test_that("test-linearAlgebra-diag_of_vech random cov", {
  testthat::expect_equal(
    result_i,
    answer_i
  )
})
# expect_error
testthat::test_that("test-linearAlgebra-diag_of_vech error", {
  testthat::expect_error(
    diag_of_vech(as.matrix(1:5))
  )
})
# clean environment
rm(
  x_i,
  n_i,
  k_i,
  mu_i,
  sigmacap_i,
  answer_i,
  result_i
)
