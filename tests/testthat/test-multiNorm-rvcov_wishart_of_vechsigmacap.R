## ---- test-multiNorm-rvcov_wishart_of_vechsigmacap
tol_i <- 0.01
k_i <- sample(x = 2:10, size = 1)
data_i <- rvcov_wishart_of_vechsigmacap(
  rcap = 10000,
  x = vech(toeplitz((k_i:1) / k_i)),
  df = 10000,
  vector = TRUE
)
testthat::test_that("means", {
  testthat::expect_true(
    all(
      abs(
        colMeans(data_i) - vech(
          toeplitz((k_i:1) / k_i)
        )
      ) <= tol_i
    )
  )
})
# coverage
data_i <- rvcov_wishart_of_vechsigmacap(
  rcap = 10000,
  x = vech(toeplitz((k_i:1) / k_i)),
  df = 10000,
  vector = FALSE
)
# clean environment
rm(
  tol_i,
  k_i,
  data_i
)
