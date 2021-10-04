## ---- test-multiNorm-rvcov_wishart_of_vechsigmacap
tol_i <- 0.05
k_i <- sample(x = 2:10, size = 1)
vech_i <- toeplitz((k_i:1) / k_i)
vech_i <- vech_i[lower.tri(vech_i, diag = TRUE)]
data_i <- rvcov_wishart_of_vechsigmacap(
  rcap = 10000,
  x = vech_i,
  df = 100,
  list = FALSE,
  vec = TRUE
)
testthat::test_that("test-multiNorm-rvcov_wishart_of_vechsigmacap means", {
  testthat::expect_true(
    all(
      abs(
        round(
          colMeans(data_i),
          digits = 2
        ) - round(
          as.vector(
            toeplitz((k_i:1) / k_i)
          ),
          digits = 2
        )
      ) <= tol_i
    )
  )
})
# coverage
data_i <- rvcov_wishart_of_vechsigmacap(
  rcap = 10,
  x = vech_i,
  df = 10,
  vec = TRUE
)
data_i <- rvcov_wishart_of_vechsigmacap(
  rcap = 10,
  x = vech_i,
  df = 10,
  list = TRUE
)
# clean environment
rm(
  tol_i,
  k_i,
  data_i,
  vech_i
)
