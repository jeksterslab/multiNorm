## ---- test-multiNorm-rvcov_wishart
tol_i <- 0.05
k_i <- sample(x = 2:10, size = 1)
data_i <- rvcov_wishart(
  rcap = 10000,
  sigmacap = toeplitz((k_i:1) / k_i),
  df = 100,
  list = FALSE,
  vec = TRUE
)
testthat::test_that("multiNorm-rvcov_wishart means", {
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
data_i <- rvcov_wishart(
  rcap = 10,
  sigmacap = toeplitz((k_i:1) / k_i),
  df = 10,
  list = FALSE,
  vec = FALSE
)
data_i <- rvcov_wishart(
  rcap = 10,
  sigmacap = toeplitz((k_i:1) / k_i),
  df = 10,
  list = TRUE,
  vec = TRUE
)
data_i <- rvcov_wishart(
  rcap = 10,
  sigmacap = toeplitz((k_i:1) / k_i),
  df = 10,
  list = TRUE,
  vec = FALSE
)
# clean environment
rm(
  tol_i,
  k_i,
  data_i
)
