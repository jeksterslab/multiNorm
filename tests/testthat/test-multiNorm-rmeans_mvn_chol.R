## ---- test-multiNorm-rmeans_mvn_chol
tol_i <- 0.05
n_i <- 100
k_i <- sample(x = 2:10, size = 1)
data_i <- rmeans_mvn_chol(
  rcap = 10000,
  mu = rep(x = 0, times = k_i),
  sigmacap = toeplitz((k_i:1) / k_i),
  n = n_i
)
testthat::test_that("multiNorm-rmeans_mvn_chol means", {
  testthat::expect_true(
    all(
      abs(
        round(
          colMeans(data_i),
          digits = 0
        ) - 0
      ) <= tol_i
    )
  )
})
testthat::test_that("multiNorm-rmeans_mvn_chol covariances", {
  testthat::expect_true(
    all(
      abs(
        round(
          as.vector(
            cov(data_i)
          ),
          digits = 2
        ) - round(
          as.vector(
            toeplitz((k_i:1) / k_i) / n_i
          ),
          digits = 2
        )
      ) <= tol_i
    )
  )
})
# coverage
data_i <- rmeans_mvn_chol(
  rcap = 10,
  mu = rep(x = 0, times = k_i),
  sigmacap = toeplitz((k_i:1) / k_i),
  n = n_i,
  varnames = paste0("x", seq_len(k_i)),
  list = TRUE
)
head(data_i)
# clean environment
rm(
  n_i,
  tol_i,
  k_i,
  data_i
)
