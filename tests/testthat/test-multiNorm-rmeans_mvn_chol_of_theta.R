## ---- test-multiNorm-rmeans_mvn_chol_of_theta
tol_i <- 0.05
n_i <- 100
k_i <- sample(x = 2:10, size = 1)
data_i <- rmeans_mvn_chol_of_theta(
  rcap = 10000,
  x = c(
    rep(x = 0, times = k_i),
    vech(toeplitz((k_i:1) / k_i))
  ),
  n = n_i
)
testthat::test_that("multiNorm-rmeans_mvn_chol_of_theta means", {
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
testthat::test_that("multiNorm-rmeans_mvn_chol_of_theta covariances", {
  testthat::expect_true(
    all(
      abs(
        round(
          as.vector(
            cov(data_i)
          ),
          digits = 1
        ) - as.vector(
          toeplitz((k_i:1) / k_i) / n_i
        )
      ) <= tol_i
    )
  )
})
# coverage
data_i <- rmeans_mvn_chol_of_theta(
  rcap = 10,
  x = c(
    rep(x = 0, times = k_i),
    vech(toeplitz((k_i:1) / k_i))
  ),
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
