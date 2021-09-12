## ---- test-multiNorm-rmvn_chol_of_theta
tol_i <- 0.05
k_i <- sample(x = 2:10, size = 1)
vech_i <- toeplitz((k_i:1) / k_i)
vech_i <- vech_i[lower.tri(vech_i, diag = TRUE)]
theta_i <- c(
  rep(x = 0, times = k_i),
  vech_i
)
data_i <- rmvn_chol_of_theta(
  n = 10000,
  x = theta_i
)
testthat::test_that("multiNorm-rmvn_chol_of_theta means", {
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
testthat::test_that("multiNorm-rmvn_chol_of_theta covariances", {
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
            toeplitz((k_i:1) / k_i)
          ),
          digits = 2
        )
      ) <= tol_i
    )
  )
})
# coverage
data_i <- rmvn_chol_of_theta(
  n = 10,
  x = theta_i,
  varnames = paste0("x", seq_len(k_i)),
  data_frame = TRUE
)
head(data_i)
# clean environment
rm(
  tol_i,
  k_i,
  theta_i,
  data_i,
  vech_i
)
