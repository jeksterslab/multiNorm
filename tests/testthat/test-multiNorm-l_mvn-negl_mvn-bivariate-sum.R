## ---- test-multiNorm-l_mvn-negl_mvn-bivariate-sum
tol_i <- 0.05
n_i <- 1000
mu_i <- c(0, 0)
sigmacap_i <- matrix(
  data = c(
    1, 0.5, 0.5, 1
  ),
  nrow = 2
)
xcap_i <- rmvn_chol(
  n = n_i,
  mu = mu_i,
  sigmacap = sigmacap_i
)
theta_i <- c(
  colMeans(xcap_i),
  vech(((n_i - 1) / n_i) * stats::cov(xcap_i))
)
opt_i <- nlminb(
  theta_i,
  objective = negl_mvn,
  x = xcap_i
)
while (opt_i$convergence != 0) {
  opt_i <- nlminb(
    opt_i$par,
    objective = negl_mvn,
    x = xcap_i
  )
}
answer_i <- opt_i$objective
result_01 <- sum(
  sapply(
    X = as.data.frame(t(xcap_i)),
    FUN = negl_mvn,
    theta = theta_i
  )
)
result_02 <- negl_mvn(
  theta = theta_i,
  x = xcap_i
)
testthat::test_that("test-multiNorm-l_mvn-negl_mvn-bivariate-sum 1", {
  testthat::expect_true(
    all(
      abs(
        result_01 - answer_i
      ) <= tol_i
    )
  )
})
testthat::test_that("test-multiNorm-l_mvn-negl_mvn-bivariate-sum 2", {
  testthat::expect_true(
    all(
      abs(
        result_02 - answer_i
      ) <= tol_i
    )
  )
})
# clean environment
rm(
  tol_i,
  n_i,
  mu_i,
  sigmacap_i,
  xcap_i,
  theta_i,
  answer_i,
  result_01,
  result_02,
  opt_i
)
