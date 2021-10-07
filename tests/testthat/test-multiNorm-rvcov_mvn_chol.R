## ---- test-multiNorm-rvcov_mvn_chol
tol_i <- 0.05
rcap_i <- 10000
n_i <- 50
k_i <- sample(x = 2:10, size = 1)
sigmacap_i <- toeplitz((k_i:1) / k_i)
kcap_i <- kcap(dim(sigmacap_i)[1])
gammacap_i <- 2 * kcap_i %*% tcrossprod(
  kronecker(
    sigmacap_i,
    sigmacap_i
  ),
  kcap_i
)
list_i <- rvcov_mvn_chol(
  rcap = rcap_i,
  sigmacap = sigmacap_i,
  gammacap = gammacap_i,
  n = n_i,
  list = TRUE
)
list_i <- (1 / rcap_i) * Reduce(
  "+",
  list_i
)
vec_i <- rvcov_mvn_chol(
  rcap = rcap_i,
  sigmacap = sigmacap_i,
  gammacap = gammacap_i,
  n = n_i,
  list = FALSE,
  vec = TRUE
)
vec_i <- colMeans(vec_i)
vech_i <- rvcov_mvn_chol(
  rcap = rcap_i,
  sigmacap = sigmacap_i,
  gammacap = gammacap_i,
  n = n_i,
  list = FALSE,
  vec = FALSE
)
vech_i <- colMeans(vech_i)
testthat::test_that("test-multiNorm-rvcov_mvn_chol list", {
  testthat::expect_true(
    all(
      abs(
        round(
          as.vector(list_i),
          digits = 2
        ) - round(
          as.vector(sigmacap_i),
          digits = 2
        )
      ) <= tol_i
    )
  )
})
testthat::test_that("test-multiNorm-rvcov_mvn_chol vec", {
  testthat::expect_true(
    all(
      abs(
        round(
          vec_i,
          digits = 2
        ) - round(
          as.vector(sigmacap_i),
          digits = 2
        )
      ) <= tol_i
    )
  )
})
testthat::test_that("test-multiNorm-rvcov_mvn_chol vech", {
  testthat::expect_true(
    all(
      abs(
        round(
          vech_i,
          digits = 2
        ) - round(
          vech(sigmacap_i),
          digits = 2
        )
      ) <= tol_i
    )
  )
})
# clean environment
rm(
  tol_i,
  n_i,
  rcap_i,
  k_i,
  sigmacap_i,
  kcap_i,
  gammacap_i,
  list_i,
  vec_i,
  vech_i
)
