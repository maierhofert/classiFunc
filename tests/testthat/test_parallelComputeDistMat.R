context("parallelComputeDistMat")

test_that("Parallel computeDistMat returns the same values as unparallelized version", {
  set.seed(123)
  dat = matrix(rnorm(n = 20), 5, byrow = TRUE)
  a1 = computeDistMat(dat)
  a2 = parallelComputeDistMat(dat, ncpus = 2)
  expect_true(all.equal(as.vector(a1), as.vector(a2)))

  b1 = computeDistMat(dat, method = "DTW")
  b2 = parallelComputeDistMat(dat, method = "DTW", ncpus = 5)
  expect_true(all.equal(as.vector(b1), as.vector(b2)))
})
