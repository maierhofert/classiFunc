context("parallelComputeDistMat")

test_that("Parallel computeDistMat returns the same values as unparallelized version", {
  set.seed(123)
  dat = matrix(rnorm(n = 20), 5, byrow = TRUE)

  a1 = computeDistMat(dat)
  a2 = parallelComputeDistMat(dat, batches = 2)
  expect_true(all.equal(as.vector(a1), as.vector(a2)))


  b1 = computeDistMat(dat, method = "max")
  b2 = parallelComputeDistMat(dat, method = "max", batches = 5)
  expect_true(all.equal(as.vector(b1), as.vector(b2)))
})

test_that("Works with kernel / knn", {
  set.seed(123)
  trn = matrix(rnorm(n = 20), 5, byrow = TRUE)
  tst = matrix(rnorm(n = 20), 5, byrow = TRUE)

  mod = classiKnn(c(1, 1, 2, 2, 1), fdata = trn, knn = 1L)
  pred = predict(mod, newdata = tst)
  pred2 = predict(mod, newdata = tst, parallel = TRUE, batches = 2)

  expect_true(all.equal(pred, pred2))
})
