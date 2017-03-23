
context("computeDistMat")

test_that("computeDistMat works for all specified metrics", {
  set.seed(123)
  dat = matrix(rnorm(n = 20), 5, byrow = TRUE)
  mets = metric.choices()
  mets.no.add.params = mets[!mets %in% c("Minkowski", "Lp",
                                         "Mahalanobis",
                                         "fJaccard", "fuzzy_Jaccard",
                                         "Bhjattacharyya",
                                         "Kullback", "Leibler",
                                         "Hellinger")]
  distMats = lapply(mets.no.add.params, computeDistMat, x = dat, y = dat)
  expect_true(all(sapply(distMats, is.matrix)))


  # Minkowski needs additional parameter p
  distMat.Minkowski = computeDistMat(x = dat, method = "Minkowski", p = 2)
  assertMatrix(distMat.Minkowski)
  # # Mahalanobis does not work for this simple data
  # distMat.Mahalanobis = computeDistMat(x = dat, method = "Mahalanobis")
  # distMat.Mahalanobis



  # Bhjattacharyya, Kullback Leibler and Hellinger are for distributions
  # create arbitrary distributions
  dat = abs(dat)
  dat = t(apply(dat, 1, function(x) return(x / sum(x))))

  distMat.Bhjattacharyya = computeDistMat(x = dat, method = "Bhjattacharyya")
  assertMatrix(distMat.Bhjattacharyya) # ok, this is meant for distributions
  distMat.Kullback = computeDistMat(x = dat, method = "Kullback")
  assertMatrix(distMat.Kullback) # ok, this is also meant for distributions
  distMat.Hellinger = computeDistMat(x = dat, method = "Hellinger")
  assertMatrix(distMat.Hellinger)
  distMat.fJaccard = computeDistMat(x = dat, method = "fJaccard", p = 2)
  assertMatrix(distMat.fJaccard)
})

test_that("computeDistMat works for custom metrics", {
  set.seed(123)
  dat = matrix(rnorm(n = 20), 5, byrow = TRUE)

  # default
  distMat = computeDistMat(x = dat, method = "custom.metric")
  distEuclidean = computeDistMat(x = dat, method = "Euclidean")

  # custom metric
  myManhattan = function(x,y) {return(sum(abs(x - y)))}
  distMat2 = computeDistMat(x = dat, method = "custom.metric",
                            custom.metric = myManhattan)
  distManhattan = computeDistMat(x = dat, method = "Manhattan")
  expect_equal(distMat2, distManhattan)

  # custom metric with additional parameters
  myMinkowski = function(x, y, lp) {
    return(sum(abs(x - y) ^ lp) ^ (1 / lp))
  }
  distMat3 = computeDistMat(x = dat, method = "custom.metric",
                            custom.metric = myMinkowski, lp = 3)
  distMinkowski = computeDistMat(x = dat, method = "Minkowski", p = 3)
  expect_equal(distMat3, distMinkowski)

})

