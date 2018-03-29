
context("computeDistMat")

test_that("computeDistMat works for all specified metrics", {
  set.seed(123)
  dat = matrix(rnorm(n = 20), 5, byrow = TRUE)
  mets = metricChoices()
  mets_no_add_params = mets[!mets %in% c("Minkowski", "Lp",
                                         "Mahalanobis",
                                         "fJaccard", "fuzzy_Jaccard",
                                         "Bhjattacharyya",
                                         "Kullback", "Leibler",
                                         "Hellinger")]
  distMats = lapply(mets_no_add_params, computeDistMat, x = dat, y = dat)
  expect_true(all(sapply(distMats, is.matrix)))


  # Minkowski needs additional parameter p
  distMat.Minkowski = computeDistMat(x = dat, method = "Minkowski", p = 2)
  assertMatrix(distMat.Minkowski)

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
  myManhattan = function(x,y) {
    return(sum(abs(x - y)))
  }
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


test_that("computeDistMat works for newly implemented metrics suggested in Fuchs etal 2015", {
  set.seed(123)
  dat = matrix(rnorm(n = 20), 5, byrow = TRUE)

  # calculate distance matrices
  dist1 = computeDistMat(x = dat, method = "Euclidean")
  dist2 = computeDistMat(x = dat, method = "shortEuclidean")
  dist3 = computeDistMat(x = dat, method = "shortEuclidean", dmin = 0, dmax = 1)

  # custom metric with additional parameters
  expect_true(all(dist1 - dist2 == 0))
  expect_equal(dist2, dist3)

})

test_that("computeDistMat works rucrdtw metrics", {
  skip_if_not_installed("rucrdtw")
  set.seed(123)
  dat = matrix(rnorm(n = 20), 5, byrow = TRUE)

  dist1 = computeDistMat(x = dat, method = "rucrdtw")
  expect_true(is.matrix(dist1))

  dist2 = computeDistMat(x = dat, method = "rucrdtw", dtwwindow = 0.4)
  expect_true(is.matrix(dist2))

  dist3 = computeDistMat(x = dat, method = "rucred")
  expect_true(is.matrix(dist3))

  as.vector(dist1)
  as.vector(dist2)
  as.vector(dist3)
  # For a bigger window, dists have to be <=
  expect_true(all(as.vector(dist1) >= as.vector(dist2)))
  # dtw has to be <= euclidean
  expect_true(all(as.vector(dist1) >= as.vector(dist3)))

})
