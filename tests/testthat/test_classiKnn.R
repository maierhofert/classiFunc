context("classiKnn")


test_that("classiKnn works for arrow head data set", {
  data("ArrowHead")
  classes = ArrowHead[,"target"]

  set.seed(123)
  train_inds = sample(1:nrow(ArrowHead), size = 0.8 * nrow(ArrowHead), replace = FALSE)
  test_inds = (1:nrow(ArrowHead))[!(1:nrow(ArrowHead)) %in% train_inds]

  ArrowHead = ArrowHead[,!colnames(ArrowHead) == "target"]

  mod1 = classiKnn(classes = classes[train_inds], fdata = ArrowHead[train_inds,])
  mod2 = classiKnn(classes = classes[train_inds], fdata = ArrowHead[train_inds,],
                   nderiv = 1L, knn = 3L)


  pred1 = predict(mod1, predict.type = "prob")
  checkmate::expect_matrix(pred1, any.missing = FALSE,
                           nrows = nrow(mod1$fdata),
                           ncols = length(levels(mod1$classes)))

  pred2 = predict(mod2, newdata = ArrowHead[train_inds,], predict.type = "response")
  checkmate::expect_factor(pred2, any.missing = FALSE, levels = levels(mod2$classes))

  pred3 = predict(mod2, newdata = ArrowHead[1,], predict.type = "response")
  checkmate::expect_factor(pred3, any.missing = FALSE, levels = levels(mod2$classes))
})


test_that("classiKnn works for phoneme data set from fda.usc package", {
  data(phoneme, package = "fda.usc")

  mod1 = classiKnn(classes = phoneme[["classlearn"]], fdata = phoneme[["learn"]]$data)
  mod2 = classiKnn(classes = phoneme[["classlearn"]], fdata = phoneme[["learn"]]$data,
                   nderiv = 1L, knn = 3L)


  pred1 = predict(mod1, predict.type = "prob")
  checkmate::expect_matrix(pred1, any.missing = FALSE,
                           nrows = nrow(mod1$fdata),
                           ncols = length(unique(mod1$classes)))

  pred2 = predict(mod2, newdata = phoneme[["test"]]$dat, predict.type = "response")
  checkmate::expect_factor(pred2, any.missing = FALSE, levels = levels(phoneme[["classtest"]]))

})


test_that("classiKnn works for custom semimetrics", {
  data("ArrowHead")
  classes = ArrowHead[,"target"]

  set.seed(123)
  train_inds = sample(1:nrow(ArrowHead), size = 0.8 * nrow(ArrowHead), replace = FALSE)
  test_inds = (1:nrow(ArrowHead))[!(1:nrow(ArrowHead)) %in% train_inds]

  ArrowHead = ArrowHead[,!colnames(ArrowHead) == "target"]


  # custom metric with additional parameters
  myMinkowski = function(x, y, lp) {
    return(sum(abs(x - y) ^ lp) ^ (1 / lp))
  }

  # create the models
  mod1 = classiKnn(classes = classes[train_inds], fdata = ArrowHead[train_inds,],
                   metric = "custom.metric")
  mod2 = classiKnn(classes = classes[train_inds], fdata = ArrowHead[train_inds,],
                   metric = "custom.metric", custom.metric = myMinkowski, lp = 2)
  mod2.error = classiKnn(classes = classes[train_inds], fdata = ArrowHead[train_inds,],
                         metric = "custom.metric", custom.metric = myMinkowski)
  mod3 = classiKnn(classes = classes[train_inds], fdata = ArrowHead[train_inds,],
                   metric = "Minkowski", p = 2)


  # get the model predictions
  pred1 = predict(mod1, newdata = ArrowHead[train_inds,], predict.type = "response")
  pred2 = predict(mod2, newdata = ArrowHead[train_inds,], predict.type = "response")
  pred3 = predict(mod3, newdata = ArrowHead[train_inds,], predict.type = "response")

  # check the model predictions
  expect_error(predict(mod2.error, newdata = ArrowHead[train_inds,], predict.type = "response"))
  expect_equal(pred1, pred2)
  expect_equal(pred1, pred3)
  expect_equal(pred2, pred3)

})

test_that("classiKnn works for newly implemented semimetrics from Fuchs etal", {
  data("ArrowHead")
  classes = ArrowHead[,"target"]

  set.seed(123)
  train_inds = sample(1:nrow(ArrowHead), size = 0.8 * nrow(ArrowHead), replace = FALSE)
  test_inds = (1:nrow(ArrowHead))[!(1:nrow(ArrowHead)) %in% train_inds]

  ArrowHead = ArrowHead[,!colnames(ArrowHead) == "target"]


  # create the models
  mod1 = classiKnn(classes = classes[train_inds], fdata = ArrowHead[train_inds,],
                   metric = "globMax")
  mod2 = classiKnn(classes = classes[train_inds], fdata = ArrowHead[train_inds,],
                   metric = "points", knn = 5)
  mod3 = classiKnn(classes = classes[train_inds], fdata = ArrowHead[train_inds,],
                   metric = "points", knn = 5, .poi = c(0.1, 0.23, 0.713))

  # get the model predictions
  pred1 = predict(mod1, newdata = ArrowHead[train_inds,], predict.type = "response")
  pred2 = predict(mod2, newdata = ArrowHead[train_inds,], predict.type = "response")
  pred3 = predict(mod3, newdata = ArrowHead[train_inds,], predict.type = "response")

  # check the model predictions
  expect_factor(pred1, len = length(train_inds), any.missing = FALSE,
                levels = levels(classes[train_inds]))
  expect_factor(pred2, len = length(train_inds), any.missing = FALSE,
                levels = levels(classes[train_inds]))
  expect_factor(pred3, len = length(train_inds), any.missing = FALSE,
                levels = levels(classes[train_inds]))
  expect_false(all(pred2 == pred3))

})


test_that("classiKnn works with DTW distances", {
  requirePackages("dtw")
  # skip_if_not_installed("dtw")
  data("ArrowHead")
  ArrowHead = subset(ArrowHead, subset = c(TRUE, FALSE),
                     select = c(FALSE, FALSE, TRUE))
  classes = ArrowHead[,"target"]

  set.seed(123)
  train_inds = sample(1:nrow(ArrowHead), size = 0.8 * nrow(ArrowHead), replace = FALSE)
  test_inds = (1:nrow(ArrowHead))[!(1:nrow(ArrowHead)) %in% train_inds]

  ArrowHead = ArrowHead[,!colnames(ArrowHead) == "target"]

  mod1 = classiKnn(classes = classes[train_inds], fdata = ArrowHead[train_inds,],
                   metric = "DTW", window.type = "none")
  mod2 = classiKnn(classes = classes[train_inds], fdata = ArrowHead[train_inds,],
                   metric = "DTW", window.type = "sakoechiba",
                   dist.method = "Euclidean",
                   window.size = 0L)

  pred1 = predict(mod1, predict.type = "response")
  pred2 = predict(mod2, predict.type = "response")
  expect_factor(pred1, len = length(train_inds), any.missing = FALSE,
                levels = levels(classes[train_inds]))
  expect_factor(pred2, len = length(train_inds), any.missing = FALSE,
                levels = levels(classes[train_inds]))
})


test_that("classiKnn works with elastic distance from the square root velocity framework", {
  # skip_if_not(requirePackages("fdasrvf"), stop = FALSE)
  data("ArrowHead")
  ArrowHead = subset(ArrowHead, subset = c(TRUE, FALSE),
                     select = c(FALSE, FALSE, TRUE))
  classes = ArrowHead[,"target"]

  set.seed(123)
  train_inds = sample(1:nrow(ArrowHead), size = 0.8 * nrow(ArrowHead), replace = FALSE)
  test_inds = (1:nrow(ArrowHead))[!(1:nrow(ArrowHead)) %in% train_inds]

  ArrowHead = ArrowHead[,!colnames(ArrowHead) == "target"]

  mod1 = classiKnn(classes = classes[train_inds], fdata = ArrowHead[train_inds,],
                   metric = "elasticDistance", lambda = Inf)
  # mod2 = classiKnn(classes = classes[train_inds], fdata = ArrowHead[train_inds,],
  #                  metric = "elastic", a = 1, b = 2, lambda = Inf)

  pred1 = predict(mod1, newdata = ArrowHead[test_inds,],
                                 predict.type = "response")
  # pred2 = predict(mod2, newdata = ArrowHead[test_inds,], predict.type = "response")
  expect_factor(pred1, len = length(test_inds), any.missing = FALSE,
                levels = levels(classes[train_inds]))
  # expect_factor(pred2, len = length(test_inds), any.missing = FALSE,
  #               levels = levels(classes[train_inds]))
  # expect_equal(pred1, pred2)
})
