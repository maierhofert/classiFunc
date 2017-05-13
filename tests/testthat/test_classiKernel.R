context("classiKernel")

test_that("classiKernel works for arrow head data set", {
  data("ArrowHead")
  classes = ArrowHead[, "target"]

  set.seed(123)
  train_inds = sample(1:nrow(ArrowHead), size = 0.8 * nrow(ArrowHead), replace = FALSE)
  test_inds = (1:nrow(ArrowHead))[!(1:nrow(ArrowHead)) %in% train_inds]

  ArrowHead = ArrowHead[, !colnames(ArrowHead) == "target"]

  mod1 = classiKernel(classes = classes[train_inds], fdata = ArrowHead[train_inds,])
  mod2 = classiKernel(classes = classes[train_inds], fdata = ArrowHead[train_inds,],
                      nderiv = 1L, h = 5)


  pred1 = predict(mod1, predict.type = "prob")
  checkmate::expect_matrix(pred1, any.missing = FALSE,
                           nrows = nrow(mod1$fdata),
                           ncols = length(levels(mod1$classes)))

  pred2 = predict(mod2, newdata = ArrowHead[train_inds,], predict.type = "response")
  checkmate::expect_factor(pred2, any.missing = FALSE, levels = levels(mod2$classes))

  pred3 = predict(mod2, newdata = ArrowHead[1,], predict.type = "response")
  checkmate::expect_factor(pred3, any.missing = FALSE, levels = levels(mod2$classes))
})


test_that("classiKernel works for different Kernels", {
  data(phoneme, package = "fda.usc")

  mod1 = classiKernel(classes = phoneme[["classlearn"]],
                      fdata = phoneme[["learn"]]$data,
                      ker = "Ker.norm", h = 50)
  mod2 = classiKernel(classes = phoneme[["classlearn"]],
                      fdata = phoneme[["learn"]]$data,
                      h = 0.1, ker = "Ker.epa")

  pred1 = predict(mod1, predict.type = "prob")
  checkmate::expect_matrix(pred1, any.missing = FALSE,
                           nrows = nrow(mod1$fdata),
                           ncols = length(unique(mod1$classes)))
  pred1.2 = predict(mod2, newdata = phoneme[["test"]]$dat, predict.type = "prob")

  pred2 = predict(mod2, newdata = phoneme[["test"]]$dat, predict.type = "response")
  checkmate::expect_factor(pred2, any.missing = FALSE,
                           levels = levels(phoneme[["classtest"]]))

})

test_that("classiKnn works for custom kernels", {
  data("ArrowHead")
  classes = ArrowHead[, "target"]

  set.seed(123)
  train_inds = sample(1:nrow(ArrowHead), size = 0.8 * nrow(ArrowHead),
                      replace = FALSE)
  test_inds = (1:nrow(ArrowHead))[!(1:nrow(ArrowHead)) %in% train_inds]

  ArrowHead = ArrowHead[,!colnames(ArrowHead) == "target"]


  # custom metric with additional parameters
  myUniformKernel = function(u) {
    return(0.5 * (abs(u) < 1))
  }

  # create the models
  mod1 = classiKernel(classes = classes[train_inds], fdata = ArrowHead[train_inds,],
                      ker = "custom.ker")
  mod2 = classiKernel(classes = classes[train_inds], fdata = ArrowHead[train_inds,],
                      ker = "custom.ker", custom.ker = myUniformKernel)
  mod3 = classiKernel(classes = classes[train_inds], fdata = ArrowHead[train_inds,],
                      ker = "Ker.unif")


  # get the model predictions
  pred1 = predict(mod1, newdata = ArrowHead[test_inds,], predict.type = "response")
  pred2 = predict(mod2, newdata = ArrowHead[test_inds,], predict.type = "response")
  pred3 = predict(mod3, newdata = ArrowHead[test_inds,], predict.type = "response")

  # check the model predictions
  expect_equal(pred2, pred3)

})
