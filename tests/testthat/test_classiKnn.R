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
