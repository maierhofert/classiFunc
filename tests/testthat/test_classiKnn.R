context("classiKnn")

test_that("classiKnn works", {
  data(Phoneme)
  classes = Phoneme[,"target"]

  set.seed(123)
  train_inds = sample(1:nrow(Phoneme), size = 0.01 * nrow(Phoneme), replace = FALSE)
  test_inds = sample(1:nrow(Phoneme), size = 0.01 * nrow(Phoneme), replace = FALSE)

  Phoneme = Phoneme[,!colnames(Phoneme) == "target"]
  Phoneme = Phoneme[, c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)]

  mod1 = classiKnn(classes = classes[train_inds], fdata = Phoneme[train_inds,])
  mod2 = classiKnn(classes = classes[train_inds], fdata = Phoneme[train_inds,],
                     nderiv = 1L, knn = 3L)


  pred1 = predict(mod1, predict.type = "prob")
  checkmate::expect_matrix(pred1, any.missing = FALSE,
                nrows = nrow(mod1$fdata),
                ncols = length(levels(mod1$classes)))

  pred2 = predict(mod2, newdata = Phoneme[train_inds,], predict.type = "response")
  checkmate::expect_factor(pred2, any.missing = FALSE, levels = levels(mod2$classes))
})


test_that("classiKnn works", {
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
