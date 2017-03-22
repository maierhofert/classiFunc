# data("Yoga")
# classes = Yoga[,"target"]
#
# library("lineprof")
#
# set.seed(123)
# train_inds = sample(1:nrow(Yoga), size = 0.8 * nrow(Yoga), replace = FALSE)
# test_inds = (1:nrow(Yoga))[!(1:nrow(Yoga)) %in% train_inds]
#
# Yoga = Yoga[,!colnames(Yoga) == "target"]
#
# # No derivation
# l1 = lineprof({
# mod1 = classiKnn(classes = classes[train_inds], fdata = Yoga[train_inds,])
# })
# str(mod1)
# l1
#
# # fast derivation
# l2 = lineprof({
# mod2 = classiKnn(classes = classes[train_inds], fdata = Yoga[train_inds,],
#                  nderiv = 1L, knn = 3L)
# })
# str(mod2)
# l2
#
# # slow derivation
# l3 = lineprof({
# mod3 = classiKnn(classes = classes[train_inds], fdata = Yoga[train_inds,],
#                  nderiv = 1L, knn = 3L, fast.deriv = FALSE)
# })
# str(mod3)
# l3
#
# # Profiling the prediction
# lp1 = lineprof({
# pred1 = predict(mod1, newdata = Yoga[test_inds,], predict.type = "prob")
# })
# lp1
#
# lp2 = lineprof({
#   pred2 = predict(mod2, newdata = Yoga[test_inds,], predict.type = "response")
# })
# lp2
#
# lp3 = lineprof({
#   pred3 = predict(mod3, newdata = Yoga[test_inds,], predict.type = "response")
# })
# lp3
#
#
# #  They predict rather similar and similarly well
# mean(pred2 == pred3)
# mean(classes[test_inds] == pred2)
# mean(classes[test_inds] == pred3)








