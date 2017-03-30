# # install.packages("fdasrvf")
# library("fdasrvf")
#
# data("Phoneme")
# # Phoneme = subset(Phoneme, subset = c(TRUE, FALSE),
# #                    select = c(FALSE, FALSE, TRUE))
# classes = Phoneme[,"target"]
#
# set.seed(123)
# train_inds = sample(1:nrow(Phoneme), size = 0.8 * nrow(Phoneme), replace = FALSE)
# test_inds = (1:nrow(Phoneme))[!(1:nrow(Phoneme)) %in% train_inds]
#
# Phoneme = Phoneme[,!colnames(Phoneme) == "target"]
#
# data("ArrowHead")
# classes = ArrowHead[,"target"]
#
# set.seed(123)
# train_inds = sample(1:nrow(ArrowHead), size = 0.8 * nrow(ArrowHead), replace = FALSE)
# test_inds = (1:nrow(ArrowHead))[!(1:nrow(ArrowHead)) %in% train_inds]
#
# ArrowHead = ArrowHead[,!colnames(ArrowHead) == "target"]
#
#
#
# dat = Phoneme
# dat = t(as.matrix(dat))
# str(dat)
#
# mybeta = array(NA, dim = c(2, nrow(dat), ncol(dat)))
# # set the data
# mybeta[1,,] = dat
# # set the observation grid
# mybeta[2,,] = matrix(1:nrow(dat),
#                      ncol = ncol(dat),
#                      nrow = nrow(dat))
#
# str(mybeta)
# plot(x = mybeta[2,,1:4], y = mybeta[1,,1:4], type = "l")
#
# plot(mybeta[1,,1], type = "l")
# plot(mybeta[2,,1])
#
# plot(mybeta[1,,2], type = "l")
# plot(mybeta[2,,2])
#
# plot(mybeta[1,,3], type = "l")
# plot(mybeta[2,,3])
#
# plot(mybeta[1,,4], type = "l")
# plot(mybeta[2,,4])
#
# myout = curve_srvf_align(mybeta[,,1:4], maxit = 20, mode = "O")
#
# # plot grid against observations,
# # it looks weird, as if it would be in mode = "C"
# plot(x = mybeta[2,,1:4], y = myout$qn[1,,1:4], type = "l")
# plot(x = mybeta[2,,1:4], y = myout$betan[1,,1:4], type = "l")
#
#
# plot(myout$qn[1,,1], type = "l")
# plot(myout$qn[2,,1], type = "l")
# plot(myout$qn[2,,1], myout$qn[1,,1])
#
#
# plot(myout$qn[1,,2], type = "l")
# plot(myout$qn[2,,2])
# plot(myout$qn[2,,2], myout$qn[1,,2])
#
# plot(myout$qn[1,,3], type = "l")
# plot(myout$qn[2,,3])
# plot(myout$qn[2,,3], myout$qn[1,,3])
#
# plot(myout$qn[1,,4], type = "l")
# plot(myout$qn[2,,4])
# plot(myout$qn[2,,4], myout$qn[1,,3])
#
#
# data("mpeg7")
# out = curve_srvf_align(beta[,,1,1:3], maxit=3, mode = "O")
# str(out)
#
#
# K = curve_karcher_cov(out$betamean, beta[,,1,1:2])
#
#
#
# # elastic distance
# # = distance of square root velocity of the functions
# # Hyperparameters are
# # c = b / (2 * a) = 1 in R+
# # b = 1 in R+
# # a = 1 / 2 in R+
# #
# # lambda: parameter that allows warping in R
#
# proxy::dist(Phoneme[1,],
#             Phoneme[2,])
#
# x = y = Phoneme[1:10,]
# debugonce(elastic.distance)
# as.matrix(proxy::dist(x, y, method = el.dist, a = 0, b = 1, lambda = -0.1))
#
# a = 1 / 2
# b = 1
#
# srv.dist = (a) ^ 2 * el.dist$Dy + b ^ 2 * el.dist$Dx
#
