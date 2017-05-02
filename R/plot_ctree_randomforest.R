# this file creates figures for classification trees and random forest
library("mlr")

# classification tree
tree = train(learner = "classif.ctree", task = bc.task)
tree$learner.model
