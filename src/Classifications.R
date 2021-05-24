#' Performs classification using decision tree algorithm
#' @param formula, train, test
#' @return preds.rpart
decision_trees <- function(formula, train, test) {
  library(rpart)
  model.rpart = rpart(formula, data = train)
  preds.rpart = predict(model.rpart, newdata = test, type = "class")
  return(preds.rpart)
}

#' Performs classification using k-Nearest Neighbours algorithm
#' @param train, test, seed
#' @return preds.rpart
k_nearest_neighbours <- function(train, test, cl, seed = 123) {
  library(class)
  set.seed(seed)
  preds.knn = knn(train, test, cl, k=3)
  return(preds.knn)
}

#' Performs classification using Support Vector Machine algorithm
#' @param formula, train_data, test_data
#' @return preds.rpart
support_vector_machine <- function(formula, train, test) {
  model.svm = svm(formula, data = train)
  preds.svm = predict(model.svm, newdata = test)
  return(preds.svm)
}

#' Performs classification using Random Forest algorithm
#' @param formula, train_data, test_data
#' @return preds.rpart
random_forest <- function(formula, train, test, seed = 123) {
  set.seed(seed)
  model.rf = randomForest(formula, data = train)
  preds.rf = predict(model.rf, newdata = test)
  return(preds.rf)
}