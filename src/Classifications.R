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
k_nearest_neighbours <- function(train, test, cl, k = 3, seed = 123) {
  library(class)
  set.seed(seed)
  preds.knn = knn(train, test, cl, k=3)
  return(preds.knn)
}

#' calculate accuracy of knn algorithm for k from 1 to try_number
#' @return list of accuracy
accuracy_for_knn = function(try_number , X_train, X_test, Y_train, Y_test) {
  set.seed(2)
  k_to_try = 1:try_number
  acc_k = rep(x = 0, times = length(k_to_try))
  
  for(i in seq_along(k_to_try)) {
    pred = knn(train = scale(X_train), 
               test = scale(X_test), 
               cl = Y_train, 
               k = k_to_try[i])
    acc_k[i] = accuracy(Y_test, pred)
  }
  
  return(acc_k)
}

#' @return best k from accuracy list (max)
best.k = function(accuracy_k) {
  res <- which(accuracy_k == max(accuracy_k))
  return(res[1])
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