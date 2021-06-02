#' Performs classification using decision tree algorithm
#' @param formula, train, test
#' @return preds.rpart
decision_trees <- function(formula, train, test, type) {
  library(rpart)
  model.rpart = rpart(formula, data = train)
  preds.rpart = predict(model.rpart, newdata = test, type = type)
  return(preds.rpart)
}

#' Performs classification using k-Nearest Neighbours algorithm
#' @param train, test, seed
#' @return preds.rpart
k_nearest_neighbours <- function(X_train, X_test, Y_train, Y_test, try_number = 100, seed = 123) {
  library(class)
  set.seed(seed)
  
  acc <- accuracy_for_knn(X_train, X_test, Y_train, Y_test, try_number)
  plot_accurency(acc)
  best.k = best.k(acc)
  print(paste("knn with k =",best.k))
  
  preds.knn = knn(train = scale(sapply(X_train, function(x) as.numeric(x))),
                 test = scale(sapply(X_test, function(x) as.numeric(x))),
                 cl = Y_train, 
                 k = best.k)
  return(preds.knn)
}

#' calculate accuracy of knn algorithm for k from 1 to try_number
#' @return list of accuracy
accuracy_for_knn = function(train, test, train.target, test.target, try_number = 50) {
  i = 1
  k.optm = 1
  for(i in 1:try_number ) {
    knn.pred <- knn(train, test, cl = train.target, k = i)
    k.optm[i] <- 100 * sum(test.target == knn.pred) / NROW(test.target)
  }
  plot_accurency(k.optm)
  return(k.optm)
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

sum_diag <- function(cross_table) {
  return( sum(diag(cross_table[["t"]])) ) 
}
accuracy.cross_table <- function(cross_table, data) {
  return( sum_diag(cross_table) /nrow(data)*100 )
}
