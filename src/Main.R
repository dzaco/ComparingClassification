source("src/CommonFunctions.R")
install_packages()

data(iris)
summary(iris)

train = create_train_dataset(iris, 100, 123)
test = create_test_dataset(iris, 100, 123)

cl_name = 'Species'                       #name of result class
X_train = remove_column(train, cl_name)   # data without result class
X_test = remove_column(test , cl_name)    # data without result class
Y_train = train[, cl_name]                # data of result class
Y_test = test[,cl_name]                   # data of result class

time.table <- data.frame(
 algorithm = I(c(NA,NA,NA,NA)),
 time = c(NA,NA,NA,NA)
 )
algorithms <- c()
times <- c()

#Decision Trees
time.start <- Sys.time()
preds.rpart = decision_trees(Species~., train, test)
CrossTable(test$Species, preds.rpart, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "decision_trees")

#k-Nearest Neighbours
time.start <- Sys.time()
# calculate accuracy for knn - to find best k
accuracy <- accuracy_for_knn(try_number = 50, X_train, X_test, Y_train, Y_test)
plot_accurency(accuracy)
best_k = best.k(accuracy)
print(paste("knn with k =",best_k))

preds.knn = k_nearest_neighbours(X_train, X_test, cl = train$Species, k = best_k)
CrossTable(preds.knn, test$Species, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time.end <- Sys.time()
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "k_nearest_neighbours")

#Support Vector Machine
time.start <- Sys.time()
preds.svm = support_vector_machine(Species~., train, test)
CrossTable(preds.svm,test$Species, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "support_vector_machine")

#Random Forest
time.start <- Sys.time()
preds.rf = random_forest(Species~., train, test)
CrossTable(preds.rf, test$Species, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "random_forest")

time.table$algorithm <- algorithms
time.table$time <- times


which(preds.rpart != preds.knn)
which(preds.rpart != preds.svm)
which(preds.rpart != preds.rf)
which(preds.knn != preds.svm)
which(preds.knn != preds.rf)
which(preds.svm != preds.rf)
