source("src/CommonFunctions.R")
install_packages()

##################################################################
#Iris
##################################################################
data(iris)
summary(iris)

cr <- cor(iris[1:4])
corrplot(cr, method = "circle")

iris = clean(iris)
summary(iris)

train = create_train_dataset(iris, 100, 123)
test = create_test_dataset(iris, 100, 123)


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
decision_trees_accuracy = ((16+20+12)/nrow(test))*100
decision_trees_accuracy

#k-Nearest Neighbours
time.start <- Sys.time()
preds.knn = k_nearest_neighbours(train[,1:4], test[,1:4], train$Species)
CrossTable(preds.knn, test$Species, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time.end <- Sys.time()
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "k_nearest_neighbours")
nearest_neighbours_accuracy = ((16+19+13)/nrow(test))*100
nearest_neighbours_accuracy

#Support Vector Machine
time.start <- Sys.time()
preds.svm = support_vector_machine(Species~., train, test)
CrossTable(preds.svm,test$Species, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "support_vector_machine")
support_vector_machine_accuracy = ((16+20+13)/nrow(test))*100
support_vector_machine_accuracy

#Random Forest
time.start <- Sys.time()
preds.rf = random_forest(Species~., train, test)
CrossTable(preds.rf, test$Species, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "random_forest")
random_forest_accuracy = ((16+19+13)/nrow(test))*100
random_forest_accuracy


time.table$algorithm <- algorithms
time.table$time <- times


which(preds.rpart != preds.knn)
which(preds.rpart != preds.svm)
which(preds.rpart != preds.rf)
which(preds.knn != preds.svm)
which(preds.knn != preds.rf)
which(preds.svm != preds.rf)


##################################################################
#Imports-85
##################################################################

data("imports85")
summary(imports85)

imports85 = clean(imports85)
summary(imports85)

cr <- cor(imports85[1:26])
corrplot(cr, method = "circle")
