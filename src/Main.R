source("src/CommonFunctions.R")
install_packages()

##################################################################
#Iris
##################################################################
data(iris)
summary(iris)

iris = clean(iris)
summary(iris)

cr <- cor(iris[1:4])
corrplot(cr, method = "circle")


train = create_train_dataset(iris, 100, 123)
test = create_test_dataset(iris, 100, 123)

cl_name = 'Species'                       # name of result class
X_train = remove_column(train, cl_name)   # data without result class
X_test = remove_column(test , cl_name)    # data without result class
Y_train = train[, cl_name]                # data of result class
Y_test = test[,cl_name]                   # data of result class


algorithms <- c()
times <- c()
accuracies <- c()


#Decision Trees
time.start <- Sys.time()
preds.rpart = decision_trees(Species~., train, test, 'class')
cross_table = CrossTable(test$Species, preds.rpart, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)

time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "decision_trees")
decision_trees_accuracy = accuracy.cross_table(cross_table, test)
decision_trees_accuracy
accuracies <- c(accuracies, decision_trees_accuracy)


#k-Nearest Neighbours
time.start <- Sys.time()
# calculate accuracy for knn - to find best k
# accuracy <- accuracy_for_knn(try_number = 50, X_train, X_test, Y_train, Y_test)
# plot_accurency(accuracy)
# best_k = best.k(accuracy)
# print(paste("knn with k =",best_k))

preds.knn = k_nearest_neighbours(X_train, X_test, Y_train, Y_test)
cross_table = CrossTable(preds.knn, test$Species, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time.end <- Sys.time()
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "k_nearest_neighbours")
nearest_neighbours_accuracy = accuracy.cross_table(cross_table, test)
nearest_neighbours_accuracy
accuracies <- c(accuracies, nearest_neighbours_accuracy)


#Support Vector Machine
time.start <- Sys.time()
preds.svm = support_vector_machine(Species~., train, test)
cross_table = CrossTable(preds.svm,test$Species, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "support_vector_machine")
support_vector_machine_accuracy = accuracy.cross_table(cross_table, test)
support_vector_machine_accuracy
accuracies <- c(accuracies, support_vector_machine_accuracy)


#Random Forest
time.start <- Sys.time()
preds.rf = random_forest(Species~., train, test)
cross_table = CrossTable(preds.rf, test$Species, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "random_forest")
random_forest_accuracy = accuracy.cross_table(cross_table, test)
random_forest_accuracy
accuracies <- c(accuracies, random_forest_accuracy)


scoreboard <- data.frame(algorithms, times, accuracies)

which(preds.rpart != preds.knn)
which(preds.rpart != preds.svm)
which(preds.rpart != preds.rf)
which(preds.knn != preds.svm)
which(preds.knn != preds.rf)
which(preds.svm != preds.rf)


##################################################################
#Imports-85
##################################################################

algorithms <- c()
times <- c()
accuracies <- c()

data("imports85")
summary(imports85)

imports85 = clean(imports85)
summary(imports85)

corr = sapply(imports85, function(x) as.numeric(x))
cr <- cor(corr)
corrplot(cr, method = "circle")

train = create_train_dataset(imports85, 100, 123)
test = create_test_dataset(imports85, 100, 123)

cl_name = 'symboling'                       # name of result class
X_train = train[, c('numOfDoors', 'bodyStyle', 'wheelBase','height')]
X_test = test[, c('numOfDoors', 'bodyStyle', 'wheelBase','height')]
Y_train = train[, cl_name]                  # data of result class
Y_test = test[,cl_name]                     # data of result class


#Decision Trees
time.start <- Sys.time()
preds.rpart = decision_trees(symboling~., train, test, 'vector')
cross_table = CrossTable(test$symboling, preds.rpart, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "decision_trees")
decision_trees_accuracy = accuracy.cross_table(cross_table, test)
decision_trees_accuracy
accuracies <- c(accuracies, decision_trees_accuracy)


#k-Nearest Neighbours
time.start <- Sys.time()
preds.knn = k_nearest_neighbours(X_train, X_test, Y_train, Y_test)
cross_table = CrossTable(preds.knn, Y_test, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time.end <- Sys.time()
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "k_nearest_neighbours")
nearest_neighbours_accuracy = accuracy.cross_table(cross_table, test)
nearest_neighbours_accuracy
accuracies <- c(accuracies, nearest_neighbours_accuracy)


#Support Vector Machine
time.start <- Sys.time()
preds.svm = support_vector_machine(symboling~., train, test)
cross_table = CrossTable(preds.svm,test$symboling, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "support_vector_machine")
support_vector_machine_accuracy = accuracy.cross_table(cross_table, test)
support_vector_machine_accuracy
accuracies <- c(accuracies, support_vector_machine_accuracy)


#Random Forest
time.start <- Sys.time()
preds.rf = random_forest(symboling~., train, test)
cross_table = CrossTable(preds.rf, test$symboling, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "random_forest")
random_forest_accuracy = accuracy.cross_table(cross_table, test)
random_forest_accuracy
accuracies <- c(accuracies, random_forest_accuracy)


scoreboard <- data.frame(algorithms, times, accuracies)


##################################################################
#adults
##################################################################

adult <- read.csv(file = 'datasets/adult.csv')
summary(adult)

adult = clean(adult)
summary(adult)
plot(adult$income)

algorithms <- c()
times <- c()
accuracies <- c()

set_size <- round(.8 * dim(adult)[1])
train <- adult[1:set_size,]
test <- adult[-(1:set_size),]

cl_name = 'income'                        # name of result class
X_train = remove_column(adult, cl_name)   # data without result class
X_test = remove_column(adult , cl_name)   # data without result class
Y_train = adult[, cl_name]                # data of result class
Y_test = adult[,cl_name]                  # data of result class


#Decision Trees
time.start <- Sys.time()
preds.rpart = decision_trees(income~., train, test, 'class')
time <- Sys.time() - time.start
times <- c(times, time)
cross_table = CrossTable(test$income, preds.rpart, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
algorithms <- c(algorithms, "decision_trees")
decision_trees_accuracy = accuracy.cross_table(cross_table, test)
decision_trees_accuracy
accuracies <- c(accuracies, decision_trees_accuracy)


#k-Nearest Neighbours
time.start <- Sys.time()
preds.knn = k_nearest_neighbours(X_train, X_test, Y_train, Y_test)
cross_table = CrossTable(preds.knn, Y_test, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time.end <- Sys.time()
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "k_nearest_neighbours")
nearest_neighbours_accuracy = accuracy.cross_table(cross_table, test)
nearest_neighbours_accuracy
accuracies <- c(accuracies, nearest_neighbours_accuracy)


#Support Vector Machine
time.start <- Sys.time()
preds.svm = support_vector_machine(income~., train, test)
time <- Sys.time() - time.start
times <- c(times, time)
cross_table = CrossTable(preds.svm, test$income, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
algorithms <- c(algorithms, "support_vector_machine")
support_vector_machine_accuracy = accuracy.cross_table(cross_table, test)
support_vector_machine_accuracy
accuracies <- c(accuracies, support_vector_machine_accuracy)


#Random Forest
time.start <- Sys.time()
preds.rf = random_forest(income~., train, test)
time <- Sys.time() - time.start
times <- c(times, time)
cross_table = CrossTable(preds.rf, test$income, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
algorithms <- c(algorithms, "random_forest")
random_forest_accuracy = accuracy.cross_table(cross_table, test)
random_forest_accuracy
accuracies <- c(accuracies, random_forest_accuracy)


scoreboard <- data.frame(algorithms, times, accuracies)


##################################################################
#dressify
##################################################################

dressify <- read.csv(file = 'datasets/train_dress.csv')
summary(dressify)

dressify = clean(dressify)
summary(dressify)

corr = sapply(dressify, function(x) as.numeric(x))
cr <- cor(corr)
corrplot(cr, method = "circle")

algorithms <- c()
times <- c()
accuracies <- c()

train = create_train_dataset(dressify, 100, 123)
test = create_test_dataset(dressify, 100, 123)

cl_name = 'Recommended'                       # name of result class
X_train = train[, c('numOfDoors', 'bodyStyle', 'wheelBase','height')]
X_test = test[, c('numOfDoors', 'bodyStyle', 'wheelBase','height')]
Y_train = train[, cl_name]                  # data of result class
Y_test = test[,cl_name]                     # data of result class


#Decision Trees
time.start <- Sys.time()
preds.rpart = decision_trees(Recommended~., train, test, 'vector')
cross_table = CrossTable(test$Recommended, preds.rpart, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "decision_trees")
decision_trees_accuracy = accuracy.cross_table(cross_table, test)
decision_trees_accuracy
accuracies <- c(accuracies, decision_trees_accuracy)


#k-Nearest Neighbours
time.start <- Sys.time()
preds.knn = k_nearest_neighbours(X_train, X_test, Y_train, Y_test)
cross_table = CrossTable(preds.knn, Y_test, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time.end <- Sys.time()
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "k_nearest_neighbours")
nearest_neighbours_accuracy = accuracy.cross_table(cross_table, test)
nearest_neighbours_accuracy
accuracies <- c(accuracies, nearest_neighbours_accuracy)


#Support Vector Machine
time.start <- Sys.time()
preds.svm = support_vector_machine(Recommended~., train, test)
cross_table = CrossTable(preds.svm,test$Recommended, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "support_vector_machine")
support_vector_machine_accuracy = accuracy.cross_table(cross_table, test)
support_vector_machine_accuracy
accuracies <- c(accuracies, support_vector_machine_accuracy)


#Random Forest
time.start <- Sys.time()
preds.rf = random_forest(Recommended~., train, test)
cross_table = CrossTable(preds.rf, test$Recommended, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "random_forest")
random_forest_accuracy = accuracy.cross_table(cross_table, test)
random_forest_accuracy
accuracies <- c(accuracies, random_forest_accuracy)


scoreboard <- data.frame(algorithms, times, accuracies)