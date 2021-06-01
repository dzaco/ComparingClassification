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

cl_name = 'Species'                       #name of result class
X_train = remove_column(train, cl_name)   # data without result class
X_test = remove_column(test , cl_name)    # data without result class
Y_train = train[, cl_name]                # data of result class
Y_test = test[,cl_name]                   # data of result class

scoreboard <- data.frame(
 algorithm = I(c(NA,NA,NA,NA)),
 time = c(NA,NA,NA,NA),
 accuracy = c(NA,NA,NA,NA)
 )
algorithms <- c()
times <- c()
accuracies <- c()

#Decision Trees
time.start <- Sys.time()
preds.rpart = decision_trees(Species~., train, test, 'class')
CrossTable(test$Species, preds.rpart, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "decision_trees")
decision_trees_accuracy = ((16+20+12)/nrow(test))*100
decision_trees_accuracy
accuracies <- c(accuracies, decision_trees_accuracy)

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
nearest_neighbours_accuracy = ((16+19+13)/nrow(test))*100
nearest_neighbours_accuracy
accuracies <- c(accuracies, nearest_neighbours_accuracy)

#Support Vector Machine
time.start <- Sys.time()
preds.svm = support_vector_machine(Species~., train, test)
CrossTable(preds.svm,test$Species, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "support_vector_machine")
support_vector_machine_accuracy = ((16+20+13)/nrow(test))*100
support_vector_machine_accuracy
accuracies <- c(accuracies, support_vector_machine_accuracy)

#Random Forest
time.start <- Sys.time()
preds.rf = random_forest(Species~., train, test)
CrossTable(preds.rf, test$Species, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "random_forest")
random_forest_accuracy = ((16+19+13)/nrow(test))*100
random_forest_accuracy
accuracies <- c(accuracies, random_forest_accuracy)


scoreboard$algorithm <- algorithms
scoreboard$time <- times
scoreboard$accuracy <- accuracies


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
corr = sapply(imports85, function(x) as.numeric(x))
cr <- cor(corr)
corrplot(cr, method = "circle")

train = create_train_dataset(imports85, 100, 123)
test = create_test_dataset(imports85, 100, 123)

cl_name = 'symboling'                       # name of result class
X_train = remove_column(train, cl_name)     # data without result class
X_test = remove_column(test , cl_name)      # data without result class
Y_train = train[, cl_name]                  # data of result class
Y_test = test[,cl_name]                     # data of result class

scoreboard <- data.frame(
  algorithm = I(c(NA,NA,NA,NA)),
  time = c(NA,NA,NA,NA),
  accuracy = c(NA,NA,NA,NA)
)
algorithms <- c()
times <- c()
accuracies <- c()

#Decision Trees
time.start <- Sys.time()
preds.rpart = decision_trees(symboling~., train, test, 'vector')
CrossTable(test$symboling, preds.rpart, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
time <- Sys.time() - time.start
times <- c(times, time)
algorithms <- c(algorithms, "decision_trees")
decision_trees_accuracy = ((3+28+7+5+1+27)/nrow(test))*100
decision_trees_accuracy
accuracies <- c(accuracies, decision_trees_accuracy)




##################################################################
#adults
##################################################################

adult <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data', 
                    sep = ',', fill = F, strip.white = T)
colnames(adult) <- c('age', 'workclass', 'fnlwgt', 'education', 
                     'education_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex', 
                     'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')
summary(adult)

adult = clean(adult)
summary(adult)
plot(adult$income)

scoreboard <- data.frame(
  algorithm = I(c(NA,NA,NA,NA)),
  time = c(NA,NA,NA,NA),
  accuracy = c(NA,NA,NA,NA)
)
algorithms <- c()
times <- c()
accuracies <- c()

set_size <- round(.8 * dim(adult)[1])
train <- adult[1:set_size,]
test <- adult[-(1:set_size),]

#Decision Trees
time.start <- Sys.time()
preds.rpart = decision_trees(income~., train, test, 'class')
time <- Sys.time() - time.start
times <- c(times, time)
CrossTable(test$income, preds.rpart, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
algorithms <- c(algorithms, "decision_trees")
decision_trees_accuracy = ((4664+834)/nrow(test))*100
decision_trees_accuracy
accuracies <- c(accuracies, decision_trees_accuracy)

#Support Vector Machine
time.start <- Sys.time()
preds.svm = support_vector_machine(income~., train, test)
time <- Sys.time() - time.start
times <- c(times, time)
CrossTable(preds.svm, test$income, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
algorithms <- c(algorithms, "support_vector_machine")
support_vector_machine_accuracy = ((0)/nrow(test))*100
support_vector_machine_accuracy
accuracies <- c(accuracies, support_vector_machine_accuracy)

#Random Forest
time.start <- Sys.time()
preds.rf = random_forest(income~., train, test)
time <- Sys.time() - time.start
times <- c(times, time)
CrossTable(preds.rf, test$income, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
algorithms <- c(algorithms, "random_forest")
random_forest_accuracy = ((4611+1007)/nrow(test))*100
random_forest_accuracy
accuracies <- c(accuracies, random_forest_accuracy)

