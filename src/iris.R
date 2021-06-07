##################################################################
#Iris
##################################################################
data(iris)
summary(iris)

##################
## PREPARE DATA ##
##################

iris = clean(iris)
summary(iris)

cr <- cor(iris[1:4])
corrplot(cr, method = "circle")

################
## SPLIT DATA ##
################

train = create_train_dataset(iris, 100, 123)
test = create_test_dataset(iris, 100, 123)

cl_name = 'Species'                       # name of result class
X_train = remove_column(train, cl_name)   # data without result class
X_test = remove_column(test , cl_name)    # data without result class
Y_train = train[, cl_name]                # data of result class
Y_test = test[,cl_name]                   # data of result class

###################
## DECISION TREE ##
###################

algorithm_name <- "decision_trees"
time.start <- Sys.time()

preds.rpart = decision_trees(Species~., train, test, 'class')
cross_table = CrossTable(test$Species, preds.rpart, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)

time <- Sys.time() - time.start

decision_trees_accuracy = accuracy.cross_table(cross_table, test)
decision_trees_accuracy

scoreboard[nrow(scoreboard) + 1,] = c(algorithm_name, time, decision_trees_accuracy, "iris")


#########
## KNN ##
#########

algorithm_name <- "k_nearest_neighbours"
time.start <- Sys.time()
# calculate accuracy for knn - to find best k
# accuracy <- accuracy_for_knn(try_number = 50, X_train, X_test, Y_train, Y_test)
# plot_accurency(accuracy)
# best_k = best.k(accuracy)
# print(paste("knn with k =",best_k))

preds.knn = k_nearest_neighbours(X_train, X_test, Y_train, Y_test)
time.end <- Sys.time()
time <- Sys.time() - time.start
cross_table = CrossTable(preds.knn, test$Species, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)

nearest_neighbours_accuracy = accuracy.cross_table(cross_table, test)
nearest_neighbours_accuracy

scoreboard[nrow(scoreboard) + 1,] = c(algorithm_name, time, nearest_neighbours_accuracy, "iris")

############################
## SUPPORT VECTOR MACHINE ##
############################
algorithm_name <- "support_vector_machine"
time.start <- Sys.time()

preds.svm = support_vector_machine(Species~., train, test)
time <- Sys.time() - time.start

cross_table = CrossTable(test$Species, preds.svm, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
support_vector_machine_accuracy = accuracy.cross_table(cross_table, test)
support_vector_machine_accuracy

scoreboard[nrow(scoreboard) + 1,] = c(algorithm_name, time, support_vector_machine_accuracy, "iris")


###################
## RANDOM FOREST ##
###################

algorithm_name <- "random_forest"
time.start <- Sys.time()

preds.rf = random_forest(Species~., train, test)
time <- Sys.time() - time.start

cross_table = CrossTable(test$Species, preds.rf, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
random_forest_accuracy = accuracy.cross_table(cross_table, test)
random_forest_accuracy

scoreboard[nrow(scoreboard) + 1,] = c(algorithm_name, time, random_forest_accuracy, "iris")


which(preds.rpart != preds.knn)
which(preds.rpart != preds.svm)
which(preds.rpart != preds.rf)
which(preds.knn != preds.svm)
which(preds.knn != preds.rf)
which(preds.svm != preds.rf)

