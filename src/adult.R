# source("src/CommonFunctions.R")
# install_packages()

##################################################################
#adults
##################################################################


##################
## PREPARE DATA ##
##################

adult <- read.csv(file = 'datasets/adult.csv')
summary(adult)

adult$workclass[which(adult$workclass =="?")] <- NA
adult$native.country [which(adult$native.country  =="?")] <- NA
adult$occupation [which(adult$occupation  =="?")] <- NA
adult = clean(adult)
summary(adult)

plot(adult$income)
hist(adult$age)
table(adult$income, useNA = "ifany")

adult.corr <- sapply(adult, function(x) as.numeric(x))
corr <- cor(adult.corr)
corrplot(corr, method = "circle")
corr[ , which(colnames(adult) == "income")]


####################
## NORMALIZE DATA ##
####################
# czy trzeba normalizowac ?
#adult.corr.norm <- normalize(adult.corr)

################
## SPLIT DATA ##
################

per70 <- floor(NROW(adult) * 0.7)
train = create_train_dataset(adult, per70, 123)
test = create_test_dataset(adult, per70, 123)
train.target = train[,'income']
test.target = test[,'income']
# train <- remove_column(train, 'income')
# test <- remove_column(test, 'income')

train.knn = sapply(train, function(x) as.numeric(x))
test.knn = sapply(test, function(x) as.numeric(x))
train.knn.target = train.knn[,'income']
test.knn.target = test.knn[,'income']
train.knn <- remove_column(train.knn, 'income')
test.knn <- remove_column(test.knn, 'income')


#########
## KNN ##
#########
algorithm_name <- "k_nearest_neighbours"
time.start <- Sys.time()

k.acc <- accuracy_for_knn(train.knn, test.knn, train.knn.target, test.knn.target)
k.best <- best.k(k.acc)
knn.best <- knn(train.knn, test.knn, cl = train.knn.target, k = k.best)
knn.best

time <- difftime(Sys.time(), time.start, units = "secs")[[1]]

cross_table = CrossTable(knn.best, test.knn.target, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
nearest_neighbours_accuracy = accuracy.cross_table(cross_table, test.knn)
nearest_neighbours_accuracy

scoreboard[nrow(scoreboard) + 1,] = c(algorithm_name, time, nearest_neighbours_accuracy, "adult")


###################
## DECISION TREE ##
###################

algorithm_name <- "decision_trees"
time.start <- Sys.time()

preds.rpart = decision_trees(income~., train, test, 'class')
time <- difftime(Sys.time(), time.start, units = "secs")[[1]]

cross_table = CrossTable(test$income, preds.rpart, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
decision_trees_accuracy = accuracy.cross_table(cross_table, test)
decision_trees_accuracy

scoreboard[nrow(scoreboard) + 1,] = c(algorithm_name, time, decision_trees_accuracy, "adult")



############################
## SUPPORT VECTOR MACHINE ##
############################

algorithm_name <- "support_vector_machine"
time.start <- Sys.time()

preds.svm = support_vector_machine(income~., train, test)
time <- difftime(Sys.time(), time.start, units = "secs")[[1]]

cross_table = CrossTable(test$income, preds.svm, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
support_vector_machine_accuracy = accuracy.cross_table(cross_table, test)
support_vector_machine_accuracy

scoreboard[nrow(scoreboard) + 1,] = c(algorithm_name, time, support_vector_machine_accuracy, "adult")


###################
## RANDOM FOREST ##
###################

algorithm_NAME <- "random_forest"
time.start <- Sys.time()

preds.rf = random_forest(income~., train, test)
time <- difftime(Sys.time(), time.start, units = "secs")[[1]]

cross_table = CrossTable(test$income, preds.rf, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
random_forest_accuracy = accuracy.cross_table(cross_table, test)
random_forest_accuracy

scoreboard[nrow(scoreboard) + 1,] = c(algorithm_name, time, random_forest_accuracy, "adult")

