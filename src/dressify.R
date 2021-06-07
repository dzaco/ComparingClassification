# source("src/CommonFunctions.R")
# install_packages()

##################################################################
#dressify
##################################################################
dressify <- read.csv(file = 'datasets/train_dress.csv')
summary(dressify)


##################################################################
# CLEAN
##################################################################

# czyszczenie - uzupelnianie NA
dressify = remove_column(dressify, 'ID')
dressify = clean(dressify)
summary(dressify)

#Łączenie poziomów
dressify$Price[which(dressify$Price =="low")] <- "Low"
dressify$Price[which(dressify$Price =="high")] <- "High"
dressify$Price[which(dressify$Price =="Average")] <- "Medium"
# dressify$Price[which(dressify$Price =="Length")] <- NA
dressify$Price = as.factor(as.character(dressify$Price))

dressify$Size[which(dressify$Size =="small")] <- "S"
dressify$Size = as.factor(as.character(dressify$Size))

dressify$Season[which(dressify$Season =="winter")] <- "Winter"
dressify$Season[which(dressify$Season =="Automn")] <- "Autumn"
dressify$Season[which(dressify$Season =="spring")] <- "Spring"
dressify$Season[which(dressify$Season =="summer")] <- "Summer"
# dressify$Season[which(dressify$Season =="Length")] <- NA
dressify$Season = as.factor(as.character(dressify$Season))

dressify$SleeveLength[which(dressify$SleeveLength =="thressqatar")] <- "threequarter"
dressify$SleeveLength[which(dressify$SleeveLength =="sleevless")] <- "sleeveless"
dressify$SleeveLength = as.factor(as.character(dressify$SleeveLength))

dressify$FabricType[which(dressify$FabricType =="shiffon")] <- "chiffon"
dressify$FabricType = as.factor(as.character(dressify$FabricType))

summary(dressify)


table(dressify$Recommended, useNA = "ifany")

#################
## correlation ##
#################

dressify.corr <- sapply(dressify, function(x) as.numeric(x))
corr <- cor(dressify.corr)
corrplot(corr, method = "circle")
corr[,which(colnames(dressify) == "Recommended")]

###################
## SPLIT DATA ##
###################

train = create_train_dataset(dressify, 200, 123)
test = create_test_dataset(dressify, 200, 123)
train.target = train[,'Recommended']
test.target = test[,'Recommended']
# train <- remove_column(train, 'Recommended')
# test <- remove_column(test, 'Recommended')

train.knn = sapply(train, function(x) as.numeric(x))
test.knn = sapply(train, function(x) as.numeric(x))
train.knn.target = train.knn[,'Recommended']
test.knn.target = test.knn[,'Recommended']
train.knn <- remove_column(train.knn, 'Recommended')
test.knn <- remove_column(test.knn, 'Recommended')



#########
## KNN ##
#########

algorithm_name <- "k_nearest_neighbours"
time.start <- Sys.time()

k.acc <- accuracy_for_knn(train.knn, test.knn, train.knn.target, test.knn.target)
k.best <- best.k(k.acc)
knn.best <- knn(train.knn, test.knn, cl = train.knn.target, k = k.best)

time <- difftime(Sys.time(), time.start, units = "secs")[[1]]

cross_table = CrossTable(knn.best, test.knn.target, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
nearest_neighbours_accuracy = accuracy.cross_table(cross_table, test.knn)
nearest_neighbours_accuracy

scoreboard[nrow(scoreboard) + 1,] = c(algorithm_name, time, nearest_neighbours_accuracy, "dressify")



###################
## DECISION TREE ##
###################

algorithm_name <- "decision_trees"
time.start <- Sys.time()

preds.rpart = decision_trees(Recommended~., train, test, 'class')
time <- Sys.time() - time.start

cross_table = CrossTable(test$Recommended, preds.rpart, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
decision_trees_accuracy = accuracy.cross_table(cross_table, test)
decision_trees_accuracy

scoreboard[nrow(scoreboard) + 1,] = c(algorithm_name, time, decision_trees_accuracy, "dressify")



############################
## SUPPORT VECTOR MACHINE ##
############################

algorithm_name <- "support_vector_machine"
time.start <- Sys.time()

preds.svm = support_vector_machine(Recommended~., train, test)
time <- Sys.time() - time.start

cross_table = CrossTable(test$Recommended, preds.svm, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
support_vector_machine_accuracy = accuracy.cross_table(cross_table, test)
support_vector_machine_accuracy

scoreboard[nrow(scoreboard) + 1,] = c(algorithm_name, time, support_vector_machine_accuracy, "dressify")


###################
## RANDOM FOREST ##
###################

algorithm_name <- "random_forest"
time.start <- Sys.time()

preds.rf = random_forest(Recommended~., train, test)
time <- Sys.time() - time.start

cross_table = CrossTable(test$Recommended, preds.rf, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
random_forest_accuracy = accuracy.cross_table(cross_table, test)
random_forest_accuracy

scoreboard[nrow(scoreboard) + 1,] = c(algorithm_name, time, random_forest_accuracy, "dressify")


