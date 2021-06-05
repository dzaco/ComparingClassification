source("src/CommonFunctions.R")
install_packages()

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

per70 <- floor(NROW(adult.corr) * 0.7)
train = create_train_dataset(adult.corr, per70, 123)
test = create_test_dataset(adult.corr, per70, 123)
train.target = train[,'income']
test.target = test[,'income']
train <- remove_column(train, 'income')
test <- remove_column(test, 'income')


##################################
## INIT VECTORS OF MEASUREMENTS ##
##################################
times <- c()
algorithms <- c()
accuracies <- c()

#########
## KNN ##
#########
algorithms <- c(algorithms, "k_nearest_neighbours")
time.start <- Sys.time()

k.acc <- accuracy_for_knn(train, test, train.target, test.target)
k.best <- best.k(k.acc)
knn.best <- knn(train, test, cl = train.target, k = k.best)

time <- difftime(Sys.time(), time.start, units = "secs")[[1]]
times <- c(times, time)

cross_table = CrossTable(knn.best, test.target, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
nearest_neighbours_accuracy = accuracy.cross_table(cross_table, test)
nearest_neighbours_accuracy
accuracies <- c(accuracies, nearest_neighbours_accuracy)


##################
## MEASUREMENTS ##
##################

scoreboard <- data.frame(algorithms, times, accuracies)





