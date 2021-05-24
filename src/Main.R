install_packages()

library(titanic)

data <- titanic_train
data <- clean(data)
summary(data)


data(iris)
summary(iris)

train = create_train_dataset(iris, 100, 123)
test = create_test_dataset(iris, 100, 123)

#Decision Trees
preds.rpart = decision_trees(Species~., train, test)
CrossTable(test$Species, preds.rpart, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)

#k-Nearest Neighbours
preds.knn = k_nearest_neighbours(train[,1:4], test[,1:4], train$Species)
CrossTable(preds.knn, test$Species, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)

#Support Vector Machine
preds.svm = support_vector_machine(Species~., train, test)
CrossTable(preds.svm,test$Species, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)

#Random Forest
preds.rf = random_forest(Species~., train, test)
CrossTable(preds.rf, test$Species, chisq = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)

which(preds.rpart != preds.knn)
which(preds.rpart != preds.svm)
which(preds.rpart != preds.rf)
which(preds.knn != preds.svm)
which(preds.knn != preds.rf)
which(preds.svm != preds.rf)
