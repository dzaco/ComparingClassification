library(class)

data(imports85)
cars <- imports85
###################
## 1. CLEAN DATA ##
###################

# remove nonnumeric data
nonnumeric_names <- c('symboling', 'make', 'fuelType', 'aspiration', 
                      'numOfDoors', 'bodyStyle', 'driveWheels', 'engineLocation', 
                      'numOfCylinders', 'fuelSystem', 'engineType', 'engineSize')
cars <- remove_column(cars, nonnumeric_names)
head(cars)
cars[cars == "?"] <- NA

typeofcols <- sapply(cars, class)
cars <- as.data.frame(apply(cars, 2, as.numeric)) # convert all col to numeric

colMeans(is.na(cars)) * 100
# 20% danych w kolumnie normalizedLosses jest NA - to za duzo - unuwamy
cars <- remove_column(cars, 'normalizedLosses')
cars <- clean(cars)

#######################
## 2. NORMALIZE DATA ##
#######################

price <- cars$price
cars <- as.data.frame( apply(cars, 2, min_max_scale))
cars$price <- price

###################
## 3. SPLIT DATA ##
###################

cr <- cor(cars)
corrplot(cr, method = "circle")

train = create_train_dataset(cars, 100, 123)
test = create_test_dataset(cars, 100, 123)
train.target = train$price
test.target = test$price
train <- remove_column(train, 'price')
test <- remove_column(test, 'price')


#########
## KNN ##
#########

acc <- accuracy_for_knn(train, test, train.target, test.target)
plot_accurency(acc)
k <- best.k(acc)

