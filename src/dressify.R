
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

train = create_train_dataset(dressify.corr, 200, 123)
test = create_test_dataset(dressify.corr, 200, 123)
train.target = train[,'Recommended']
test.target = test[,'Recommended']
train <- remove_column(train, 'Recommended')
test <- remove_column(test, 'Recommended')

#########
## KNN ##
#########

knn.4 <- knn(train,test, cl = train.target, k = 4)
acc.4 <- 100 * sum(test.target == knn.4) / NROW(test.target)

