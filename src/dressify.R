
##################################################################
#dressify
##################################################################
dressify <- read.csv(file = 'datasets/train_dress.csv')
summary(dressify)


##################################################################
# CLEAN
##################################################################
dressify = clean(dressify)
summary(dressify)

corr = sapply(dressify, function(x) as.numeric(x))
cr <- cor(corr)
corrplot(cr, method = "circle")