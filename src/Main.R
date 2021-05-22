install_packages()

library(titanic)

data <- titanic_train
data <- clean(data)
summary(data)
