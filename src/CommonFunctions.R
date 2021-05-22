install_packages <- function() {
  packages <- c(
    "ggplot2",
    "rpart",
    "rpart.plot",
    "gmodels",
    "e1071",
    "gridExtra",
    "randomForest"
  )
  
  install.packages(packages)
}
