install_packages <- function() {
  list.of.packages <- c(
    "ggplot2",
    "rpart",
    "rpart.plot",
    "gmodels",
    "e1071",
    "gridExtra",
    "randomForest",
    
    "devtools",
    "roxygen2",
    "rlang"
  )
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  else print("already have all the packages")
  
  
}
