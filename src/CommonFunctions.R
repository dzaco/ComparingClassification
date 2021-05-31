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
  
  for (pack in list.of.packages) {
    library(pack, character.only = TRUE)
  }
  
  list.of.sourcefile <- c(
    "src/Classifications.R",
    "src/CreateDatasets.R",
    "src/PrepareData.R",
    "src/Visualisation.R",
    "src/knn.R"
  )
  
  for (file in list.of.sourcefile) {
    source(file)
  }
  
}

accuracy = function(actual, predicted) {
  mean = mean(actual == predicted);
  mean
  return( mean )
}
