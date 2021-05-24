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
    "roxygen2"
  )
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  else print("already have all the packages")
  
  list.of.sourcefile <- c(
    "src/PrepareData.R",
    "src/CreateDatasets.R",
    "src/Classifications.R"
  )
  
  for (file in list.of.sourcefile) {
    source(file)
  }
  
}
