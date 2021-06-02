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
    "rlang",
    "corrplot"
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
    "src/Visualisation.R"
  )
  
  for (file in list.of.sourcefile) {
    source(file)
  }
  
}

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

min_max_scale <- function(x)
{
  return ((x - min(x))/(max(x)-min(x)))
}
