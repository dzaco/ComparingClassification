source("src/CommonFunctions.R")
install_packages()

scoreboard <- data.frame('Algorithm Name'= character(), 
                         'Time' = double(), 
                         'Accuracy' = integer(),
                         'Database Name' = character(),
                         stringsAsFactors=FALSE) 

source("src/iris.R")
source("src/dressify.R")
source("src/adult.R")
