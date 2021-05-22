data <- data.frame(titanic_train)
data<- remove_id_columns(data)

for( col_number in 1:ncol(data))
{
  cat(col_number)
  cat(" ")
  cat(colnames(data[col_number]))
  cat(" ")
  cat(is_numeric(data[col_number]))
  cat("\n")
}


col <- data[5] #@ Age
vcol <- unlist(col)
mean <- mean(vcol, na.rm = T)
col[is.na(col)] <- mean(vcol, na.rm = T)


# tests 1
data <- titanic_train
data <- clean(data)
summary(data)




####################################################################


sex <- data[4]
sum <- summary(sex)
count <- unique(sex)
count <- length(count[[1]])
count2 <- number_of_unique_value(sex)






