#' Basic clean of data
#' - Remove id column
#' - fill empty rows with NA symbol
#' - fill NA rows with mean for numeric columns
#' - transform to factor columns with small number of unique value - default 5 unique values
#' @param data.frame
#' @return modified data.frame
clean <- function(data) {
  data <- fill_empty_with_NA(data)
  data <- fill_nan(data)
  data <- transform_to_factor(data)
  
  return(data)
}

#' remove column with index == idx
#' @param data.frame and column index
#' @return modified data.frame
remove_column <- function(data, idx) {
  data = data[, -idx]
  return(data)
}

#' remove column with colname == name
#' @param data.frame
#' @return modified data.frame
remove_column <- function(data, name) {
  idx <- which(colnames(data) == name)
  data = data[, -idx]
  return(data)
}



#' fill nan values
#' @param data.frame
#' @return modified data.frame
fill_nan <- function(data) {
  for( col_number in 1:ncol(data))
  {
    if(is_numeric(data[col_number]))
    {
      data[col_number] <- fill_nan_in_numeric_col(data[col_number])
    }
    else
    {
      data[col_number] <- fill_nan_in_nonnumeric_col(data[col_number])
    }
  }
  return(data)
}

#' fill nan values in single column
#' @param single column as list
#' @return modified column as list
fill_nan_in_numeric_col <- function(col) {
  if(anyNA(col)) {
    col_vector <- unlist(col)
    col[is.na(col)] <- mean(col_vector, na.rm = T)
  }
  return(col)
}

#' fill nan values in single column
#' @param single column as list
#' @return modified column as list
fill_nan_in_nonnumeric_col <- function(col) {
  if(anyNA(col)) {
    col_vector <- unlist(col)
    biggest_group <- names(summary(col_vector)[1])
    col[is.na(col)] <- biggest_group
  }
  return(col)
}

#' check if argument is numberic
#' If argument is multicolumn data.frame check if all columns are numeric
#' @param any
#' @return true if parameter is numeric. Overwise return false
is_numeric <- function(data) {
  tmp <- lapply(data, is.numeric)
  for(i in tmp)
  {
    if( i == FALSE)
      return(FALSE)
  }
  return(TRUE)
}

#' fetch unique values in single column and return their number
#' @param single column as list
#' @return number of unique values in column
number_of_unique_value <- function(col) {
  count <- unique(col)
  count <- length(count[[1]])
  return(count)
}

#' transform this columns which have less or equals number of unique values 
#' @param data.frame
#' @return modified data.frame
transform_to_factor <- function(data, required_unique_number = 5) {
  for( col_number in 1:ncol(data))
  {
    if(number_of_unique_value(data[col_number]) <= required_unique_number)
    {
      data[col_number] <- as.factor(unlist(data[col_number]))
    }
  }
  return(data)
}

#' fill empty rows in columns with NA symbol
#' @param data.frame
#' @return modified data.frame
fill_empty_with_NA <- function(data) {
  for( col_number in 1:ncol(data))
  {
    col <- data[col_number]
    col[which(col == ""), ] <- NA
    data[col_number] <- col
  }
  return(data)
}


#' normalize continuse data
#' @param data.frame
#' @return modified data.frame
normalize <- function(data) {
  for( col_number in 1:ncol(data))
  {
    if(is_numeric(data[col_number]))
    {
      data[col_number] <- scale(data[col_number])
    }
  }
  return(data)
}




