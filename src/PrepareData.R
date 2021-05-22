clean <- function(data) {
  data <- remove_id_columns(data)
  data <- fill_nan(data)
  
  return(data)
}

remove_id_columns <- function(data) {
  names <- tolower(colnames(data))
  idx <- grepl('id', names)
  data = data[, -idx]
  return(data)
}

fill_nan <- function(data) {
  data <- fill_nan_in_numeric_data(data)
  
  return(data)
}

fill_nan_in_numeric_data <- function(data) {
  for( col_number in 1:ncol(data))
  {
    if(is_numeric(data[col_number]))
    {
      data[col_number] <- fill_nan_in_numeric_col(data[col_number])
    }
  }
  return(data)
}

fill_nan_in_numeric_col <- function(col) {
  if(anyNA(col)) {
    col_vector <- unlist(col)
    col[is.na(col)] <- mean(col_vector, na.rm = T)
  }
  return(col)
}

is_numeric <- function(data) {
  tmp <- lapply(data, is.numeric)
  for(i in tmp)
  {
    if( i == FALSE)
      return(FALSE)
  }
  return(TRUE)
}



number_of_unique_value <- function(col) {
  count <- unique(col)
  count <- length(count[[1]])
  return(count)
}