#' creates train dataset 
#' @param data.frame, smp_size, seed
#' @return train dataset
create_train_dataset <- function(data, smp_size = 100, seed = 123) {
  train_ind = get_train_ind(data, smp_size, seed)
  train = data[train_ind, ]
  return(train)
}

#' creates test dataset 
#' @param data.frame, smp_size, seed
#' @return test dataset
create_test_dataset <- function(data, smp_size = 100, seed = 123) {
  train_ind = get_train_ind(data, smp_size, seed)
  test = data[-train_ind, ]
  return(test)
}

get_train_ind <- function(data, smp_size = 100, seed = 123) {
  set.seed(seed)
  train_ind = sample(seq_len(nrow(data)), size = smp_size)
  return(train_ind)
}