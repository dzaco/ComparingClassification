data(iris)
train = create_train_dataset(iris, 100, 123)
test = create_test_dataset(iris, 100, 123)
cl_name = 'Species'
X_train = remove_column(train, cl_name)
X_test = remove_column(test , cl_name)
Y_train = train[, cl_name]
Y_test = test[,cl_name]



set.seed(2)
k_to_try = 1:100
acc_k = rep(x = 0, times = length(k_to_try))

for(i in seq_along(k_to_try)) {
  pred = knn(train = scale(X_train), 
             test = scale(X_test), 
             cl = Y_train, 
             k = k_to_try[i])
  acc_k[i] = accuracy(Y_test, pred)
}
plot_accurency(acc_k)
best = best.k(acc_k)
