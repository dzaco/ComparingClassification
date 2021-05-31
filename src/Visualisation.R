
simply_plot <- function(mydf, x_axis, y_axis, result_class, title = "") {
  g = ggplot(
    mydf,
    aes(
      x ={{x_axis}},
      y = {{y_axis}},
      color = {{result_class}}
    ))+ geom_point() + ggtitle(title)
  g
  return (g)
}

plot_group <- function(...) {
  group = grid.arrange(..., ncol = 2)
  group
  return(group)
}

# przykład
# p1 = plot(iris, `Sepal.Length`, `Sepal.Width`, `Species`)
# p2 = plot(iris, `Sepal.Width`, `Sepal.Length`, `Species`)
# group = plot_group(p1,p2,p3,p4)

plot_accurency <- function(accuracy_k) {
  plot(accuracy_k, type = "b", col = "blue", 
            cex = 1, pch = 20, 
            xlab = "k, number of neighbors", 
            ylab = "classification accuracy",
            main = "Accuracy vs Neighbors")
  abline(v = which(accuracy_k == max(accuracy_k)), col = "green", lwd = 1.5)
  abline(h = max(accuracy_k), col = "grey", lty = 2)
}


