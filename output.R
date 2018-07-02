output <- function(compare.result, clustering.method, mds.method, analyze.result) {
  x <- analyze.result$dendrogram
  # need to eliminate $points to use cmdscale as a mds.method
  y <- analyze.result$mds$points
  labels <- paste(as.character(compare.result$data$item), " (", as.character(compare.result$data$image), ")", sep = "")

  # plot
  plot(x, main = "Dendrogram Result", sub = paste("Method:", clustering.method), xlab = "items", ylab = "distance", labels = labels)
  plot(y, main = "MDS Result", sub = paste("Method:", mds.method), xlab = "", ylab = "", pch = 16, asp = 1)
  text(y, labels = labels, pos = 3);
}
