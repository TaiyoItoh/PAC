analyze <- function(compare.result, clustering.method, mds.method) {
  # return value
  result <- NULL

  # data formatting
  # This code postulates that subject's answer itself represents Euclidean distance,
  # which is problematic when using the methods except "single" and "complete" for clustering.
  # It's also problematic for MDS because the result can be defferent depending on the matrix.
  # Use dist function if postulating that it doesn't represent Euclidean distance.
  data <- as.dist(compare.result$mat)

  # analysis
  dendrogram <- hclust(data, method = clustering.method, members = NULL)
  mds <- eval(call(mds.method, data, k = 2))
  result <- list(data = data, dendrogram = dendrogram, mds = mds)

  # return
  return(result)
}
