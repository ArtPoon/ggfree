ridgeplot <- function(x, ..., labels, add.grid=F) {
  # Args:
  #   x: either the first vector followed by additional vectors, or a list
  #      produced by calling split().
  
  
  if (is.list(x)) {
    # check that list contains numeric vectors
    if (!all(sapply(x, class))) {
      stop("List x must contain only numeric vectors")
    }
  } else if (is.numeric(x)) {
    # turn into list
    x <- list(x)
    for (x2 in list(...)) {
      if (!is.numeric(x2)) stop("Numeric vector required")
      x[[length(x)+1]] <- x2
    }
  } else {
    stop("Unsupported class of argument x (must be list or numeric)")
  }
  
  # generate plot region
  plot(NA, xlim=range(unlist(x)), ylim=c(1, length(x)))
  
  # generate kernel densities
  kd <- lapply(x, density)
}



ridgeplot(split(iris$Sepal.Length, iris$Species))
