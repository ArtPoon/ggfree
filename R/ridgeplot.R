ridgeplot <- function(x, ..., labels, add.grid=F, step=0.1,
                      xlab=NA, ylab='Density', col=NA, bg=NA) {
  # Args:
  #   x: either the first vector followed by additional vectors, or a list
  #      produced by calling split().
  
  
  if (is.list(x)) {
    # check that list contains numeric vectors
    if (!all(sapply(x, is.numeric))) {
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
  
  # generate kernel densities
  kdens <- lapply(x, density)
  all.x <- c(sapply(kdens, function(k) k$x))
  all.y <- c(sapply(1:length(kdens), function(i) {
    kdens[[i]]$y + i*step
  }))
  
  # generate plot region
  plot(NA, xlim=range(all.x), ylim=range(all.y), 
       xlab=xlab, ylab=ylab)
  
  for (i in 1:length(kdens)) {
    kd <- kdens[[i]]
    y <- kd$y+i*step
    lines(kd$x, y)
    abline(h=min(y), col='grey')
  }
}



ridgeplot(split(iris$Sepal.Length, iris$Species))
