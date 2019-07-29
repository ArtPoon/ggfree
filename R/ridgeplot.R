#' ridgeplot
#' 
#' \code{ridgeplot} generates stacked density plots (aka. "ridge plots") in 
#' the style of the package \code{ggridge}, but using only base R graphics.
#' 
#' @examples
#' ridgeplot(split(iris$Sepal.Length, iris$Species))
#' 
#' # reverse ordering, use solid colour lines and modify kernel density
#' ridgeplot(split(iris$Sepal.Length, iris$Species), step=-0.2, 
#'           col=c('firebrick', 'steelblue', 'olivedrab'), lwd=5, 
#'                     density.args=list(n=20, kernel='rectangular', adjust=0.5))
#'                     
#' # fill densities with translucent colours
#' require(RColorBrewer)
#' pal <- add.alpha(brewer.pal(3, 'Set1'), 0.5)
#' ridgeplot(split(iris$Sepal.Length, iris$Species), step=0.4, col='white', 
#' fill=pal, lwd=2, xlab='Sepal length', cex.lab=1.2)
#' 
#' # passing arguments to add.grid()
#' ridgeplot(split(iris$Sepal.Length, iris$Species), step=0.4, col='white', 
#' add.grid=T, grid.args=list(mode='y', bg.col='black', fg.col='grey', 
#' lwd.major=0.5, lwd.minor=0))
#' 
#' @param x: a list of numeric vectors, such as produced by calling split().
#' @param labels: override names in list 'x'
#' @param xlab: string to label x-axis (default NA)
#' @param ylab: string to label y-axis (default NA)
#' @param step: numeric value to determine spread and ordering of densities.
#' Negative values cause reversed ordering.
#' @param col: character vector of R colours for lines and borders.  Will recycle 
#' colours if there are fewer than the number of groups in 'x'.
#' @param fill: character vector of R colours for filling densities.  Will recycle
#' colours if there are fewer than the number of groups in 'x'.
#' @param lwd: line width for drawing density curves and line segments.
#' @param density.args: list of optional arguments for density().
#' @param add.grid: if TRUE, call add.grid() before drawing densities
#' @param grid.args: list of optional arguments for add.grid()
#' @param ...: additional arguments for plot()
#' 
#' @export
ridgeplot <- function(x, labels=NA, xlab=NA, ylab=NA, step=0.2,
                      col=NA, fill=NA, lwd=1, density.args=list(),
                      add.grid=F, grid.args=list(), ...) {

  # check inputs
  if (is.list(x)) {
    # check that list contains numeric vectors
    if (!all(sapply(x, is.numeric))) {
      stop("List 'x' must contain only numeric vectors")
    }
  } else {
    stop("Unsupported class of argument x (must be list)")
  }
  
  if (any(is.na(labels))) {
    # extract labels from list 'x'
    labels <- names(x)
  }
  
  # generate kernel densities
  kdens <- lapply(x, function(xx) do.call('density', c(list(x=xx), density.args)))
  n <- length(kdens)  # number of groups
  
  # determine plot ranges from densities
  all.x <- c(sapply(kdens, function(k) k$x))
  all.y <- c(sapply(1:n, function(i) kdens[[i]]$y + i*step))
  
  # parse optional colour vector arguments
  if (all(is.na(col))) {
    pal <- rep('black', n)  # default
  } else {
    # reuse values in <col> if less than n
    pal <- rep(col, length.out=n)
  }
  if (all(is.na(fill))) {
    bg <- rep(NA, n)
  } else {
    bg <- rep(fill, length.out=n)
  }
  
  # generate plot region
  plot(NA, xlim=range(all.x), ylim=range(all.y), xlab=xlab, ylab=ylab, yaxt='n', ...)
  axis(side=2, at=seq(step, n*step, step), labels=labels, las=2)
  
  # optional grid
  if (add.grid) {
    do.call("add.grid", grid.args)
  }
  
  ordering <- seq(n, 1, -1)
  if (step < 0) {
    ordering <- 1:n
  }
  for (i in ordering) {
    kd <- kdens[[i]]
    y <- kd$y + i*step
    
    polygon(kd$x, y, col=bg[i], border=NA)
    lines(kd$x, y, col=pal[i], lwd=lwd)
    
    # extend density curve to full horizontal range
    segments(x0=min(all.x), x1=min(kd$x), y0=min(y), y1=min(y), col=pal[i], lwd=lwd)
    segments(x0=max(kd$x), x1=max(all.x), y0=min(y), y1=min(y), col=pal[i], lwd=lwd)
  }
}
