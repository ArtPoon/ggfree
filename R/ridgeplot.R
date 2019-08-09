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
#' @param xlim: by default, automatically set to range of
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
#' @param extend.lines: whether to draw densities across the horizontal range of plot.  
#' Defaults to TRUE.
#' @param ...: additional arguments for plot()
#' 
#' @export
ridgeplot <- function(x, xlim=NA, labels=NA, yaxt='s', xlab=NA, ylab=NA, step=0.2,
                      col=NA, fill=NA, lwd=1, density.args=list(),
                      add.grid=F, grid.args=list(), extend.lines=TRUE, add=FALSE, ...) {

  # check inputs
  if (is.list(x)) {
    # check that list contains numeric vectors
    if (!all(sapply(x, is.numeric))) {
      stop("List 'x' must contain only numeric vectors")
    }
  } else {
    stop("Unsupported class of argument x (must be list)")
  }
  
  n <- length(x)  # number of groups
  
  if (any(is.na(labels))) {
    # extract labels from list 'x'
    labels <- names(x)
  }
  
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
  
  # generate kernel densities
  kdens <- list()
  for (i in 1:length(x)) {
    xi <- x[[i]]
    if (length(xi) == 0 && is.numeric(xi)) {
      # is numeric(0)
      kdens[[names(x)[i]]] <- NA
    } else {
      kdens[[names(x)[i]]] <- do.call('density', c(list(x=xi), density.args))
    }
  }
  
  if (!add) {
    # determine plot ranges from densities
    all.x <- c(sapply(kdens, function(k) k$x))
    all.y <- c(sapply(1:n, function(i) kdens[[i]]$y + i*step))
    
    # generate plot region
    if (any(is.na(xlim))) {
      xlim <- range(all.x)
    }
    plot(NA, xlim=xlim, ylim=range(all.y), xlab=xlab, ylab=ylab, yaxt='n', ...)
    
    # override y-axis labels
    if (yaxt != 'n') axis(side=2, at=seq(step, n*step, step), labels=labels, las=2)
    
    # optional grid
    if (add.grid) {
      do.call("add.grid", grid.args)
    }
  }
  
  # arrangement of densities
  ordering <- seq(n, 1, -1)
  if (step < 0) {
    ordering <- 1:n
  }
  for (i in ordering) {
    kd <- kdens[[i]]
    if (length(kd) == 1 && is.na(kd)) next  # skip missing entry
    y <- kd$y + i*step
    
    polygon(kd$x, y, col=bg[i], border=NA)
    lines(kd$x, y, col=pal[i], lwd=lwd)
    
    # extend density curve to full horizontal range
    if (extend.lines) {
      segments(x0=min(all.x), x1=min(kd$x), y0=min(y), y1=min(y), col=pal[i], lwd=lwd)
      segments(x0=max(kd$x), x1=max(all.x), y0=min(y), y1=min(y), col=pal[i], lwd=lwd)
    }
  }
}
