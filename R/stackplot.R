#' stackplot
#' 
#' Generates a stacked area plot of the absolute or relative
#' frequencies of groups over time (or some other discrete or 
#' continuous variable).
#' 
#' Credit for streamplot
#' https://gist.github.com/menugget/7864454
#' 
#' @examples 
#' stackplot(EuStockMarkets, xlab='Days (1991-1998)', 
#'   ylab='Daily Closing Price', col=gg.rainbow(4), border=NA, 
#'   bty='n')
#' 
#' # a centered stackplot with shading 
#' last.mar <- par()$mar
#' par(mar=rep(1,4))
#' stackplot(EuStockMarkets, center=TRUE, density=seq(0, 60, 20), 
#'   angle=30, border='black', bty='n', xaxt='n', yaxt='n', lwd=0.5)
#' par(mar=last.mar)
#'
#' # diversity of SARS-CoV-2 in 2020
#' par(mar=c(1,1,3,1))
#' set.seed(1); pal <- gg.rainbow(ncol(cov2))[sample.int(ncol(cov2))]
#' stackplot(cov2, col=pal, center=T, bty='n', xaxt='n', yaxt='n',
#' xlab=NA, ylab=NA, spline=T, border='black', lwd=0.2, xlim=c(10, 20))
#' axis(side=3, at=seq(10, 20, 2), line=1, cex.axis=0.8, col='grey60')
#' par(mar=last.mar)
#' 
#' @param obj: an object of class 'matrix' or 'table'.  Rows are assumed 
#'             to correspond to repeated measures, and columns to 
#'             different groups.
#' @param x: optional, customize the horizontal location of data
#'           points (i.e., counts in `obj`)
#' @param freq: if FALSE, normalize to relative frequencies
#' @param center: if TRUE, normalize frequencies to the mean for 
#'                centered baseline
#' @param spline: if TRUE, interpolate data points to draw areas with 
#'                smoothed trendlines
#' @param density: numeric, density of shading lines for each area;
#'                 values recycled as necessary 
#' @param angle: numeric, angle of shading lines for each area;
#'               values recycled as necessary
#' @param col: character, colour specification strings for filling
#'             areas, recycled as necessary
#' @param border: border color for polygon areas
#' @param lwd: line weight for polygon borders
#' @param xlim: optional, allows user to override default horizontal
#'              plot range
#' @param ...: additional arguments to pass to `plot` function
#' 
#' @export
stackplot <- function(obj, x=NA, freq=TRUE, center=FALSE, spline=FALSE, 
                      density=NA, angle=NA, col=NA, border=NULL, 
                      lwd=1, xlim=NA, ...) {
  # check inputs
  if (is.data.frame(obj)) {
    obj <- as.matrix(obj)
  }
  if (is.matrix(obj)) {
    if (!is.numeric(obj)) {
      stop("Matrix `obj` must be numeric.")
    }
  }
  else if (is.table(obj)) {
    if (length(dim(obj)) != 2) {
      stop("Table `obj`` must have two dimensions only.")
    }
  }
  else {
    stop("Unsupported class of argument `obj` ('table' or 'matrix' only).")
  }
  
  # x is used to customize the horizontal location of counts
  if (!is.na(x)) {
    if (is.vector(x)) {
      if (length(x) != nrow(obj)) {
        stop("Length of `x` must match number of rows in obj.")
      }
    }
    else {
      stop("`x` must be a vector.")
    }
  }
  else {
    x <- 1:nrow(obj)
  }
  
  # recycle polygon vectors as required
  density <- rep(density, length.out=ncol(obj))
  angle <- rep(angle, length.out=ncol(obj))
  
  # generate cumulative counts
  cx <- t(apply(obj, 1, cumsum))
  cx <- cbind(rep(0, nrow(cx)), cx)
  if (!freq) {
    # normalize to relative frequencies
    cx <- sweep(cx, 1, apply(cx, 1, max), '/')
  } 
  else {
    if (center) {
      # adjust cumulative counts to center around mean
      cx <- sweep(cx, 1, apply(cx, 1, mean))
    }
  }
  
  ylim <- c(0, max(cx))
  if (center) {
    ylim <- range(cx)
  }
  
  # prepare plot region
  if (any(is.na(xlim))) {
    # allows user to override this default
    xlim <- range(x)
  }
  plot(NA, xlim=xlim, ylim=ylim, ...)
  
  for (i in 2:ncol(cx)) {
    if (spline) {
      y0 <- spline(x, cx[,i-1])
      y1 <- spline(x, cx[,i], xout=y0$x)
      xx <- c(y0$x, rev(y0$x))
      yy <- c(y0$y, rev(y1$y))
    }
    else {
      y0 <- cx[,i-1]
      y1 <- cx[,i]
      xx <- c(x, rev(x))
      yy <- c(y0, rev(y1))
    }
    
    polygon(x=xx, y=yy, density=density[i-1], 
            angle=angle[i-1], col=col[i-1], border=border, lwd=lwd)
  }

}

