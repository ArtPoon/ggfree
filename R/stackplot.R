#' stackplot
#' 
#' Generates a stacked area plot of the absolute or relative
#' frequencies of groups over time (or some other discrete or 
#' continuous variable).  
#' Stream plot functionality inspired by https://gist.github.com/menugget/7864454
#' 
#' @references
#' Byron L, Wattenberg M. Stacked graphs–geometry & aesthetics. 
#' IEEE transactions on visualization and computer graphics. 
#' 2008 Oct 24;14(6):1245-52.
#' 
#' @examples 
#' stackplot(EuStockMarkets, xlab='Days (1991-1998)', 
#'   ylab='Daily Closing Price', col=gg.rainbow(4), border=NA, 
#'   bty='n')
#' 
#' # a centered stackplot with shading 
#' last.mar <- par()$mar
#' par(mar=rep(1,4))
#' stackplot(EuStockMarkets, type='t', density=seq(0, 60, 20), 
#'   angle=30, col='black', border='black', bty='n', xaxt='n', 
#'   yaxt='n', lwd=0.5)
#' par(mar=last.mar)
#'
#' # Christmas bird counts in Hamilton, Ontario
#' # https://sharleenw.rbind.io/post/hamilton_cbc_part_1/hamilton-christmas-bird-count-part-1/
#' par(mar=c(3,1,1,1))
#' n <- ncol(cbc)
#' stackplot(cbc[2:n], x=cbc[,1], type='w', bty='n', yaxt='n',
#'           spline=T, border='white', lwd=0.25)
#' labels <- gsub('\\.', ' ', names(cbc)[2:n])
#' legend(x=1959, y=-150, legend=rev(labels), bty='n', cex=0.7, 
#'        fill=rev(pal), y.intersp=0.8)
#' par(mar=last.mar)
#' 
#' @param obj: an object of class 'matrix' or 'table'.  Rows are assumed 
#'             to correspond to repeated measures, and columns to 
#'             different groups.
#' @param x: optional, customize the horizontal location of data
#'           points (i.e., counts in `obj`)
#' @param freq: if FALSE, normalize to relative frequencies
#' @param type: determines baseline of stacked areas, defaults to 'n' 
#'              for zero baseline (against horizontal axis); 't' applies
#'              symmetric layout (ThemeRiver), 'w' applies wiggle layout.
#' @param sort:  if TRUE, sort counts by marginal totals such that the most 
#'               frequent types are drawn on the outside of the stack.  This is 
#'               effective for stream plots, as the most frequent type 
#'               otherwise drives the overall shape.  Defaults to FALSE (
#'               plot by input order).
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
stackplot <- function(obj, x=NA, freq=TRUE, type='n', sort=FALSE, spline=FALSE, 
                      density=NA, angle=NA, col=NA, border=NULL, 
                      lwd=1, xlim=NA, ylim=NA, ...) {
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
  if (!any(is.na(x))) {
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
  
  if (freq) {
    f <- obj
  } else {
    # normalize to relative frequencies
    f <- sweep(obj, 1, apply(obj, 1, sum), '/')
  }
  n <- ncol(f)
  
  # sort frequencies so that dominant types are external
  if (sort) {
    sums <- apply(f, 2, sum)
    index <- order(sums)
    ni <- length(index)
    mid.ni <- n%/%2
    fold <- c(index[ni:mid.ni], index[1:(mid.ni-1)])
    f <- f[,fold]
  }
  
  # compute baseline
  if (type == 'n') {
    g0 <- rep(0, nrow(f))
  }
  else if (type == 't') {
    # symmetric (Havre et al., ThemeRiver)
    g0 <- -0.5 * apply(f, 1, sum)
  }
  else if (type == 'w') {
    # wiggle (see Byron and Wattenberg)
    # TODO: implement StreamGraph? need interpolation and numeric integration...)
    g0 <- apply(f, 1, function(x) -sum((n-1:length(x)+1)*x) / (n+1) )
  }
  else {
    stop("Unrecognized option for `baseline`, expected 'n', 't' or 'w")
  }
  
  # generate cumulative frequencies
  g <- cbind(g0, g0+t(apply(f, 1, cumsum)))
  
  # prepare plot region
  if (any(is.na(xlim))) {
    xlim <- range(x)  # allows user to override this default
  }
  if (any(is.na(ylim))) {
    ylim <- range(g)
  }
  plot(NA, xlim=xlim, ylim=ylim, ...)
  
  # default palette
  if (any(is.na(col))) {
    col <- gg.rainbow(n)
  }
  
  for (i in 2:ncol(g)) {
    if (spline) {
      y0 <- spline(x, g[,i-1])
      y1 <- spline(x, g[,i], xout=y0$x)
      xx <- c(y0$x, rev(y0$x))
      yy <- c(y0$y, rev(y1$y))
    }
    else {
      y0 <- g[,i-1]
      y1 <- g[,i]
      xx <- c(x, rev(x))
      yy <- c(y0, rev(y1))
    }
    
    polygon(x=xx, y=yy, density=density[i-1], 
            angle=angle[i-1], col=col[i-1], border=border, lwd=lwd)
  }
  invisible(list(baseline=g0, ylim=range(g)))
}

