#' slopegraph
#' 
#' \code{slopegraph} displays paired sample data as two sets of points connected 
#' by line segments as a visualization of overall trends.  See also @leeper/slopegraph
#' at GitHub.
#' 
#' @examples 
#' # CO2 emissions data
#' co2.emissions
#' 
#' # with default settings
#' slopegraph(co2.emissions)
#' 
#' # annotate slopes with colour
#' slopegraph(co2.emissions, names.arg=c(2000, 2010), 
#' ylab=expression(text=paste('CO'[2], ' emissions (metric tons) per capita')), 
#' type='b', cex.lab=1.2, colorize=T)
#' 
#' # more in style of Edward Tufte
#' slopegraph(co2.emissions, names.arg=c(2000, 2010), type='t')
#' title(expression(text=paste('CO'[2], ' emissions (metric tons) per capita')), 
#' family='Palatino')
#' 
#' @param x: numeric vector of values to plot on the left
#' @param y: numeric vector of values to plot on the right
#' @param type: 'b' displays both points and lines; 'l' displays lines only.
#'              't' replaces points with numeric values, as per Tufte.
#' @param box: if TRUE, add box-and-whisker plots in shim regions
#' @param names.arg: optional names for labeling the x-axis
#' @param xlab: label for x-axis, as in generic \code{plot()}. Defaults to 'Groups'.
#' @param ylab: label for y-axis, as in generic \code{plot()}. Defaults to \code{NA}.
#' @param colorize: use line colours to emphasize slope
#' @param pal: vector of two colours for annotating slopes down and up, respectively.
#' @param shim: width of horizontal padding to left and right of plot
#' @param cex.text: character expansion for text
#' @param pt.cex:  character expansion for points, if type=='b'
#' @param ...: additional arguments to pass to \code{plot()}
#' 
#' @export
slopegraph <- function(x, y=NA, type='b', box=FALSE, names.arg=NA, xlab=NA, ylab=NA,
                       colorize=F, pal=c('firebrick', 'steelblue'), shim=0.5, 
                       cex.text=0.8, pt.cex=1, ...) {
  # save user's current mar settings
  last.mar <- par()$mar
  
  # check if 'x' holds a matrix or dataframe
  if (all(is.na(y))) {
    if (is.element(class(x), c('data.frame', 'matrix')) && ncol(x)==2) {
      if (all(is.na(names.arg))) {
        names.arg <- names(x)
      }
      y <- x[,2]
      names(y) <- row.names(x)
      x <- x[,1]
      names(x) <- names(y)
    } else {
      stop("x must be a data frame or matrix of 2 columns")
    }
  }
  
  # check other inputs
  if (length(x) != length(y)) {
    stop("x and y must be of equal lengths")
  }
  if (all(!is.na(names.arg)) && (length(names.arg) != 2)) {
    stop("names must be character vector of length 2")
  }
  if (!is.numeric(x) || !is.numeric(y)) {
    stop ("x and y must be numeric vectors")
  }
  
  # prepare plot region
  #par(mar=c(5,5,2,5))
  plot(NA, xlim=c(1-shim, 2+shim), 
       ylim=range(c(x, y)), 
       xaxt='n', yaxt=ifelse(type=='t', 'n', 's'), bty='n', 
       xlab=xlab, ylab=ylab, ...)
  
  axis(side=1, at=c(1,2), labels=names.arg)
  
  # labels for slopes
  if (!is.null(names(x))) {
    par(xpd=NA)
    if (type=='t') {
      text(x=rep(0.95, times=length(y)), y=x, pos=2, labels=names(x), cex=cex.text)
    }
    text(x=rep(2.05, times=length(y)), y=y, pos=4, labels=names(x), cex=cex.text)
    par(xpd=FALSE)
  }
  
  # draw slopes
  . <- apply(cbind(x, y), 1, function(r) {
    col <- ifelse(colorize, ifelse(diff(r)>0, pal[1], pal[2]), 'black')
    if (type=='t') {
      # 'b' leaves space for labels
      lines(x=c(1,2), y=r, type='b', cex=0, col=col, ...)
      text(x=c(1,2), y=r, labels = r, col=col, cex=cex.text)
    } 
<<<<<<< HEAD
    else if (type=='x') {
      lines(x=c(1,2), y=r, type='l', col=col, ...)
      
    }
    else {
      lines(x=c(1,2), y=r, type=type, col=col, ...)
=======
    else {
      lines(x=c(1,2), y=r, type=type, col=col, cex=pt.cex, ...)
>>>>>>> a0fc610303afabe2c59c5a62f0de34668a6f4be1
    }
  })
  
  if (box) {
    
    x.box <- boxplot(x, plot=F)
    # median
    segments(x0=1-shim*0.75, x1=1-shim*0.25, y0=x.box$stats[3,1], lwd=3, lend=1)
    # whiskers
    segments(x0=1-shim*0.66, x1=1-shim*0.33, y0=x.box$stats[c(1, 5), 1])
    # IQR
    rect(xleft=1-shim*0.75, xright=1-shim*0.25, 
         ybot=x.box$stats[2,1], ytop=x.box$stats[4,1])
    segments(x0=1-shim*0.5, y0=x.box$stats[1,1], y1=x.box$stats[2,1], lty=2)
    segments(x0=1-shim*0.5, y0=x.box$stats[4,1], y1=x.box$stats[5,1], lty=2)
    
    y.box <- boxplot(y, plot=F)
    segments(x0=2+shim*0.75, x1=2+shim*0.25, y0=y.box$stats[3,1], lwd=3, lend=1)
    segments(x0=2+shim*0.66, x1=2+shim*0.33, y0=y.box$stats[c(1, 5), 1])
    rect(xleft=2+shim*0.75, xright=2+shim*0.25, 
         ybot=y.box$stats[2,1], ytop=y.box$stats[4,1])
    segments(x0=2+shim*0.5, y0=y.box$stats[1,1], y1=y.box$stats[2,1], lty=2)
    segments(x0=2+shim*0.5, y0=y.box$stats[4,1], y1=y.box$stats[5,1], lty=2)    
  }
  
  # restore user's previous mar settings
  par(mar=last.mar)
}
