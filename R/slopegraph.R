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
#' @param names.arg: optional names for labeling the x-axis
#' @param xlab: label for x-axis, as in generic \code{plot()}. Defaults to 'Groups'.
#' @param ylab: label for y-axis, as in generic \code{plot()}. Defaults to \code{NA}.
#' @param colorize: use line colours to emphasize slope
#' @param pal: vector of two colours for annotating slopes down and up, respectively.
#' @param shim: width of horizontal padding to left and right of plot
#' @param cex.text: character expansion for text
#' @param ...: additional arguments to pass to \code{plot()}
#' 
#' @export
slopegraph <- function(x, y=NA, type='b', names.arg=NA, xlab=NA, ylab=NA,
                       colorize=F, pal=c('firebrick', 'steelblue'), shim=0.5, 
                       cex.text=0.8, ...) {
  # save user's current mar settings
  last.mar <- par()$mar
  
  # check if 'x' holds a matrix or dataframe
  if (is.na(y)) {
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
  if (!is.na(names.arg) && (length(names.arg) != 2)) {
    stop("names must be character vector of length 2")
  }
  if (!is.numeric(x) || !is.numeric(y)) {
    stop ("x and y must be numeric vectors")
  }
  
  # prepare plot region
  #par(mar=c(5,5,2,5))
  plot(NA, xlim=c(ifelse(type=='t', 1-shim, 0.9), 2+shim), 
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
      lines(x=c(1,2), y=r, type='b', cex=0, col=col, ...)
      text(x=c(1,2), y=r, labels = r, col=col, cex=cex.text)
    } else{
      lines(x=c(1,2), y=r, type=type, col=col, ...)
    }
  })
  
  # restore user's previous mar settings
  par(mar=last.mar)
}
