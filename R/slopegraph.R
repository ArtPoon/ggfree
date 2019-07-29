#' slopegraph
#' 
#' \code{slopegraph} displays paired sample data as two sets of points connected 
#' by line segments as a visualization of overall trends.
#' 
#' @examples 
#' slopegraph(iris$Sepal.Width, iris$Petal.Length, colorize=T, add.grid=T, 
#' pch=21, bg='white')
#' 
#' @param x: numeric vector of values to plot on the left
#' @param y: numeric vector of values to plot on the right
#' @param groups: optional names for labeling the x-axis
#' @param labels: character vector of labels to annotate slopes
#' @param xlab: label for x-axis, as in generic \code{plot()}. Defaults to 'Groups'.
#' @param ylab: label for y-axis, as in generic \code{plot()}. Defaults to \code{NA}.
#' @param type: 'b' displays both points and lines; 'l' displays lines only.
#' @param colorize: use line colours to emphasize slope
#' @param pal: vector of two colours for annotating slopes down and up, respectively.
#' @param shim: width of horizontal padding to left and right of plot
#' @param add.grid: if TRUE, calls the function \code{ggfree::add.grid}
#' @param grid.args: list of optional arguments for \code{add.grid()}
#' @param ...: additional arguments to pass to \code{lines()}, *e.g.*, \code{lwd}
#' 
#' @export
slopegraph <- function(x, y, names=NA, labels=NA, xlab='Groups', ylab=NA, type='b', 
                       colorize=F, pal=c('firebrick', 'steelblue'), shim=0.2, 
                       cex.labels=0.8, add.grid=F, grid.args=list(), ...) {
  if (length(x) != length(y)) {
    stop("x and y must be of equal lengths")
  }
  if (!is.na(names) && (length(names) != 2)) {
    stop("names must be character vector of length 2")
  }
  if (!is.na(labels) && (length(labels) != length(x))) {
    stop("labels and x must be of equal lengths")
  }
  if (!is.numeric(x) || !is.numeric(y)) {
    stop ("x and y must be numeric vectors")
  }
  
  plot(NA, xlim=c(1-shim, 2+shim), ylim=range(c(x, y)), 
       xaxt='n', xlab=xlab, ylab=ylab)
  
  axis(side=1, at=c(1,2), labels=names)
  
  if (add.grid) {
    do.call('add.grid', c(list(mode='x'), grid.args))  # draw background and gridlines
  }
  
  if ((length(labels) > 1) && !is.na(labels))
    text(x=rep(2, times=length(y)), y=y, pos=4, labels=labels, cex=cex.labels)
  
  . <- apply(cbind(x, y), 1, function(r) {
    lines(x=c(1,2), y=r, type=type, 
          col=ifelse(colorize, ifelse(diff(r)>0, pal[1], pal[2]), 'black'),
          ...)
  })
}
