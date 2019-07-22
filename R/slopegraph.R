slopegraph <- function(x, y, labels=FALSE, xlab='Groups', ylab=NA, type='b', 
                       colorize=F, pal=c('firebrick', 'steelblue'), shim=0.2, 
                       add.grid=F, ...) {
  # slopegraph displays paired sample data as two sets of points connected
  # by line segments as a visualization of overall trends
  
  # Args:
  #   x: first vector
  #   y: second vector
  #   labels: <optional> vector of labels to display along right side of plot
  #   xlab: label for x-axis, as in generic plot()
  #   ylab: label for y-axis, as in generic plot()
  #   type: 'b' displays both points and lines; 'l' displays lines only.
  #   colorize: use line colors to emphasize slope
  #   pal: vector of two colors for annotating slopes down and up, respectively
  #   shim: width of horizontal padding for plot
  #   add.grid: if TRUE, use package add.grid() function
  #   ...: additional arguments to pass to lines()
  
  plot(NA, xlim=c(1-shim, 2+shim), ylim=range(c(x, y)), 
       xaxt='n', xlab=xlab, ylab=ylab)
  axis(side=1, at=c(1,2), labels=labels)
  
  if (add.grid) {
    add.grid()
  }
  
  . <- apply(cbind(x, y), 1, function(r) {
    lines(x=c(1,2), y=r, type=type, 
          col=ifelse(colorize, ifelse(diff(r)>0, pal[1], pal[2]), 'black'),
          ...)
  })
}

