add.grid <- function(fg.col='white', bg.col='ivory2', lwd.major=3, lwd.minor=1) {
  # add.grid draws a filled background in the plot region and 
  # lines for the major and minor ticks, in the style of ggplot2
  # 
  # Args:
  #   fg.col: colour for lines in foreground
  #   bg.col: colour for filled rectangle in background
  #   lwd.major: line width for major tick mark lines
  #   lwd.minor: line width for minor tick mark lines
  
  u <- par('usr')  # get plot region dimensions
  x <- par('xaxp')
  y <- par('yaxp')
  
  # fill background
  rect(xl=u[1], yb=u[3], xr=u[2], yt=u[4], col=bg.col, border=NA)
  
  x.major <- seq(x[1], x[2], length.out=x[3]+1)
  x.minor <- x.major[1:(length(x.major)-1)] + diff(x.major)[1]/2
  abline(v=x.major, col=fg.col, lwd=lwd.major, lend=2)
  abline(v=x.minor, col=fg.col, lwd=lwd.minor, lend=2)
  
  y.major <- seq(y[1], y[2], length.out=y[3]+1)
  y.minor <- y.major[1:(length(y.major)-1)] + diff(y.major)[1]/2
  abline(h=y.major, col=fg.col, lwd=lwd.major, lend=2)
  abline(h=y.minor, col=fg.col, lwd=lwd.minor, lend=2)
  
  box()  # redraw plot border
}

