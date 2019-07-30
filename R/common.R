#' add.grid
#' 
#' \code{add.grid} draws a filled background in the plot region and 
#' lines for the major and minor ticks, in the style of ggplot2.  Because 
#' generic plot drawing adds objects in layers, \code{add.grid} should be called
#' *after* drawing an empty plot region, and *before* adding any points or lines. 
#' 
#' @examples
#' x <- rnorm(100)
#' y <- rnorm(100)
#' plot(NA, xlim=range(x), ylim=range(y))  # prepare empty plot
#' add.grid()
#' points(x, y, pch=21, bg='white')  # now draw the data
#' 
#' @param mode: 'xy' displays both horizontal and vertical lines, 'x' horizontal only
#' and 'y' vertical only.  Any string without 'x' or 'y' gives plain background.
#' @param fg.col: colour for grid lines in foreground
#' @param bg.col: colour for grid background
#' @param lwd.major: line width for major grid lines
#' @param lwd.minor: line width for minor grid lines.  Set to 0 to suppress.
#' 
#' @export
add.grid <- function(mode='xy', fg.col='white', bg.col='ivory2', 
                     lwd.major=3, lwd.minor=1) {
  u <- par('usr')  # get plot region dimensions
  x <- par('xaxp')
  y <- par('yaxp')
  
  # fill background
  rect(xl=u[1], yb=u[3], xr=u[2], yt=u[4], col=bg.col, border=NA)
  
  if (grepl('y', mode)) {
    x.major <- seq(x[1], x[2], length.out=x[3]+1)
    x.minor <- x.major[1:(length(x.major)-1)] + diff(x.major)[1]/2
    abline(v=x.major, col=fg.col, lwd=lwd.major, lend=2)
    abline(v=x.minor, col=fg.col, lwd=lwd.minor, lend=2)
  }
  
  if (grepl('x', mode)) {
    y.major <- seq(y[1], y[2], length.out=y[3]+1)
    y.minor <- y.major[1:(length(y.major)-1)] + diff(y.major)[1]/2
    abline(h=y.major, col=fg.col, lwd=lwd.major, lend=2)
    abline(h=y.minor, col=fg.col, lwd=lwd.minor, lend=2)    
  }

  box()  # redraw plot border
}


#' add.alpha
#' 
#' \code{add.alpha} modifies a vector of named ('firebrick') or hexdecimal 
#' ('#B22222') color specifications to add an alpha (transparency) channel.  
#' Inspired by https://gist.github.com/mages/5339689
#' 
#' @examples 
#' require(RColorBrewer)
#' pal <- add.alpha(brewer.pal(5, 'Set1'), 0.5)
#' plot(rnorm(100), rnorm(100), pch=21, col='black', bg=pal, cex=10)
#' 
#' @param col: A string vector of colour names or hex colour strings.
#' @param alpha: A number between 0 (solid) and 1 (fully transparent).
#' @return A string vector of modified hex colour strings.
#' 
#' @export
add.alpha <- function(col, alpha) {
  sapply(col, function(cl) {
    vals <- col2rgb(cl)  # convert colour to RGB values
    rgb(vals[1], vals[2], vals[3], alpha*255, maxColorValue=255)
  })
}


#' draw.arc
#' 
#' \code{draw.arc} draws an annular sector around origin (x,y)
#' as a polygon
#' 
#' @examples 
#' # create plot region
#' par(mar=c(5,5,1,1))
#' plot(NA, xlim=c(-1, 1), ylim=c(-1, 1))
#' draw.arc(0, 0, 0.5, 0.8, 0.7*pi, 1.1*pi, col='steelblue')
#' 
#' @param x: x-coordinate of origin
#' @param y: y-coordinate of origin
#' @param r0: radius to inner arc
#' @param r1: radius to outer arc
#' @param theta0: start angle of arc
#' @param theta1: end angle of arc
#' @param border: colour for line around polygon border
#' @param col: colour for polygon fill
#' @param lty: line type parameter
#' @param n: number of points to interpolate curves
draw.arc <- function(x, y, r0, r1, theta0, theta1, border=NULL, 
                     col=NA, lty=par('lty'), n=64) {
  z <- seq(theta0, theta1, length.out=n)
  polygon(
    x=c(x+r0*cos(z), x+r1*cos(theta1), x+r1*cos(rev(z))), 
    y=c(y+r0*sin(z), y+r1*sin(theta1), x+r1*sin(rev(z))),
    border=border, col=col
    )
}


