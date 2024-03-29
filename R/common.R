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
                     lwd.major=3, lwd.minor=1, bty='o') {
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

  box(bty=bty)  # redraw plot border
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


#' blend.colors
#' 
#' Average a vector of colours in RGBA colour space.
#' @examples 
#' blend.colors(c('red', 'blue', 'green'))
#'
#' @param col:  a vector of named or hexadecimal color specifications
#' @return a hex color string, including an alpha channel
#' 
#' @export
blend.colors <- function(col) {
  m <- col2rgb(col, alpha=TRUE)
  vals <- apply(m, 1, mean)
  rgb(vals[1], vals[2], vals[3], vals[4], maxColorValue=255)
}


#' gg.rainbow
#' 
#' Mimic default colour palette of ggplot2.
#' Credit: John Colby, https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
#' 
#' @param n:  number of colours to generate
#' @param c:  chroma, radial component of hue colour space (saturation)
#' @param l:  luminance in range [0, 100], vertical component of 
#'            hue colour space (converges to black as l approaches 0)
#' @return vector of colour specification strings
#' 
#' @export
gg.rainbow <- function(n, c=100, l=65, alpha=1) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h=hues, c=c, l=l, alpha=alpha)[1:n]
}


#' draw.arc
#' 
#' \code{draw.arc} draws an annular sector around origin (x,y)
#' as a polygon.  If only one radius argument is given, then \code{draw.arc}
#' will render an arc instead of an annular sector.
#' 
#' @examples 
#' # create plot region
#' par(mar=c(1,1,1,1))
#' plot(NA, xlim=c(-1, 1), ylim=c(-1, 1), bty='n' , xaxt='n', yaxt='n')
#' # draw annular sector as filled polygon
#' draw.arc(0, 0, theta0=0.7*pi, theta1=1.1*pi, r0=0.5, r1=0.8, col='steelblue')
#' # draw arc with custom line style
#' draw.arc(0, 0, theta0=0, theta1=0.7*pi, r0=0.65, col='black', lwd=2, lty=3)
#' 
#' @param x: x-coordinate of origin
#' @param y: y-coordinate of origin
#' @param theta0: start angle of arc
#' @param theta1: end angle of arc
#' @param r0: radius to inner arc
#' @param r1: radius to outer arc (optional, draw line if omitted)
#' @param n: number of points to interpolate curves
#' 
#' @export
draw.arc <- function(x, y, theta0, theta1, r0, r1=NA, n=64, ...) {
  z <- seq(theta0, theta1, length.out=n)
  if (is.na(r1)) {
    lines(x+r0*cos(z), y+r0*sin(z), ...)
  } else {
    polygon(
      x=x+c(r0*cos(z), r1*cos(theta1), r1*cos(rev(z)), r0*cos(theta0)), 
      y=y+c(r0*sin(z), r1*sin(theta1), r1*sin(rev(z)), r0*sin(theta0)), 
      ...
    ) 
  }
}



#' draw.stack
#' 
#' Draw stacked bars as in barplot(), but with arbitrary location
#' on x-axis.
#' TODO: extend for circular layouts using draw.arc
#' 
#' @param x: numeric vector or matrix of non-negative values
#' @param offset: horizontal location of bars relative to origin
#' @param width: width of the drawing area for the stacked bars
#' @param space: inset from the bounds of the drawing area
#' @param col: vector of colours
#' @export
draw.stack <- function(x, xoffset, yoffset=0, width=1, space=0.1, col=NA, ...) {
  if (is.vector(x)) {
    y <- c(0, cumsum(x)) + yoffset
    rect(xleft=xoffset+space, xright=xoffset+width-space, 
         ybottom=y[1:(length(y)-1)], ytop=y[2:length(y)], 
         col=col, ...)
  }
  else if (is.matrix(x)) {
    for (i in 1:ncol(x)) {
      y <- c(0, cumsum(x[,i])) + yoffset
      rect(xleft=xoffset+space, xright=xoffset+width-space, 
           ybottom=y[1:(length(y)-1)], ytop=y[2:length(y)], 
           col=col, ...)
      xoffset <- xoffset + width
    }
  }
}


#' rotate.layout
#' 
#' @param layout: an object of S3 class phyloLayout
#' @param angle: rotation angle in degrees
#' @return rotated phyloLayout object
#' 
rotate.layout <- function(layout, angle) {
  rotation.mx <- matrix(
    c(cos(angle), -sin(angle), sin(angle), cos(angle)),
    ncol=2, byrow=T)
  nodes.xy <- rbind(layout$nodes$x, layout$nodes$y)
  result <- rotation.mx %*% nodes.xy
}


#' axis.srt
#' Extend generic axis() function with rotation of labels (string rotation,
#' srt).  If labels on the x-axis are parallel, then they are center justified;
#' otherwise they are right-justified.
#' 
#' @examples 
#' par(mar=c(5,5,5,5), xpd=FALSE)
#' boxplot(split(iris$Sepal.Length, iris$Species), xaxt='n')
#' axis.srt(side=1, at=1:3, labels=levels(iris$Species), srt=30)
#' axis.srt(side=3, at=1:3, labels=levels(iris$Species), srt=-30)
#' # draw horizontally instead
#' boxplot(split(iris$Petal.Length, iris$Species), horizontal=TRUE, yaxt='n')
#' axis.srt(side=2, at=1:3, labels=levels(iris$Species), srt=-30)
#' axis.srt(side=4, at=1:3, labels=levels(iris$Species), srt=30)
#' 
#' @param side: an integer specifying which side of the plot the axis is
#'              placed as follows: 1=below, 2=left, 3=above and 4=right.
#' @param at: the points at which tick-marks are to bve drawn.
#' @param labels: a character vector of labels.
#' @param srt:  string rotation in degrees, where 0 is horizontal
#' @param offset:  distance to draw labels relative to axis, scaled to 
#'                 dimensions of plot region.
#' @param cex:  character expansion factor for axis labels
#' @param ...:  other arguments passed to axis(), *e.g.*, `line` or `col`
#' 
#' @export
axis.srt <- function(side, at, labels, srt, offset=0.05, cex=1, ...) {
  # draw the usual axis without labels
  axis(side=side, at=at, labels=FALSE, ...)
  
  # get plot region coordinates
  orient <- ifelse(side %% 2 == 1, 3, 1)
  span <- diff(par("usr")[orient:(orient+1)])
  baseline <- par("usr")[c(3,1,4,2)][side] + offset*span*ifelse(side>2, 1, -1)
  
  if (side %% 2 == 1) {
    # horizontal axis
    text(x=at, y=baseline, labels=labels, cex=cex, 
         xpd=NA, srt=srt, adj=ifelse(srt==0, 0.5, 1))
  } else {
    # vertical axis
    text(x=baseline, y=at, labels=labels, cex=cex, 
         xpd=NA, srt=srt, adj=ifelse(side==2, 1, 0))
  }
}

