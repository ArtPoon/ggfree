#' ringplot
#' 
#' \code{ringplot} generates a donut- or ring-plot to display the
#' relative frequencies in a vector.  If called with x- and y-
#' coordinates, then the ringplot will be added to the current plot
#' region, like calling \code{points} with \code{add=TRUE}.
#' If 'vec' is a named vector, setting use.names=TRUE will add 
#' labels.
#' 
#' @examples
#' ringplot(c(1,3,2,4), r0=0.5, r1=0.7)
#' ringplot(c(6,8,10,9,7), x=0, y=0, r0=0.7, r1=0.9)
#' @param vec: a numeric vector containing data for plotting
#' @param r0: radius to inner edge of ring
#' @param r1: radius to outer edge of ring
#' @param theta: angle of first segment in radians (fraction of 2pi).
#'               Essentially rotates the ringplot.
#' @param x: x-coordinate of origin. Defaults to NA for new plot.
#' @param y: y-coordinate of origin. Defaults to NA for new plot.
#' @param col: vector of colours for filling segments.  Defaults to 
#'             RColorBrewer::brewer.pal(n=10, name="Set3")
ringplot <- function(vec, r0, r1, theta=pi/2, x=NA, y=NA, 
                     col=NA, use.names=F) {
  if (!is.numeric(vec)) {
    stop("'vec' must be a numeric vector")
  }
  if (is.na(x) || is.na(y)) {
    # create new plot region
    par(mar=c(1,1,1,1))
    plot(NA, xlim=c(-1, 1), ylim=c(-1, 1), 
         bty='n', xaxt='n', yaxt='n', xlab=NA, ylab=NA)
    x <- 0  # set origin
    y <- 0
  }
  if (any(is.na(col))) {
    # default colour palette
    col <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", 
             "#80B1D3", "#FDB462", "#B3DE69", 
             "#FCCDE5", "#D9D9D9", "#BC80BD")
  }
  
  # use.names only works if input is a named vector
  if (is.null(names(vec))) {
    use.names <- FALSE
    warning("vec is not a named vector, setting use.names to FALSE")
  }
  
  # map frequency vector to radians
  thetas <- c(theta, 2*pi * cumsum(vec/sum(vec)) + theta)
  # recycle colours as necessary
  pal <- rep(col, length.out=length(thetas)-1)
  for (i in 2:length(thetas)) {
    draw.arc(x, y, r0, r1, thetas[i-1], thetas[i], col=pal[i-1])
    if (use.names) {
      theta.mid <- (thetas[i-1]+thetas[i])/2
      text(x+1.1*r1*cos(theta.mid), y+1.1*r1*sin(theta.mid),
           label=names(vec)[i-1], srt=theta.mid/(2*pi) * 360,
           cex=0.8)
    }
  }
}

sunburst <- function(x) {
  
}