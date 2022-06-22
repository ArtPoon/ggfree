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
#' require(RColorBrewer)
#' pal1 <- brewer.pal(5, 'Blues')
#' pal2 <- brewer.pal(5, 'Reds')
#' 
#' # nested ring-plots
#' ringplot(VADeaths[,1], r0=0.3, r1=0.6, col=pal1)
#' ringplot(VADeaths[,2], x=0, y=0, r0=0.6, r1=0.9, col=pal2, 
#' use.names=T, offset=0.05, srt=90)
#' text(x=0, y=0, adj=0.5, label='VADeaths', cex=0.8)
#' 
#' @param vec: a numeric vector containing data for plotting
#' @param r0: radius to inner edge of ring.  If set to 0, ringplot
#'            produces the dreaded piechart.
#' @param r1: radius to outer edge of ring
#' @param theta: angle of first segment in radians (fraction of 2pi).
#'               Essentially rotates the ringplot.
#' @param x: x-coordinate of origin. Defaults to NA for new plot.
#' @param y: y-coordinate of origin. Defaults to NA for new plot.
#' @param col: vector of colours for filling segments.  Defaults to 
#'             RColorBrewer::brewer.pal(n=10, name="Set3")
#' @param use.names: if vec is a named vector, display labels
#' @param offset: radius adjustment of labels relative to origin
#' @param srt: string rotation for labels
#' @param cex.label: character expansion for labels
#' @param ...: additional arguments for plot() if new
#'             
#' @export
ringplot <- function(vec, r0, r1, theta=pi/2, x=NA, y=NA, 
                     col=NA, use.names=F, offset=1, srt=0, cex.label=0.8, ...) {
  if (!is.numeric(vec)) {
    stop("'vec' must be a numeric vector")
  }
  if (is.na(x) || is.na(y)) {
    # create new plot region
    par(mar=c(1,1,1,1))
    plot(NA, xlim=c(-1, 1), ylim=c(-1, 1), 
         bty='n', xaxt='n', yaxt='n', xlab=NA, ylab=NA, ...)
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
  if (use.names & is.null(names(vec))) {
    use.names <- FALSE
    warning("vec is not a named vector, setting use.names to FALSE")
  }
  
  # map frequency vector to radians
  thetas <- c(theta, 2*pi * cumsum(vec/sum(vec)) + theta)
  # recycle colours as necessary
  pal <- rep(col, length.out=length(thetas)-1)
  for (i in 2:length(thetas)) {
    draw.arc(x, y, thetas[i-1], thetas[i], r0, r1, col=pal[i-1])
    if (use.names) {
      theta.mid <- (thetas[i-1]+thetas[i])/2
      xl <- x+(r1+offset)*cos(theta.mid)
      yl <- y+(r1+offset)*sin(theta.mid)
      # middle of the arc
      srtl <- srt + theta.mid/(2*pi) * 360
      # FIXME: flip labels right-side up
      if (theta.mid > 0 & theta.mid < pi) srtl <- srtl + 180
      text(xl, yl, label=names(vec)[i-1], srt=srtl, cex=cex.label)
    }
  }
}


# essentially a stacked ringplot
sunburst <- function(x) {
  # work in progress!
}


#' polarplot
#' 
#' A polar area diagram is a circular plot where the circle is partitioned by 
#' radii at equal angles, and the areas of the resulting sections are rescaled in 
#' proportion to the respective frequencies.  The origin of the polar
#' 
#' @examples
#' require(RColorBrewer)
#' pal <- brewer.pal(3, 'Pastel2')
#' 
#' # load the Florence Nightingale data set
#' require(HistData)
#' ng <- subset(Nightingale, Year==1855, c('Wounds.rate', 'Other.rate', 'Disease.rate'))
#' row.names(ng) <- Nightingale$Month[Nightingale$Year==1855]
#' 
#' par(mar=rep(0,4), family='Palatino')
#' polarplot(as.matrix(ng), x=0.2, y=0.3, decay=1, theta=1.1*pi, col=pal, 
#' use.names=T)
#' title('Causes of mortality in British army, Crimean War (1855)', 
#'       font.main=1, line=-3)
#' legend(x=-0.8, y=0.6, legend=c('Wounds', 'Other', 'Disease'), bty='n', 
#'        fill=pal, cex=0.9)
#'        
#' # generate a wind rose (https://en.wikipedia.org/wiki/Wind_rose)
#' 
#' 
#' @param obj: a numeric vector, matrix or table of frequency data
#' @param r: radius for inner circle, defaults to 0
#' @param theta: rotation offset for plot in radians, defaults to pi/2
#' @param space: a numeric vector on interval [0,1).  Values are reused as
#'               necessary.  Defaults to 0.
#' @param col: a vector of colour strings
#' @param use.names: if 'obj' is a named vector or matrix with row names, 
#'                   use these to label the outer edge of each sector.
#' @param pad.names: extra distance from origin for labels (default 0.05)
#' @param cex.names: character expansion factor for labels (default 0.8)
#' @param col.names: color to pass to text(), defaults to black
#' @param border: color to draw border, passed to polygon(); default black
#' @param las: 1, horizontal (default); 2, perpendicular (along radius)
#' @param ...: additional arguments passed to the \code{plot} function.
#'  
#' @export
polarplot <- function(obj, x=0, y=0, r=0, theta=0.5*pi, space=0, col=NA, 
                      use.names=FALSE, pad.names=0.05, cex.names=0.8, col.names=NULL,
                      border='black', las=1, ...) {
  if (!is.numeric(obj)) {
    stop("obj must be a numeric vector or matrix, or a table")
  }
  if (is.vector(obj)) {
    obj <- as.matrix(obj)
  }
  obj <- obj/sum(obj)
  
  n.sect <- nrow(obj)  # number of sectors
  n.lev <- ncol(obj)  # number of levels (layers)
  space <- rep(space, length.out=n.lev)
  
  # create new plot region
  plot(NA, xlim=c(-1, 1), ylim=c(-1, 1), 
       bty='n', xaxt='n', yaxt='n', xlab=NA, ylab=NA, ...)
  
  # angles of sector boundaries
  h <- seq(0, 2*pi, length.out=n.sect+1) + theta
  
  if (any(is.na(col))) {
    # default colour palette = brewer.pal(10, 'Set3')
    col <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", 
             "#80B1D3", "#FDB462", "#B3DE69", 
             "#FCCDE5", "#D9D9D9", "#BC80BD")
  }
  # recycle colours as necessary
  pal <- rep(col, length.out=length(h)-1)
  
  dh0 <- diff(h)[1]
  r0 <- rep(r, n.sect)
  
  for (i in 1:n.lev) {
    dh <- dh0 - space[i]  # change in arc length
    r1 <- sqrt(r0*r0 + 2*obj[,i]/dh0)
    for (j in 1:n.sect) {
      draw.arc(x, y, theta0=h[j], theta1=h[j]+dh, r0=r0[j], r1=r1[j], 
               col=pal[i], border=border)
    }
    # update r0
    r0 <- r1
  }
  # r0 now contains radii of outer layers
  
  # optionally label sections
  if (use.names) {
    if (is.vector(obj)) {
      labels <- names(obj)
    } else {
      labels <- row.names(obj)
    }
    h.mids <- (h+dh0/2)[1:n.sect]
    angle <- h.mids / (2*pi)*360 - ifelse(las==1, 90, 0)
    if (las==2) {
      angle <- sapply(angle, function(x) {
        ifelse (x>90 && x<270, x+180, x) 
      })
    }

    for (i in 1:length(labels)) {
      text(x = x+(r0[i]+pad.names)*cos(h.mids[i]), 
           y = y+(r0[i]+pad.names)*sin(h.mids[i]), 
           labels = labels[i],
           adj=ifelse(las==1, 0.5, 
                      ifelse(h.mids[i]%%(2*pi)>pi/2 && 
                               h.mids[i]%%(2*pi)<(1.5*pi), 1, 0)), 
           srt=angle[i],
           cex=cex.names,
           col=col.names
           )
    }
  }
}




