#' bookplot
#' 
#' Similar to graphics::spineplot, a bookplot visualizes the contingency table 
#' of two categorical variables (factors).  The width of each stack is 
#' scaled to the square root of the marginal total, and the height of each 
#' is scaled such that the area of each rectangle in the stack is proportional 
#' to the corresponding count.
#' 
#' @examples 
#' 
#' 
#' @param x: factor, independent variable
#' @param y: factor, dependent variable
#' @param space: amount of space between stacked bars
#' @param edges:  if TRUE, draw line segments connecting adjacent rectangles 
#'                of the same variable.
#' @param border:  colour for rectangle borders; defaults to NA (no border)
#' @param col:  colour(s) to fill rectangles
#' @export
bookplot <- function(x, y=NA, space=1, edges=T, border=NA, col=NA, lwd=1,
                       xaxt='s', axis.srt=90, axis.offset=0.05, cex.axis=1, 
                       edge.col='grey', edge.lwd=0.5,
                       legend=NA) {
  if (all(is.na(y))) {
    nx <- nlevels(x)
    sum.x <- as.integer(table(x))
    
    widths <- sqrt(as.integer(table(x)))
    heights <- widths / max(widths)
    
    rights <- cumsum(widths) + (1:nx)*space
    lefts <- rights - widths
    mids <- (rights+lefts)/2
    
    tab <- apply(table(x, y), 2, function(cl) cl/sum.x)
  }
  else {
    nx <- ncol(x)
    sum.x <- apply(x, 1, sum)
    tab <- apply(x, 2, function(cl) cl/sum.x)
  }
  ctab <- t(apply(tab, 1, cumsum))
  
  if (any(is.na(col))) {
    col <- hcl.colors(nlevels(y), "Spectral")
  }
  
  # determine figure dimensions
  plot(NA, xlim=c(space, sum(widths) + nx*space), ylim=c(0, 1), 
       bty='n', xaxt='n', yaxt='n', xlab=NA, ylab=NA)
  
  # draw axes
  if (xaxt != 'n') {
    axis(side=1, at=mids, labels=FALSE)
    if (axis.srt==0) {
      yspan <- diff(par("usr")[3:4])
      text(x=mids, y=par("usr")[3]-axis.offset*yspan, labels=row.names(tab), cex=cex.axis, 
           xpd=NA)  
    }
    else {
      text(x=mids, y=par("usr")[3]-axis.offset*yspan, adj=1, srt=axis.srt, cex=cex.axis,
           labels=row.names(tab), xpd=NA)  
    }
  }
  
  # draw rectangles
  for (ix in 1:nrow(tab)) {
    for (iy in 1:ncol(tab)) {
      val <- tab[ix, iy]
      cval <- ctab[ix, iy]
      if (val > 0) {
        rect(xl=lefts[ix], xr=rights[ix], 
             yb=(cval-val)*heights[ix], yt=cval*heights[ix], 
             col=col[iy], border=border, lwd=lwd)
      }
    }
  }
  
  # draw edges
  if (edges) {
    for (ix in 1:(nrow(tab)-1)) {
      # bottom segment
      segments(x0=rights[ix], x1=lefts[ix+1], y0=0, col=edge.col, lwd=edge.lwd)
      
      temp <- cbind(ctab[ix, ], ctab[ix+1, ])
      apply(temp[!duplicated(temp), ], 1, function(r) {
        segments(x0=rights[ix], x1=lefts[ix+1], 
                 y0=r[1]*heights[ix], y1=r[2]*heights[ix+1], 
                 col=edge.col, lwd=edge.lwd)
      })
    }
  } 
  
  # add legend
  if (is.list(legend)) {
    ny <- nlevels(y)
    temp <- seq(0, 1-1/ny, length.out=ny)
    legend$y <- legend$y0+(temp + 0.5/ny)*(legend$y1-legend$y0)
    text(x=legend$x0+legend$label.offset, y=legend$y, adj=1,
         labels=levels(y), xpd=NA, cex=legend$label.cex)
    par(xpd=NA)
    for (i in 1:ny) {
      shim <- 0.5/ny * (legend$y1-legend$y0)
      rect(xl=legend$x0, xr=legend$x1, 
           yb=legend$y[i]-shim, yt=legend$y[i]+shim, col=col[i])
      segments(x0=legend$x1, x1=lefts[1], 
               y0=legend$y[i]-shim, y1=ctab[1,i]-tab[1,i], 
               lwd=edge.lwd, col=legend$col)
    }
    segments(x0=legend$x1, x1=lefts[1], y0=legend$y[ny]+shim, y1=ctab[1,  ny], 
             lwd=edge.lwd, col=legend$col)
    par(xpd=FALSE)
  }
}

