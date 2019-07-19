slopegraph <- function(x, y, labels=FALSE, xlab='Groups', ylab=NA, type='b', 
                       colorize=F, pal=c('firebrick', 'steelblue'), shim=0.2, ...) {
  plot(NA, xlim=c(1-shim, 2+shim), ylim=range(c(x, y)), 
       xaxt='n', xlab=xlab, ylab=ylab, ...)
  axis(side=1, at=c(1,2), labels=labels)
  
  . <- apply(cbind(x, y), 1, function(r) {
    lines(x=c(1,2), y=r, type=type, 
          col=ifelse(colorize, ifelse(diff(r)>0, pal[1], pal[2]), 'black'))
  })
}

