% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ringplot.R
\name{polarplot}
\alias{polarplot}
\title{polarplot}
\usage{
polarplot(
  obj,
  x = 0,
  y = 0,
  r = 0,
  theta = 0.5 * pi,
  space = 0,
  col = NA,
  use.names = FALSE,
  pad.names = 0.05,
  cex.names = 0.8,
  col.names = NULL,
  border = "black",
  las = 1,
  ...
)
}
\arguments{
\item{obj:}{a numeric vector, matrix or table of frequency data}

\item{r:}{radius for inner circle, defaults to 0}

\item{theta:}{rotation offset for plot in radians, defaults to pi/2}

\item{space:}{a numeric vector on interval [0,1).  Values are reused as
necessary.  Defaults to 0.}

\item{col:}{a vector of colour strings}

\item{use.names:}{if 'obj' is a named vector or matrix with row names, 
use these to label the outer edge of each sector.}

\item{pad.names:}{extra distance from origin for labels (default 0.05)}

\item{cex.names:}{character expansion factor for labels (default 0.8)}

\item{col.names:}{color to pass to text(), defaults to black}

\item{border:}{color to draw border, passed to polygon(); default black}

\item{las:}{1, horizontal (default); 2, perpendicular (along radius)}

\item{...:}{additional arguments passed to the \code{plot} function.}
}
\description{
A polar area diagram is a circular plot where the circle is partitioned by 
radii at equal angles, and the areas of the resulting sections are rescaled in 
proportion to the respective frequencies.  The origin of the polar
}
\examples{
require(RColorBrewer)
pal <- brewer.pal(3, 'Pastel2')

# load the Florence Nightingale data set
require(HistData)
ng <- subset(Nightingale, Year==1855, c('Wounds.rate', 'Other.rate', 'Disease.rate'))
row.names(ng) <- Nightingale$Month[Nightingale$Year==1855]

par(mar=rep(0,4), family='Palatino')
polarplot(as.matrix(ng), x=0.2, y=0.3, decay=1, theta=1.1*pi, col=pal, 
use.names=T)
title('Causes of mortality in British army, Crimean War (1855)', 
      font.main=1, line=-3)
legend(x=-0.8, y=0.6, legend=c('Wounds', 'Other', 'Disease'), bty='n', 
       fill=pal, cex=0.9)
       
# generate a wind rose (https://en.wikipedia.org/wiki/Wind_rose)


}
