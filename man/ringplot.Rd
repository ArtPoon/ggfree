% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ringplot.R
\name{ringplot}
\alias{ringplot}
\title{ringplot}
\usage{
ringplot(
  vec,
  r0,
  r1,
  theta = pi/2,
  x = NA,
  y = NA,
  col = NA,
  use.names = F,
  offset = 1,
  srt = 0,
  cex.label = 0.8,
  ...
)
}
\arguments{
\item{vec:}{a numeric vector containing data for plotting}

\item{r0:}{radius to inner edge of ring.  If set to 0, ringplot
produces the dreaded piechart.}

\item{r1:}{radius to outer edge of ring}

\item{theta:}{angle of first segment in radians (fraction of 2pi).
Essentially rotates the ringplot.}

\item{x:}{x-coordinate of origin. Defaults to NA for new plot.}

\item{y:}{y-coordinate of origin. Defaults to NA for new plot.}

\item{col:}{vector of colours for filling segments.  Defaults to 
RColorBrewer::brewer.pal(n=10, name="Set3")}

\item{use.names:}{if vec is a named vector, display labels}

\item{offset:}{radius adjustment of labels relative to origin}

\item{srt:}{string rotation for labels}

\item{cex.label:}{character expansion for labels}

\item{...:}{additional arguments for plot() if new}
}
\description{
\code{ringplot} generates a donut- or ring-plot to display the
relative frequencies in a vector.  If called with x- and y-
coordinates, then the ringplot will be added to the current plot
region, like calling \code{points} with \code{add=TRUE}.
If 'vec' is a named vector, setting use.names=TRUE will add 
labels.
}
\examples{
require(RColorBrewer)
pal1 <- brewer.pal(5, 'Blues')
pal2 <- brewer.pal(5, 'Reds')

# nested ring-plots
ringplot(VADeaths[,1], r0=0.3, r1=0.6, col=pal1)
ringplot(VADeaths[,2], x=0, y=0, r0=0.6, r1=0.9, col=pal2, 
use.names=T, offset=0.05, srt=90)
text(x=0, y=0, adj=0.5, label='VADeaths', cex=0.8)

}
