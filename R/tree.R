# WORK IN PROGRESS

# to generate a tree
require(twt)
path <- system.file('extdata', 'structSI.yaml', package='twt')
settings <- yaml.load_file(path)

set.seed(1)
structSI <- Model$new(settings)
run <- sim.outer.tree(structSI)
eventlog <- sim.inner.tree(run)
phy <- as.phylo(eventlog, transmissions=TRUE, migrations=TRUE) 


#' as.phyloData
#' 
#' Convert a `phylo` object into a data frame of edge information and 
#' a vector of node information.
#' 
#' @param phy:  an S3 object of class `phylo` (`ape` package), which 
#'        must be a rooted tree.
#' @return an S3 object of class `phyloData`
#' 
#' @examples 
#' require(ape)
#' phy <- rtree(20)
#' as.phyloData(phy)
#' 
#' @export
as.phyloData <- function(phy, unscaled=FALSE) {
  if (!is.element('phylo', class(phy))) {
    stop("Error: as.phyloData requires an ape:phylo object as input.")
  }
  if (!is.rooted(phy)) {
    stop("Error: phylo object must be rooted.")
  }
  
  # use post-order traversal to gather related tips for layout
  #phy2 <- reorder(phy, 'postorder')
  
  # convert edge attributes into data frame
  edges <- data.frame(
    parent = phy$edge[,1],
    child = phy$edge[,2],
    length = phy$edge.length,
    isTip = (phy$edge[,2] <= Ntip(phy))
  )
  
  # total branch length from root to node
  if (unscaled) {
    depths <- Ntip(phy)-node.depth(phy)
  } else {
    depths <- node.depth.edgelength(phy)
  }
  
  # convert node attributes to data frame
  nodes <- data.frame(
    row.names = c(phy$tip.label, phy$node.label),
    x = depths
  )
  # calculate number of tips per node
  st <- subtrees(phy)
  nodes$n.tips <- c(rep(0, Ntip(phy)), sapply(st, Ntip))
  
  # carry over any non-default attributes
  # we assume these are ordered correctly!
  default.keys <- c('tip.label', 'node.label', 'Nnode', 'edge', 'edge.length')
  for (key in names(phy)) {
    if (!is.element(key, default.keys)) {
      vec <- phy[[key]]
      if (length(vec) == nrow(edges)) {
        edges[key] <- vec
      }
      else if (length(vec) == nrow(nodes)) {
        # NOTE nodes are unchanged in order (numbering)
        nodes[key] <- vec
      }
      else {
        warning("as.phyloData ignoring attribute ", key, 
                " of length ", length(vec))
      }
    }
  }

  # return to original ordering (we don't have to change nodes)
  #index <- match(phy$edge[,2], edges$child)
  
  obj <- list(edges=edges, nodes=nodes)
  class(obj) <- 'phyloData'
  obj
}


#' print.phyloData
#' 
#' Generic function to display object.
#' @param obj:  S3 object of class `phyloData`
#' @export
print.phyloData <- function(obj) {
  n <- sum(obj$edges$isTip)
  cat("Phylogenetic data with", n, "tips and",
      nrow(obj$nodes)-n, "internal nodes.\n")
  cat("nodes:\n")
  cat(str(head(obj$nodes)))
  cat("edges:\n")
  cat(str(head(obj$edges)))
}


#' tree.layout
#' 
#' Generate the coordinates for the nodes and edges of a 
#' phylogenetic tree (ape:phylo object) using one of several
#' layout algorithms.  Used before calling the generic plot
#' functions (plot, points).
#' 
#' @param phy:  an S3 object of class `phylo``
#' @param type:  what type of layout algorithm to employ:
#' \itemize {
#'   \item{"r"}{Rectangular layout}
#'   \item{"s"}{Slanted (triangular) layout}
#'   \item{"u"}{Unrooted (equal-angle) layout}
#'   \item{"c"}{Circular (radial) layout}
#' }
#' @param unscaled:  if TRUE, return a cladogram layout
#' 
#' @return S3 object of class `phyloLayout`
#' 
#' @examples 
#' require(ape)
#' phy <- rcoal(20)
#' lay <- tree.layout(phy, 's')
#' head(lay$edges)
#' 
#' @export
tree.layout <- function(phy, type='r', unscaled=FALSE) {
  if (type=='r') {
    layout.rect(phy, unscaled, slanted=FALSE)
  }
  else if (type=='s') {
    layout.rect(phy, unscaled, slanted=TRUE)
  }
  else if (type=='u') {
    # unrooted
    layout.equalangle(phy, unscaled)
  }
  else if (type == 'o') {
    layout.radial(phy, unscaled)
  }
  else {
    stop("Unrecognized layout type '", type, "'")
  }
}


.layout.rect <- function(phy, unscaled=FALSE) {
  # automatically generate node labels if necessary
  if (is.null(phy$node.label)) {
    phy$node.label <- paste("Node", 1:Nnode(phy), sep='')
  }
  
  subs <- subtrees(phy)
  names(subs) <- phy$node.label
  
  # assign integer vertical positions to tips
  pd <- as.phyloData(phy, unscaled)
  
  tips <- pd$edges$child[pd$edges$isTip]
  pd$nodes$y <- rep(NA, nrow(pd$nodes))
  pd$nodes$y[tips] <- 1:Ntip(phy)
  
  # assign vertical positions to internal nodes
  internals <- pd$edges$child[!pd$edges$isTip]
  for (i in internals) {
    # retrieve subtree by node label
    st <- subs[[ as.character(row.names(pd$nodes)[i]) ]]
    y.tips <- pd$nodes$y[which(is.element(row.names(pd$nodes), st$tip.label))]
    pd$nodes$y[i] <- mean(y.tips)
  }
  root <- Ntip(phy)+1
  pd$nodes$y[root] <- mean(1:Ntip(phy))

  pd$edges$x0 <- pd$nodes$x[pd$edges$parent]
  pd$edges$x1 <- pd$nodes$x[pd$edges$child]
  pd$edges$y0 <- pd$nodes$y[pd$edges$parent]
  pd$edges$y1 <- pd$nodes$y[pd$edges$child]
  
  class(pd) <- c('phyloLayout', 'phyloData')
  pd
}



#' layout.radial
#' 
#' Generate the coordinates for a radial layout of a phylogenetic tree.
#' 
#' @param phy:  an S3 object of class `phylo`
#' 
#' @return S3 object of class `phyloLayout`
#' @keywords internal
.layout.radial <- function(phy) {
  warning("Under development")
}


#' layout.equalangle
#' 
#'  Genearte the coordinates for an unrooted layout of a phylogenetic
#'  tree using Joe Felsenstein's equal-angle algorithm.
#'  
#'  @param phy:  an S3 object of class `phylo`
#'  
#'  @return S3 object of class `phyloLayout`
#'  @keywords internal
.layout.equalangle <- function(phy) {
  warning("Under development")
}


#' plot.phyloLayout
#' 
#' Generic plot function for S3 objects of class `phyloLayout`.
#' 
plot.phyloLayout <- function(pd, ...) {
  if (!is.element('phyloLayout', class(pd))) {
    stop("Argument `pd` must be S3 object of class `phyloData`")
  }
  if (is.null(pd$nodes$y)) {
    stop("You have to apply one of the layout functions to the `phyloData` ",
         "object before plotting! e.g., `layout.rect()`")
  }
  
  # prepare the plot region
  par(mar=rep(0.1,4))
  plot(NA, xlim=c(0, max(pd$nodes$x)+1), ylim=c(0, max(pd$nodes$y)),
       main=NA, xlab=NA, ylab=NA, xaxt='n', yaxt='n', bty='n')
  segments(pd$edges$x0, pd$edges$y0, pd$edges$x1, pd$edges$y1)
}

points.phyloData <- function(pd, ...) {
  
}


axis.phyloData <- function(pd, ...) {
  
}
