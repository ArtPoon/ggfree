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
  
  # convert node attributes to data frame
  nodes <- data.frame(
    label = c(phy$tip.label, phy$node.label)
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

  # total branch length from root to node
  if (unscaled) {
    nodes$x <- Ntip(phy)-node.depth(phy)
  } else {
    nodes$x <- node.depth.edgelength(phy)
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
#' lay <- tree.layout(phy, 'r')
#' head(lay$edges)
#' 
#' @export
tree.layout <- function(phy, type='r', unscaled=FALSE) {
  if (type=='r') {
    .layout.rect(phy, unscaled, slanted=FALSE)
  }
  else if (type=='s') {
    .layout.rect(phy, unscaled, slanted=TRUE)
  }
  else if (type=='u') {
    # unrooted
    .layout.equalangle(phy, unscaled)
  }
  else if (type == 'o') {
    .layout.radial(phy, unscaled)
  }
  else {
    stop("Unrecognized layout type '", type, "'")
  }
}


#' print.phyloLayout
#' 
#' Generic function to display object.
#' @param obj:  S3 object of class `phyloLayout`
#' @export
print.phyloLayout <- function(obj) {
  n <- sum(obj$edges$isTip)
  cat("Phylogenetic layout using a", obj$layout, "algorithm\n",
      "with", n, "tips and", nrow(obj$nodes)-n, "internal nodes.\n\n")
  cat(paste0("$nodes (", nrow(obj$nodes)), "nodes with", ncol(obj$nodes), "attributes):\n")
  print(head(obj$nodes))
  cat("\n")
  cat(paste0("$edges (", nrow(obj$edges)), "edges with", ncol(obj$edges), "attributes):\n")
  print(head(obj$edges))
  cat("\n")
}



#' .layout.rect
#' 
#' Algorithm to generate a "rectangular" (left to right) layout of a 
#' phylogenetic tree, with the root at the left and tips on the right.
#' 
#' @keywords internal
.layout.rect <- function(phy, unscaled=FALSE, slanted=FALSE) {
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
    st <- subs[[ as.character(pd$nodes$label[i]) ]]
    y.tips <- pd$nodes$y[which(is.element(pd$nodes$label, st$tip.label))]
    pd$nodes$y[i] <- mean(y.tips)
  }
  root <- Ntip(phy)+1
  pd$nodes$y[root] <- mean(1:Ntip(phy))

  pd$edges$x0 <- pd$nodes$x[pd$edges$parent]
  pd$edges$x1 <- pd$nodes$x[pd$edges$child]
  if (slanted) {
    pd$edges$y0 <- pd$nodes$y[pd$edges$parent]  
  } else {
    pd$edges$y0 <- pd$nodes$y[pd$edges$child]
  }
  pd$edges$y1 <- pd$nodes$y[pd$edges$child]
  
  pd$layout <- ifelse(slanted, 'slanted', 'rectangular')
  pd$postorder <- postorder(phy)
  
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
#' Generic plot function for phylogenetic trees with layouts generated
#' using ggfree.  The coordinate system of the plot depends on the layout
#' algorithm:
#' \itemize {
#'   \item Rectangular/slanted layout: The x-axis is scaled to the height
#'   of the tree (from the root to the furthest tip).  The y-axis is scaled 
#'   to the number of tips (1:Ntip(phy)).
#'   \item Circular (radial) layout:  
#' }
#' 
#' @param layout: S3 object of class `phyloLayout`
#' @param lwd:  (optional) stroke width for line segments.  Either a single value applied
#'        to all branches or a numeric vector in the same order as the edges 
#'        (ape:phylo defaults to preorder traversal).  Defaults to 2.
#' @param label:  If 'n', suppresses drawing of tip and node labels.  If 'b', both 
#'        tip and node labels are drawn.  Defaults to 't' for tip labels only.
#' @param mar:  (optional) vector of margin widths (graphical parameter).
#' 
#' @examples 
#' require(ape)
#' phy <- rcoal(20)
#' layout <- tree.layout(phy, type='r')
#' plot(layout)
#' @export
plot.phyloLayout <- function(obj, col='grey50', lwd=2, label='t', mar=NA, cex.lab=0.8, ...) {
  # check inputs
  if (!is.element('phyloLayout', class(obj))) {
    stop("Argument `obj` must be S3 object of class `phyloData`")
  }
  if (is.null(obj$nodes$y)) {
    stop("You have to apply one of the layout functions to the `phyloData` ",
         "object before plotting! e.g., `layout.rect()`")
  }
  
  if (is.element(obj$layout, c('slanted', 'rectangular'))) {
    if (is.na(mar)) {
      # default margins for rectangular layouts
      par(mar=c(2,1,1,5))
    }
    # prepare the plot region
    plot(NA, xlim=c(0, max(obj$nodes$x)), ylim=c(0, max(obj$nodes$y)+1),
         main=NA, xlab=NA, ylab=NA, xaxt='n', yaxt='n', bty='n')
    
    segments(x0=obj$edges$x0, y0=obj$edges$y0, 
             x1=obj$edges$x1, y1=obj$edges$y1,
             lwd=lwd, col=col)
    
    # draw vertical edges?
    if (obj$layout=='rectangular') {
      # reorder edges by postorder traversal (grouped by parent)
      edges <- obj$edges[obj$postorder, ]
      for (i in seq(1, nrow(edges), 2)) {
        # If user specifies variable line widths for edges, then vertical 
        # edges should have an arbitrary constant width.
        segments(x0=edges[i,]$x0, y0=edges[i,]$y0, 
                 x1=edges[i,]$x0, y1=edges[i+1,]$y0, 
                 lwd=ifelse(length(lwd)==1, lwd, 1),
                 col=col)
      }
    }
    
    if (label != 'n') {
      if (is.element(label, c('t', 'b'))) {
        # draw tip labels
        tips <- obj$nodes[obj$nodes$n.tips==0, ]
        x.space <- max(tips$x) * 0.01
        par(xpd=NA)
        text(x=tips$x+x.space, y=tips$y, labels=tips$label, adj=0, cex=cex.lab)
      }
    }
  }
  
  else if (obj$layout == 'radial') {
    plot(NA, xlim=c(0, max(obj$nodes$x)), ylim=c(0, max(obj$nodes$y)+1),
         main=NA, xlab=NA, ylab=NA, xaxt='n', yaxt='n', bty='n')
    
    segments(obj$edges$x0, obj$edges$y0, obj$edges$x1, obj$edges$y1)
  }
}


points.phyloData <- function(pd, ...) {
  
}


axis.phyloData <- function(pd, ...) {
  
}
