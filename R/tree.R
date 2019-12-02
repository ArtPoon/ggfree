# WORK IN PROGRESS

# to generate a tree
require(twt)
path <- system.file('extdata', 'structSI.yaml', package='twt')
settings <- yaml.load_file(path)

set.seed(1)
run <- sim.outer.tree(Model$new(settings))
eventlog <- sim.inner.tree(run)
structSI <- as.phylo(eventlog, transmissions=TRUE, migrations=TRUE) 


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
  if ( !is.element('phylo', class(phy)) ) {
    stop("tree.layout() requires `phy` to be an ape:phylo object.")
  }
  # automatically generate node labels if necessary
  if (is.null(phy$node.label)) {
    phy$node.label <- paste("Node", 1:Nnode(phy), sep='')
  }
  
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
  # generate subtrees
  subs <- subtrees(phy)
  names(subs) <- phy$node.label
  
  # convert phylo object to data frames
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

  # map node coordinates to edges
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
.layout.radial <- function(phy, unscaled) {
  pd <- as.phyloData(phy, unscaled)
  
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
.layout.equalangle <- function(phy, unscaled) {
  if (is.rooted(phy)) {
    phy <- unroot(phy)
  }
  pd <- as.phyloData(phy, unscaled)
  
  # allocate new node attributes
  pd$nodes$start <- NA
  pd$nodes$end <- NA
  pd$nodes$angle <- NA
  pd$nodes$r <- pd$nodes$x  # depth becomes radius
  pd$nodes$x <- NA
  pd$nodes$y <- NA
  
  # initialize at root
  root <- unique(pd$edges$parent[which(
    !is.element(pd$edges$parent, pd$edges$child)
    )])
  if (length(root) != 1) {
    stop("Failed to locate root in .layout.equalangle")
  }

  pd$nodes$start[root] <- 0.
  pd$nodes$end[root] <- 2  # PI
  pd$nodes$angle[root] <- 0.  # arbitrary
  pd$nodes$x = 0;  # map to origin
  pd$nodes$y = 0;
  
  pd <- .recursive.equalAngles(root, pd)
  
  # map node coordinates to edges
  pd$edges$x0 <- pd$nodes$x[pd$edges$parent]
  pd$edges$x1 <- pd$nodes$x[pd$edges$child]
  pd$edges$y0 <- pd$nodes$y[pd$edges$parent]  
  pd$edges$y1 <- pd$nodes$y[pd$edges$child]  
  
  class(pd) <- c('phyloLayout', 'phyloData')
  pd$layout <- 'equal.angle'
  
  pd
}


.recursive.equalAngles <- function(index, pd) {
  edges <- pd$edges
  node <- pd$nodes[index, ]
  
  last.start <- node$start
  children <- edges$child[edges$parent==index]
  for (i in children) {
    child <- pd$nodes[i, ]
    
    # assign proportion of arc to this child
    arc <- (node$end - node$start) * max(child$n.tips, 1)/node$n.tips
    child$start <- last.start
    child$end <- child$start + arc
    
    # bisect the arc
    child$angle <- child$start + (child$end - child$start)/2.
    last.start <- child$end
    
    # map to x,y coordinates
    child$x <- node$x + child$r * sin(child$angle*pi)
    child$y <- node$y + child$r * cos(child$angle*pi)
    
    # save to data frame
    pd$nodes[i, ] <- child
    
    # climb up tree
    pd <- .recursive.equalAngles(i, pd)
  }
  
  return(pd)
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
#' Graphical parameters such as `col` or `lwd` can take either a single value 
#' or a vector to specify custom values for each edge.  The ordering should
#' be the same as the original tree (phylo) object, which defaults to 
#' preorder traversal.
#' 
#' @param obj:  S3 object of class `phyloLayout`
#' @param col:  colour for line segments.  Defaults to 'grey50'.
#' @param lwd:  stroke width for line segments.  Defaults to 2.
#' @param label:  Specifies whether nodes are labeled with text.
#'   \itemize{
#'     \item {'n'}{No node labels.}
#'     \item {'t'}{Tip labels only (default).}
#'     \item {'i'}{Internal node labels only.}
#'     \item {'b'}{Both tip and internal node labels.}
#'   }
#' @param cex.lab:  Character expansion factor for node labels (default 0.8).
#' @param mar:  (optional) vector of margin widths (graphical parameter).
#' 
#' @examples 
#' require(ape)
#' phy <- rcoal(20)
#' layout <- tree.layout(phy, type='r')
#' plot(layout)
#' @export
plot.phyloLayout <- function(obj, col='grey50', lwd=2, label='t', cex.lab=0.8, 
                             mar=NA, ...) {
  # check inputs
  if (!is.element('phyloLayout', class(obj))) {
    stop("Argument `obj` must be S3 object of class `phyloData`")
  }
  if (is.null(obj$nodes$y)) {
    stop("You have to apply one of the layout functions to the `phyloData` ",
         "object before plotting! e.g., `layout.rect()`")
  }
  
  if (is.element(obj$layout, c('slanted', 'rectangular'))) {
    if (any(is.na(mar))) {
      # default margins for rectangular layouts
      par(mar=c(2,1,1,5))
    } else {
      par(mar=mar)
    }
    
    # prepare the plot region
    plot(NA, xlim=c(0, max(obj$nodes$x)), ylim=c(0, max(obj$nodes$y)+1),
         main=NA, xlab=NA, ylab=NA, xaxt='n', yaxt='n', bty='n')
    
    segments(x0=obj$edges$x0, y0=obj$edges$y0, 
             x1=obj$edges$x1, y1=obj$edges$y1,
             lwd=lwd, col=col)
    
    # draw vertical edges?
    if (obj$layout=='rectangular') {
      . <- sapply(split(obj$edges, obj$edges$parent), function(e) {
        x0 <- e[1,]$x0
        y0 <- min(e$y0)
        y1 <- max(e$y0)
        # FIXME: what if `col` is a vector?
        segments(x0=x0, y0=y0, x1=x0, y1=y1, 
                 lwd=ifelse(length(lwd)==1, lwd, 1), 
                 col=col)
      })
    }
    
    # node labeling
    if (label != 'n') {
      par(xpd=NA)
      x.space <- max(obj$nodes$x) * 0.01
      if (is.element(label, c('t', 'b'))) {
        # draw tip labels
        tips <- obj$nodes[obj$nodes$n.tips==0, ]
        text(x=tips$x + x.space, y=tips$y, labels=tips$label, 
             adj=0, cex=cex.lab)
      }
      if (is.element(label, c('i', 'b'))) {
        # draw internal labels
        internals <- obj$nodes[obj$nodes$n.tips>0, ]
        text(x=internals$x + x.space, y=internals$y, labels=internals$label, 
             adj=0, cex=cex.lab)
      }
      par(xpd=FALSE)
    }
  }
  
  ##############################################
  
  else if (obj$layout == 'radial') {
    plot(NA, xlim=c(0, max(obj$nodes$x)), ylim=c(0, max(obj$nodes$y)+1),
         main=NA, xlab=NA, ylab=NA, xaxt='n', yaxt='n', bty='n')
    
    segments(obj$edges$x0, obj$edges$y0, obj$edges$x1, obj$edges$y1)
  }
  
  ##############################################
  
  else if (obj$layout == 'equal.angle') {
    if (any(is.na(mar))) {
      # default for unrooted layout
      par(mar=rep(2, 4))
    } else {
      par(mar=mar)
    }
    
    plot(NA, xlim=range(obj$nodes$x), ylim=range(obj$nodes$y),
         main=NA, xlab=NA, ylab=NA, xaxt='n', yaxt='n', bty='n')
    
    segments(x0=obj$edges$x0, y0=obj$edges$y0, 
             x1=obj$edges$x1, y1=obj$edges$y1,
             lwd=lwd, col=col)
  
    # node labeling
    if (label != 'n') {
      par(xpd=NA)
      if (is.element(label, c('t', 'b'))) {
        # draw tip labels
        tips <- obj$nodes[obj$nodes$n.tips==0, ]
        for (i in 1:nrow(tips)) {
          tip <- tips[i, ]
          text(x=tip$x, y=tip$y, labels=tip$label, 
               srt=-tip$angle*180+90,  # radians to degrees
               adj=0, cex=cex.lab)
        }
      }
      if (is.element(label, c('i', 'b'))) {
        # draw internal labels
        internals <- obj$nodes[obj$nodes$n.tips>0, ]
        for (i in 1:nrow(internals)) {
          node <- internals[i, ]
          text(x=node$x, y=node$y, labels=node$label, 
               srt=-node$angle*180+90,
               adj=0, cex=cex.lab) 
        }
      }
      par(xpd=FALSE)
    }
  }
}


#' points.phyloData
#' 
#' A generic function to add points to the plot.  This is 
#' customized for phyloLayout S3 objects that provide the x-
#' and y- coordinates for tips and internal nodes.
#' 
#' @param obj:  S3 object of class `phyloLayout`
#' @param ...:  additional graphical parameters to pass to 
#'        points()
#'
#' @examples
#' # the structSI `phylo` object was generated by simulating
#' # an epidemic within a structured population
#' l <- tree.layout(structSI, type='r') 
#' plot(l)
#' 
#' # highlight nodes that represent transmission or migration events
#' cex <- as.integer(structSI$event.type %in% c('transmission', 'migration'))
#' bg <- ifelse(structSI$event.type=='transmission', 'dodgerblue', 'salmon')
#' points(l, cex=cex, pch=21, bg=bg)
#' 
#' @export
points.phyloLayout <- function(obj, ...) {
  points(x=obj$nodes$x, y=obj$nodes$y, ...)
}


axis.phyloData <- function(obj, ...) {
  
}

scale.bar <- function(obj, ...) {
  
}


#' unroot
#' 
unroot <- function(phy) {
  # make copy to avoid altering original tree
  obj <- phy
  
  # remove node and edge attributes associated with the root node
  if (is.rooted(obj)) {
    n.nodes <- Ntip(obj) + Nnode(obj)
    n.edges <- nrow(obj$edge)
    
    root <- unique(obj$edge[,1][which(
      !is.element(obj$edge[,1], obj$edge[,2])
      )])
    if (length(root) > 1) {
      stop("Error: found multiple root nodes in tree")
    }
    root.edges <- which(obj$edge[,1] == root)
    
    default.keys <- c('tip.label', 'node.label', 'Nnode', 'edge', 'edge.length')
    for (key in names(obj)) {
      if (!is.element(key, default.keys)) {
        val <- obj[[key]]
        if (length(val) == n.nodes) {
          # filter node attribute associated with root
          obj[[key]] <- val[-root]
        }
        else if (length(val) == n.edges) {
          # filter edge attributes associated with root
          obj[[key]] <- val(-root.edges)
        }
      }
    }
  }
  
  unroot.phylo(obj)
}




