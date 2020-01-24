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
  
  if (is.null(phy$node.label)) {
    phy$node.label <- paste("Node", 1:Nnode(phy), sep='')
  }
  if (is.null(phy$edge.length)) {
    unscaled <- TRUE
    phy$edge.length <- rep(NA, nrow(phy$edge))
  }
  
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
#'   \item{"o"}{Circular (radial) layout}
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

  if (type=='r') {
    # rectangular
    .layout.rect(phy, unscaled, slanted=FALSE)
  }
  else if (type=='s') {
    # slanted
    .layout.rect(phy, unscaled, slanted=TRUE)
  }
  else if (type=='u') {
    # unrooted
    .layout.equalangle(phy, unscaled)
  }
  else if (type == 'o') {
    # circular (radial)
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
  # convert phylo object to data frames
  pd <- as.phyloData(phy, unscaled)
  
  tips <- pd$edges$child[pd$edges$isTip]
  pd$nodes$y <- rep(NA, nrow(pd$nodes))
  pd$nodes$y[tips] <- 1:Ntip(phy)
  
  # assign vertical positions to internal nodes
  internals <- pd$edges$child[!pd$edges$isTip]
  for (i in rev(internals)) {
    children <- pd$edges$child[pd$edges$parent==i]
    pd$nodes$y[i] <- mean(pd$nodes$y[children])
  }
  root <- Ntip(phy)+1
  children <- pd$edges$child[pd$edges$parent==root]
  pd$nodes$y[root] <- mean(pd$nodes$y[children])

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
#' Generate the coordinates for a radial layout of a phylogenetic tree
#' within a circular region.
#' 
#' @param phy:  an S3 object of class `phylo`
#' 
#' @return S3 object of class `phyloLayout`
#' @keywords internal
.layout.radial <- function(phy, unscaled) {
  # convert phylo object to data frames
  pd <- as.phyloData(phy, unscaled)
  pd$nodes$r <- pd$nodes$x  # reassign depth to radius
  
  tips <- pd$edges$child[pd$edges$isTip]
  pd$nodes$angle <- rep(NA, nrow(pd$nodes))
  pd$nodes$angle[tips] <- (1:Ntip(phy)) / Ntip(phy) * 2 * pi
  
  # assign angles to internal nodes
  internals <- pd$edges$child[!pd$edges$isTip]
  for (i in rev(internals)) {
    children <- pd$edges$child[pd$edges$parent==i]
    pd$nodes$angle[i] <- mean(pd$nodes$angle[children])
  }
  root <- Ntip(phy)+1
  children <- pd$edges$child[pd$edges$parent==root]
  pd$nodes$angle[root] <- mean(pd$nodes$angle[children])
  
  # calculate x,y from polar coordinates
  pd$nodes$x <- pd$nodes$r * cos(pd$nodes$angle)
  pd$nodes$y <- pd$nodes$r * sin(pd$nodes$angle)
  
  # map node coordinates to edges
  pd$edges$x1 <- pd$nodes$x[pd$edges$child]
  pd$edges$y1 <- pd$nodes$y[pd$edges$child]
  
  pd$edges$x0 <- pd$nodes$r[pd$edges$parent] * cos(pd$nodes$angle[pd$edges$child])
  pd$edges$y0 <- pd$nodes$r[pd$edges$parent] * sin(pd$nodes$angle[pd$edges$child])
  
  pd$layout <- 'radial'
  class(pd) <- c('phyloLayout', 'phyloData')
  pd
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
    # unrooting destroys internal node labels
    phy$node.label <- paste("Node", 1:Nnode(phy), sep='')
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
  pd$nodes$end[root] <- 2*pi
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


#' .recursive.equalAngles
#' 
#' A recursive function for calculating Felsenstein's equal-angles algorithm
#' for unrooted tree layouts.
#' 
#' @param index:  row number of current node in data frame
#' @param pd:  S3 object of class `phyloData``
#' 
#' @return `phyloData` object with updated `nodes` data frame
#' 
#' @keywords internal
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
    child$x <- node$x + child$r * sin(child$angle)
    child$y <- node$y + child$r * cos(child$angle)
    
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
#' @param type:  if 'n', then line segments are not drawn
#' @param col:  colour for line segments.  Defaults to 'grey50'.
#' @param lwd:  stroke width for line segments.  Defaults to 2.
#' @param label:  Specifies whether nodes are labeled with text.
#'   \itemize{
#'     \item{'n'}{No node labels.}
#'     \item{'t'}{Tip labels only (default).}
#'     \item{'i'}{Internal node labels only.}
#'     \item{'b'}{Both tip and internal node labels.}
#'   }
#' @param cex.lab:  Character expansion factor for node labels (default 0.8).
#' @param mar:  (optional) vector of margin widths (graphical parameter).
#' @param ...:  additional graphical parameters to pass to `lines` and `label`
#' 
#' @examples 
#' # generate random coalescent tree
#' set.seed(1999); phy <- rcoal(50)
#' 
#' # rectangular tree
#' plot(tree.layout(phy, type='r'), mar=c(3,0,0,1))
#' axis(side=1)
#' 
#' # unscaled slanted tree with colors
#' pal <- colorRampPalette(c('cadetblue', 'brown'))
#' plot(tree.layout(phy, type='s', unscaled=T), col=pal(5))
#' 
#' # unrooted tree
#' plot(tree.layout(phy, type='u'))
#' 
#' # circular (radial) tree
#' plot(tree.layout(phy, type='o'), label='t')
#' 
#' @export
plot.phyloLayout <- function(obj, type='l', col='grey50', lwd=2, label='t', cex.lab=0.8, 
                             mar=NA, ...) {
  # check inputs
  if (!is.element('phyloLayout', class(obj))) {
    stop("Argument `obj` must be S3 object of class `phyloData`")
  }
  if (is.null(obj$nodes$y)) {
    stop("You have to apply one of the layout functions to the `phyloData` ",
         "object before plotting! e.g., `layout.rect()`")
  }
  
  # prepare the plot region
  if (is.element(obj$layout, c('slanted', 'rectangular'))) {
    if (any(is.na(mar))) {
      par(mar=c(2,1,1,5))  # default margins
    } else {
      par(mar=mar)
    }
    plot(NA, xlim=c(0, max(obj$nodes$x)), ylim=c(0, max(obj$nodes$y)+1),
         main=NA, xlab=NA, ylab=NA, xaxt='n', yaxt='n', bty='n')
  }
  else if (obj$layout=='radial' | obj$layout == 'equal.angle') {
    if (any(is.na(mar))) {
      # default for radial layout
      par(mar=rep(2, 4))
    } else {
      par(mar=mar)
    }
    plot(NA, xlim=range(obj$nodes$x), ylim=range(obj$nodes$y),
         main=NA, xlab=NA, ylab=NA, xaxt='n', yaxt='n', bty='n')
  }
  
  if (type != 'n') {
    # draw line segments
    suppressWarnings({
      lines(obj, col=col, lwd=lwd, ...)  
    })
  }
  
  if (label != 'n') {
    # draw node labels
    suppressWarnings({
      text.phyloLayout(obj, label=label, cex.lab=cex.lab, ...)
    })
  }
}


#' lines.phyloLayout
#' 
#' Generic S3 method adapted for drawing the line segments that 
#' comprise a phylogenetic tree.
#' 
#' @param obj:  an S3 object of class `phyloLayout`
#' @param col:  if a colour vector is specified, `lines` will apply
#'        the colors in the same order as edges in the data frame 
#'        in `obj`.  If the vector is too short, it will recycle colors.
#' @param shade:  if TRUE (default), vertical edges will be coloured 
#'        by the average of descendant horizonatal edges.  If FALSE,
#'        vertical edges are uniformly coloured grey.
#' @param ...:  additional graphical parameters to pass to `segments`
#'        and `draw.arc`
#' 
#' @export
lines.phyloLayout <- function(obj, col='grey50', shade=TRUE, ...) {
  if (length(col) < nrow(obj$edges)) {
    # recycle colours if necessary
    col <- rep(col, length.out=nrow(obj$edges))
  }
  
  # this applies for all layouts
  segments(x0=obj$edges$x0, y0=obj$edges$y0, 
           x1=obj$edges$x1, y1=obj$edges$y1, col=col, ...)
  
  # group edges by parent node
  temp <- split(obj$edges, obj$edges$parent)
  
  # draw vertical edges for rect or radial layouts
  for (e in temp) {
    
    # determine edge color
    if (shade) {
      pal <- col[as.integer(row.names(e))]
      this.col <- blend.colors(pal)
    }
    else {
      this.col <- 'grey50'
    }
    
    if (obj$layout == 'rectangular') {
      
      x0 <- e[1,]$x0  # should be the same for all entries
      y0 <- min(e$y0)
      y1 <- max(e$y0)

      segments(x0=x0, y0=y0, x1=x0, y1=y1, col=this.col, ...)
    }
    else if (obj$layout == 'radial') {
      nodes <- obj$nodes[e$child,]
      parent <- obj$nodes[unique(e$parent),]
      start <- min(nodes$angle)
      end <- max(nodes$angle)
        
      draw.arc(x=0, y=0, theta0=start, theta1=end, r0=parent$r, 
               col=this.col, ...)
    }
  }
}


#' text.phyloLayout
#' 
#' Draw tip and/or internal node labels on the phylogenetic tree.
#' 
#' @param obj:  an S3 object of class `phyloLayout`
#' @param label:  Specifies which set of nodes are labeled with text.
#'   \itemize{
#'     \item{'t'}{Tip labels only (default).}
#'     \item{'i'}{Internal node labels only.}
#'     \item{'b'}{Both tip and internal node labels.}
#'   }
#' @param align:  if TRUE, all tip labels are drawn at maximum value
#' @param cex.lab:  character expansion factor for text
#' @param ...:  additional graphical parameters passed to `text`
#' 
#' @export
text.phyloLayout <- function(obj, label='t', align=FALSE, cex.lab=1, offset=0, ...) {
  
  # filter node data frame
  tips <- obj$nodes[obj$nodes$n.tips==0, ]
  internals <- obj$nodes[obj$nodes$n.tips>0, ]
  
  # allow drawing into margins of plot device
  par(xpd=NA)
  
  if (obj$layout=='rectangular' | obj$layout=='slanted') {
    if (is.element(label, c('t', 'b'))) {
      # draw tip labels
      x <- tips$x + offset
      if (align) { x <- max(tips$x) + offset }
      text(x=x, y=tips$y, labels=paste0(' ', tips$label), 
           adj=0, cex=cex.lab, ...)
    }
    if (is.element(label, c('i', 'b'))) {
      # draw internal labels
      text(x=internals$x, y=internals$y, labels=paste0(' ', internals$label), 
           adj=0, cex=cex.lab, ...)
    }
  }
  else if (obj$layout == 'radial' | obj$layout == 'equal.angle') {
    if (is.element(label, c('t', 'b'))) {
      # srt does not work for vectors
      for (i in 1:nrow(tips)) {
        tip <- tips[i, ]
        
        # equal angle layout draws zero-angle straight up
        if (obj$layout == 'equal.angle') tip$angle <- pi/2-tip$angle
        tip <- .rotate.label(tip, offset)
        
        text(x=tip$x, y=tip$y, labels=tip$label, 
             srt=tip$angle/pi*180, adj=as.integer(tip$rotated), 
             cex=cex.lab, ...)
      }
    }
    if (is.element(label, c('i', 'b'))) {
      # draw internal labels
      for (i in 1:nrow(internals)) {
        node <- internals[i, ]
        
        if (obj$layout == 'equal.angle') node$angle <- pi/2-node$angle
        node <- .rotate.label(node, offset)
        
        text(x=node$x, y=node$y, labels=node$label, 
             srt=node$angle/pi*180, adj=as.integer(node$rotated), 
             cex=cex.lab, ...) 
      }
    }
  }

  # revert to default trimming at margins    
  par(xpd=FALSE)
}


#' .rotate.label
#' Adjust the string rotation of node labels for unrooted
#' or radial trees so they are not upside-down.
#' 
#' @param node:  named vector, a row from the nodes data frame of a 
#'        `phyloLayout` object.
#' @param offset:  amount to push label outward from origin
#' 
#' @return  a named vector with updated `x`, `y`, `angle` and `label` values
#' 
#' @keywords internal
.rotate.label <- function(node, offset) {
  # slide label outward
  node$x <- node$x + offset*cos(node$angle)
  node$y <- node$y + offset*sin(node$angle)
  
  h <- node$angle %% (2*pi)
  if (h>0.5*pi && h<(1.5*pi)) {
    # invert the label
    node$angle <- node$angle+pi
    # pad the label on the right
    node$label <- paste0(node$label, ' ')
    node$rotated <- TRUE
  }
  else {
    # pad the label on the left
    node$label <- paste0(' ', node$label)
    node$rotated <- FALSE
  }
  node
}


#' points.phyloLayout
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



#' add.scalebar
#' Add a scale bar to the plot.
#' @param obj:  `phyloLayout` S3 object generated by `tree.layout`
#' @param len:  length of scale bar to draw (defaults to 1)
#' @param x0:  (optional) override default horizontal placement of bar
#' @param y0:  (optional) override default vertical placement of bar.
#'        Note both x and y need to be specified to override the default.
#' @param ...:  additional graphical parameters to pass to `segments`, 
#'        such as `cex`, `col` and `lwd`.
#' 
#' @examples 
#' require(ape)
#' phy <- rcoal(30)
#' Y <- tree.layout(phy, type='r')
#' plot(Y)
#' add.scalebar(Y, len=1)
#' @export
add.scalebar <- function(obj, len=1, x0=NA, y0=NA, lwd=2, ...) {
  if (is.na(x0) | is.na(y0)) {
    # use default location
    mid.pt <- mean(range(obj$nodes$x))
    x0 <- mid.pt - len/2
    x1 <- mid.pt + len/2
    y0 <- min(obj$nodes$y) - 1
  } else {
    x1 <- x0+len
  }
  suppressWarnings({
    segments(x0, y0, x1, lwd=lwd, ...)
    text(x=mean(c(x0, x1)), y=y0-.7, label=len, ...)
  })
}


#' unroot
#' 
#' If the `phylo` object contains non-default attributes for nodes
#' or edges, then unrooting breaks the alignment between these 
#' attributes and the tree. 
#' 
#' @export
unroot <- function(obj) {
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


#' draw.guidelines
#' 
#' Draws segments from the tips of the tree out to the furthest
#' tip label.  This should only be used in conjunction with 
#' text.phyloLayout(align=TRUE), or else the segments will be drawn
#' over the tip labels.
#' 
#' @param obj:  S3 object of class `phyloLayout`
#' @param lty:  line style, defaults to 3 (dotted)
#' @param ...:  other graphical parameters to pass to `segments`,
#'        such as `col` or `lwd`
#'        
#' @export
draw.guidelines <- function(obj, lty=3, ...) {
  # right ends of terminal edges
  x0 <- obj$edges$x1[obj$edges$isTip]
  y0 <- obj$edges$y0[obj$edges$isTip]
  
  # map colours from edges to nodes
  index <- obj$edges$child[obj$edges$isTip]
  suppressWarnings(segments(x0=x0, x1=max(x0), y0=y0, lty=lty, ...))
}


#' image
#' 
#' Generic function for drawing a grid of coloured or grey-scale 
#' rectangles corresponding to values in a matrix `z`.  Rows in 
#' the matrix are assumed to correspond to tips of the tree.
#' 
#' @param obj:  an S3 object of class `phyloLayout`
#' @param z:  matrix, data to annotate tips in order of *nodes*
#' @param xlim:  horizontal range of grid relative to current plot device.
#'        Note this function will call `xpd=NA` to permit drawing 
#'        in margins.
#' @param col:  a vector of colours that maps to factor levels in `z`
#' @param border:  colour for border of rectangles in grid
#' @param xaxt:  if 'n', suppress drawing of axis and labels
#' 
#' @export
image.phyloLayout <- function(obj, z, xlim, col=NA, border='white', xaxt='y', ...) {
  
  # recode contents of `z` as integer-valued matrix
  z <- apply(z, 2, function(x) as.integer(as.factor(x)))
  
  # use default colors if not specified by user
  if (any(is.na(col))) {
    col <- colorRampPalette(c('firebrick', 'dodgerblue'))(max(z, na.rm=T))
  }
  
  #tips <- obj$edges[obj$edges$isTip, ]
  tips <- obj$nodes[obj$nodes$n.tips==0, ]
  
  # check that z has the expected dimensions
  if (nrow(z) != nrow(tips)) {
    stop("Number of rows in matrix `z` does not match number of tips ",
         "in `phyloLayout` object: ", nrow(z), "!=", nrow(tips))
  }
  
  # draw the image
  par(xpd=NA)
  
  if (obj$layout == 'rectangular' | obj$layout == 'slanted') {
    y <- tips$y
    x <- seq(xlim[1], xlim[2], length.out=ncol(z)+1)
    dx <- (x[2]-x[1])/2
    
    for (j in 1:ncol(z)) {
      for (i in 1:nrow(z)) {
        val <- z[i,j]
        if (is.na(val)) next
        rect(xleft = x[j], xright = x[j+1], 
             ybottom = y[i]-0.5, ytop = y[i]+0.5,
             border = border, col = col[val])
      }
    }
    
    # draw axis (override masking of labels)
    if (xaxt != 'n') {
      odds <- seq(1, ncol(z), 2)
      evens <- odds+1
      suppressWarnings({
        axis(side=1, at=x[1:ncol(z)]+dx, labels=NA, ...)
        axis(side=1, at=x[odds]+dx, labels=names(geno)[odds], lwd=0, ...)
        axis(side=1, at=x[evens]+dx, labels=names(geno)[evens], lwd=0, ...)
      })
    }
  }
  else if (obj$layout == 'radial') {
    angles <- tips$angle
    d.theta <- pi/nrow(tips)
    r <- seq(xlim[1], xlim[2], length.out=ncol(z)+1)
    dr <- (r[2]-r[1])/2
    
    for (j in 1:ncol(z)) {
      for (i in 1:nrow(z)) {
        val <- z[i,j]
        if (is.na(val)) next
        draw.arc(x = 0, y = 0, theta0 = angles[i]-d.theta, 
                 theta1 = angles[i]+d.theta, r0 = r[j], r1 = r[j+1],
                 col = col[val], border = border)
      }
    }
  }
  par(xpd=FALSE)
}




