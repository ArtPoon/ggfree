# WORK IN PROGRESS

#' layout.rect
#' 
#' Generate the coordinates for a rectangular layout of an ape:phylo
#' object.
#' 
#' @param phy:  an S3 object of class `phylo``
#' @param unscaled:  if TRUE, return a cladogram layout
#' 
#' @return S3 object of class `phyloLayout`
#' @export
layout.rect <- function(phy, unscaled=FALSE) {
  warning("Under development")
  n <- length(phy$tip.label)
  
}


#' layout.radial
#' 
#' Generate the coordinates for a radial layout of a phylogenetic tree.
#' 
#' @param phy:  an S3 object of class `phylo`
#' 
#' @return S3 object of class `phyloLayout`
#' @export
layout.radial <- function(phy) {
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
#'  @export 
layout.equalangle <- function(phy) {
  warning("Under development")
}
