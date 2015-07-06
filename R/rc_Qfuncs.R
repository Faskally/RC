# -------------------------------------------------------
#
#  Functions to create simple gmrfs
#
# -------------------------------------------------------


# -------------------------------
# first the difference functions
# -------------------------------

#' Differences defining a river network
#'
#' Returns the incidence matrix of a graph
#'
#' Description - This function does stuff.
#'
#' @param g an igraph graph specifying the dependence structure
#' @return a Matrix with a column for each node and a row for each connection / edge
#' @export
Dgraph <- function(g) {
  # still to do this... is it the graph laplacian?
  NULL
}


# -------------------------------
# Now the precision functions
# -------------------------------


#' Compute RW1 precision matrix on a river network
#'
#' Returns the laplacian matrix of a graph.  If weights are supplied
#' then the weighted lacplacian is returned.  Sensible weights are
#' based on the flows of the incoming upstream graph edges.
#'
#' Weights argument is passed onto the graph.laplacian function from
#' the igraph package. If weights is NULL (default) and the graph has 
#' an edge attribute called weight, then it will be used automatically. 
#' Set this to NA if you want the unweighted Laplacian on a graph that 
#' has a weight edge attribute.
#'
#' @param g an igraph graph specifying the dependence structure
#' @param weights weights to be applied to the edges of the graph (see details)
#' @return what does it return
#' @export
getQgraph <- function(g, weights = NULL) {

  # is this a connected graph?
  if (!is.connected(g)) {
    stop("there are some unconnected vertices...")
  }

  Q <- graph.laplacian(g, weights = weights, sparse = FALSE)
  rownames(Q) <- colnames(Q) <- 1:nrow(Q)
  Q
}

#' Compute RW1 precision matrix on a river network
#'
#' Returns the laplacian matrix of a graph.  If weights are supplied
#' then the weighted lacplacian is returned.  Sensible weights are
#' based on the flows of the incoming upstream graph edges.
#'
#' Weights argument is passed onto the graph.laplacian function from
#' the igraph package. If weights is NULL (default) and the graph has 
#' an edge attribute called weight, then it will be used automatically. 
#' Set this to NA if you want the unweighted Laplacian on a graph that 
#' has a weight edge attribute.
#'
#' @param g an igraph graph specifying the dependence structure
#' @param weights weights to be applied to the edges of the graph (see details)
#' @return what does it return
#' @export
getWRW1Mat <- function(g, weights = NULL) {

  message("This function is contained for back compatability. Please use 'getQgraph' function instead")

  getQgraph(g, weights = weights)
}

