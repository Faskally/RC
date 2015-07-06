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
#' @return matrix
#' @examples
#' # make a simple river netork
#' M <- rbind(c( 1, -1,  0,  0,  0,  0,  0,  0),
#'            c( 0,  1, -1,  0,  0,  0,  0,  0),
#'            c( 0,  1,  0, -1,  0,  0,  0,  0),
#'            c( 0,  0,  1,  0, -1,  0,  0,  0),
#'            c( 0,  0,  1,  0,  0, -1,  0,  0),
#'            c( 0,  0,  0,  1,  0,  0, -1,  0),
#'            c( 0,  0,  0,  1,  0,  0,  0, -1))
#' A <- -1 * t(M) %*% M
#' diag(A) <- 0
#' g <- graph.adjacency(A, mode = "undirected")
#' # plot graph
#' plot(g)
#' # add a node in between all other nodes
#' g <- add.node(g, c(1,2,2,3,3,4,4), c(2,3,4,5,6,7,8))
#' # get precision matrix for river network
#' Q <- getQgraph(g)
#' # simulate river network effect
#' x <- simQ(Q)
#' # simulate observations, 1 per region in this case 
#' y <- x + rnorm(length(x))*0.5
#' # get node ids for observations
#' # note node ID should not be character
#' # it can either be numeric, or a factor.
#' nid <- factor(rownames(Q))
#' # collect data in a list
#' dat <- data.frame(y = y, nid = nid)
#' # provide rank to avoid calculating this at every fit
#' xtr <- list(penalty = Q, rank = nrow(Q) - 1)
#' # plot simulation
#' breaks <- seq(min(y)-0.001, max(y)+0.001, length = 11)
#' par(mfrow = c(1,2))
#' plot(g, vertex.color = heat.colors(length(breaks)-1)[as.numeric(cut(x, breaks))])
#' # fit a model
#' g1 <- gam(y ~ s(nid, bs = "gmrf", xt = xtr), method="REML", data = dat)
#' summary(g1)
#' # plot fitted values
#' plot(g, vertex.color = heat.colors(length(breaks)-1)[as.numeric(cut(fitted(g1), breaks))])
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

