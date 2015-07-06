# -------------------------------------------------------
#
#  Functions to build river networks
#
# -------------------------------------------------------

# ----------------------------------------
# some utility functions
# ----------------------------------------


#' Add a new node in between two nodes in an igraph graph
#'
#' This increases the size of the graph by the number of new nodes
#'
#' Description - This function does stuff.
#'
#' @param g an igraph graph
#' @param from a vector of node IDs
#' @param to a vector of node IDs
#' @return an igraph graph
#' @export
add.node <- function(g, from, to) {
  nv <- length(from)
  newv <- 1:nv + vcount(g)
  g <- add.vertices(g, nv = nv)
  g[from,to] <- FALSE
  newfrom <- c(rbind(from, newv))
  newto <- c(rbind(newv, to))
  g[from = newfrom, to = newto] <- TRUE
  g
}






#' Create an igraph graph from a spatial lines data.frame network
#'
#' Details
#'
#' Description - This function does stuff.
#'
#' @param lines a spatial lines data.frame containing a river network
#' @param from the column name containing the 'from' node IDs
#' @param to the column name containing the 'to' node IDs
#' @return what does it return
#' @export
buildTopo <- function(lines, from = "FNODE_", to = "TNODE_") {
  # need to strip out all the digitised points, leaving only the nodes

  # from and to vertices
  from.orig <- lines @ data[[from]]
  to.orig <- lines @ data[[to]]

  # edges
  ne <- length(from.orig)

  # vertices
  vnames <- paste(sort(unique(c(from.orig, to.orig))))
  nv <- length(vnames)

  # edges using renamed vertices
  from.new <- as.numeric(factor(paste(from.orig), levels = vnames))
  to.new <- as.numeric(factor(paste(to.orig), levels = vnames))

  # construct graph
  graph <- graph.edgelist(cbind(from.new, to.new), directed = FALSE)

  # add in locations to vertices
  V(graph)$x = as.numeric(substring(vnames, 1, 7))
  V(graph)$y = as.numeric(substring(vnames, 8, 14))

  # add length to edges ?
  E(graph)$length <- lines $ LENGTH

  # name vertices?
  #V(graph)$name <- paste(1:length(V(graph)$name))

  # add in some default plotting options
  V(graph) $ frame.color <- NA
  V(graph) $ size <- 2.5
  V(graph) $ label <- NA

  return(graph)
}
