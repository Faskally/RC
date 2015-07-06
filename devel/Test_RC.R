

# set proxy values
httr::set_config(httr::use_proxy(url="192.168.41.8", port=80))

# load packages
devtools::install_github("faskally/gmrf")
library(gmrf)
pkg <- devtools::as.package("C:/work/repos/Faskally/rc")
devtools::load_all(pkg)

example(smooth.construct.gmrf.smooth.spec)

# -----------------------------------------------
#
# lets start with a simple example
#
# -----------------------------------------------

pkg <- devtools::as.package("C:/work/repos/Faskally/rc")
devtools::load_all(pkg)
library(magrittr)


# make a simple river netork
M <- rbind(c( 1, -1,  0,  0,  0,  0,  0,  0),
           c( 0,  1, -1,  0,  0,  0,  0,  0),
           c( 0,  1,  0, -1,  0,  0,  0,  0),
           c( 0,  0,  1,  0, -1,  0,  0,  0),
           c( 0,  0,  1,  0,  0, -1,  0,  0),
           c( 0,  0,  0,  1,  0,  0, -1,  0),
           c( 0,  0,  0,  1,  0,  0,  0, -1))

A <- -1 * t(M) %*% M
diag(A) <- 0
g <- graph.adjacency(A, mode = "undirected")
# plot graph
plot(g)
# add a node in between all other nodes
g <- add.node(g, c(1,2,2,3,3,4,4), c(2,3,4,5,6,7,8))
plot(g)
#get.shortest.paths(g, 1, 5)

# get precision matrix for river network
Q <- getQgraph(g)
Q %>% replace(. == 0, ".") %>% as.table(.)

# simulate river network effect
x <- simQ(Q)

# simulate observations, 1 per region in this case 
y <- x + rnorm(length(x))*0.5

# get node ids for observations
# note node ID should not be character
# it can either be numeric, or a factor.
nid <- factor(rownames(Q))

# collect data in a list
dat <- data.frame(y = y, nid = nid)

# provide rank to avoid calculating this at every fit
xtr <- list(penalty = Q, rank = nrow(Q) - 1)

# plot simulation
breaks <- seq(min(y)-0.001, max(y)+0.001, length = 11)
par(mfrow = c(1,2))
plot(g, vertex.color = heat.colors(length(breaks)-1)[as.numeric(cut(y, breaks))])

# fit a model
# use REML this time
g1 <- gam(y ~ s(nid, bs = "gmrf", xt = xtr), method="REML", data = dat)
summary(g1)

# plot fitted values
plotgraph(g, col = heat.colors(length(breaks)-1)[as.numeric(cut(fitted(g1), breaks))])





# -----------------------------------------------
#
# lets try a river network effect based on a real river
#
# -----------------------------------------------

pkg <- devtools::as.package("C:/work/repos/Faskally/rc")
devtools::load_all(pkg)
library(magrittr)
library(sp)

# -------------------------------------
# get data
# -------------------------------------


# load river esk data
load("C:/work/repos/Faskally/rc/devel/RiverEsk.rData")
plot(esk)

# great a graph of the full river
g <- buildTopo(esk)

# test smfs report code ?
# g <- buildTopo(esk)
# g <- reduceNetwork(g)
# Qrc <- getWRW1Mat(g)
# getRCLocation(xy, g) # this should return a vector of node IDs

# plot graph
plot(g)


# -------------------------------------
# get GMRF
# -------------------------------------

# there are three clusters - non-connected groups of nodes.
# if these were legitimate rivers then we should retain them
# howewer in this case they are unimportant in we can remove them
no.clusters(g)
# can we find them?
cl <- clusters(g)
plot(g, vertex.color = cl $ membership)
# remove the tiny clusters from the graph
g <- delete.vertices(g, which(cl $ membership != 1))
# check
no.clusters(g)

# now get the gmrf
Q <- getQgraph(g)

image(Matrix(Q))


# -------------------------------------
# simulate some data
# -------------------------------------

# spatial effect
x <- simQ(Q)

# simulate observations, 1 per region in this case 
y <- x + rnorm(length(x))*0.5

# get node ids for observations
# note node ID should not be character
# it can either be numeric, or a factor.
nid <- factor(rownames(Q))

# collect data in a list
dat <- data.frame(y = y, nid = nid)

# provide rank to avoid calculating this at every fit
xtr <- list(penalty = Q, rank = nrow(Q) - 1)

# plot simulation
breaks <- seq(min(y)-0.001, max(y)+0.001, length = 11)
par(mfrow = c(1,2))
plot(g, vertex.color = heat.colors(length(breaks)-1)[as.numeric(cut(x, breaks))])

# -------------------------------------
# fit a model
# -------------------------------------

# use REML this time
# takes a while to fit full gmrf
#g1 <- gam(y ~ s(nid, bs = "gmrf", xt = xtr), method="REML", data = dat)
g1 <- gam(y ~ s(nid, bs = "gmrf", xt = xtr, k = 25), method="REML", data = dat)
summary(g1)

# plot fitted values
plot(g, vertex.color = heat.colors(length(breaks)-1)[as.numeric(cut(fitted(g1), breaks))])





# -----------------------------------------------
#
# lets try a river network effect with discontinuities
# i.e. a model with 2 rivers!
#
# -----------------------------------------------

pkg <- devtools::as.package("C:/work/repos/Faskally/rc")
devtools::load_all(pkg)
#library(sp)
#library(magrittr)

 
# -------------------------------------
# get data
# -------------------------------------


# load catchments
ctm <- rgdal::readOGR("P:/vector/nationalgrid/Catchment/SEPA","Baseline_confluence_nested_catchments")
load("C:/work/repos/Faskally/gmrf/devel/ctm_data.rData")
ctm @ data <- ctm_data
hma <- CLdata::hma


# Select a few
hanames <- c("Esk Group", "Deveron Group")
ctm <- ctm[ctm $ HAName %in% hanames,]
# remove "605" so we have a singleton catchment
ctm <- ctm[rownames(ctm@data) != "605",]
hma <- hma[hma $ HAName %in% hanames,]

# -------------------------------------
# get GMRF
# -------------------------------------

nb <- spdep::poly2nb(ctm, queen = FALSE)
Q <- getQnb(nb)

Q %>% replace(. == 0, ".") %>% as.table(.)


# -------------------------------------
# Get constraint martrix and a factor for grouping
# -------------------------------------

constraint <- getCnb(Q)
ctmgrp <- getFactorsnb(Q)

# visualise groupings
plot(ctm, col = ctmgrp)


# -------------------------------------
# simulate some data
# -------------------------------------

# design matrix for catchment group means
X <- model.matrix(~ ctmgrp - 1)
cmu <- c(1, 2, 3) * 2

# spatial effect (in effect catchment within catchment group)
x <- simQ(Q)
# check groups sum to zero
tapply(x, ctmgrp, mean)

# simulate observations, 1 per region in this case 
y <- x + c(X %*% cmu) + rnorm(length(x))*0.5

# get region ids for observations
# note region ID should not be character
# it can either be numeric, or a factor.
cid <- factor(rownames(ctm @ data))

# collect data in a list
dat <- data.frame(y = y, cid = cid, ctmgrp)

# collect smoother details in a list
xt <- list(penalty = Q, constraint = constraint)
# provide rank to avoid calculating this at every fit
xtr <- list(penalty = Q, constraint = constraint, rank = nrow(Q) - nrow(constraint))

# plot simulation
breaks <- seq(min(y)-0.001, max(y)+0.001, length = 11)
plot(ctm, col = heat.colors(length(breaks)-1)[as.numeric(cut(y, breaks))])

# -------------------------------------
# fit a model
# -------------------------------------

# use REML this time
g1 <- gam(y ~ -1 + ctmgrp + s(cid, bs = "gmrf", xt = xt), method="REML", data = dat)
#g1 <- gam(y ~ -1 + ctmgrp + s(cid, bs = "gmrf", xt = xtr), method="REML", data = dat)

summary(g1)
# check groups sum to ctm group means
tapply(fitted(g1), dat$ctmgrp, mean)

# plot fitted values
plot(ctm, col = heat.colors(length(breaks)-1)[as.numeric(cut(fitted(g1), breaks))])



# -----------------------------------------------
#
# lets try a reduced rank spatial effect with discontinuities
#
# -----------------------------------------------

# in this case, the eigen value decomposition used to reduce the
# rank of the smoother matrix results in regions with more connections
# getting more relative degrees of freedom in the reduced rank
# approx, as is the case in the full GMRF model

# at the moment, k must be equal to the number of groups with connections + null.space
#g1 <- gam(y ~ -1 + ctmgrp + s(cid, bs = "gmrf", xt = xt, k = 6), method="REML", data = dat)
g1 <- gam(y ~ -1 + ctmgrp + s(cid, bs = "gmrf", xt = xtr, k = 3), method="REML", data = dat)
g1 <- gam(y ~ -1 + ctmgrp + s(cid, bs = "gmrf", xt = xtr, k = 10), method="REML", data = dat)

# note it may not be the case the the appropriate thing is to add 2 to the null space dim
# I am pretty sure it is though...

summary(g1)
# check groups sum to ctm group means
tapply(fitted(g1), dat$ctmgrp, mean)

# plot fitted values
plot(ctm, col = heat.colors(length(breaks)-1)[as.numeric(cut(fitted(g1), breaks))])

