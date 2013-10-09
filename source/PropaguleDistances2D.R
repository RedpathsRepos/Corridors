PropaguleDistances2D <- function(n.propagules, std.dev, initialize, distances) {
  # creates locations of prpoagules after dispersal in a 2D framework; currently multivariate normally distributed
  # requires library mnormt
  ## area: the total length of the 2 dimensional habitat, is unitless
  ## std.dev: the standard deviation to use for generating dispersal kernels
  ## n.propagules:  how many propagues to disperse per sites
  ## initalize: if TRUE assumes this is first run; if FALSE will use existing distance object capacity
  ## distances: if initialize == TRUE, then a distances object must be supplied
  OUT <- NULL
  if (initialize == TRUE){
    x <- sqrt(area)  # the desired axis length
    nodes <- c(c(x, x),c(x*2, x),c(x*2, x*2),c(x, x*2))   # node locations: bottom left, bottom right, top right, top left
    nodes <- c(c(0, 0),c(x, 0),c(x, x),c(0, x))           # node locations: bottom left, bottom right, top right, top left
    OUT <- mapply(rmnorm, n.propagules, nodes, std.dev)   # locations will be a vector of starting positions
    ncols <- ncol(OUT)
    OUT2 <- NULL
    for (i in seq(from = 1 , to = ncols, by =2)){         # reformatting of output for downstream analyses
      tmp <- OUT[,c(i, i+1)]
      tmp2 <- cbind(rep(ceiling(i/2), length(tmp[,1])), tmp)
      OUT2 <- rbind(OUT2, tmp2)
      }
    OUT2 <- cbind(1:length(OUT2[, 1]), OUT2)              # A "distances' object: col1:unique ID, col2:population origion, col3,4: x,y 
    return (OUT2)
  }  
    
  if (initialize == FALSE) {
    nodes <- distances[, 3:4]                               
    OUT <- NULL
    for (n in 1:length(nodes[, 1])){                     # get nodes in appropriate format for rmnorm
      OUT <- c(OUT, nodes[n, 1], nodes[n, 2])
    }
    nodes <- as.numeric(OUT)
    OUT <- mapply(rmnorm, n.propagules, nodes, std.dev)   # locations will be a vector of starting positions
    ncols <- ncol(OUT)
    OUT2 <- NULL
    for (i in seq(from = 1 , to = ncols, by =2)){         # reformatting of output for downstream analyses
      tmp <- OUT[,c(i, i+1)]
      m1 <- distances[floor(i/2)+1, ]
      m2 <- cbind(m1[1], m1[2], tmp)
      #gtypes1 <- m1[-(1:4)]                              # reactivate if want to track ALL genotypes of EVERY offspring
      #gtypes2 <- t(matrix(gtypes1, length(gtypes1), length(m2[, 1])))
      #m3 <- cbind(m2, gtypes2)
      OUT2 <- rbind(OUT2, m2)
      } 
    return(OUT2)
  }
}
  



