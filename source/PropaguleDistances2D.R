PropaguleDistances2D <- function(n.propagules, std.dev, initialize = FALSE) {
  # creates locations of prpoagules after dispersal in a 2D framework; currently multivariate normally distributed
  # requires library mnormt
  ## area: the total length of the 2 dimensional habitat, is unitless
  ## std.dev: the standard deviation to use for generating dispersal kernels
  ## n.propagules:  how many propagues to disperse per sites
  ## initalize: if TRUE assumes this is first run; if FALSE will use existing distance object capacity
  OUT <- NULL
  if (initialize == TRUE){
    x <- sqrt(area)  # the desired axis length
    nodes <- c(c(x, x),c(x*2, x),c(x*2, x*2),c(x, x*2))  # node locations: bottom left, bottom right, top right, top left
    nodes <- c(c(0, 0),c(x, 0),c(x, x),c(0, x))  # node locations: bottom left, bottom right, top right, top left
    } else {
    nodes <- capacity[order(capacity[, 1]), ]
    nodes <- nodes[, 2:3]                                # where capacity is a 'distances' object
    OUT <- NULL
    for (n in 1:length(nodes[, 1])){                     # get nodes in appropriate format for rmnorm
    OUT <- c(OUT, nodes[n, 1], nodes[n, 2])
    }
    nodes <- OUT
  }
  OUT <- mapply(rmnorm, n.propagules, nodes, std.dev)   # locations will be a vector of starting positions
  ncols <- ncol(OUT)
  OUT2 <- NULL
  for (i in seq(from = 1 , to = ncols, by =2)){         # reformatting of output for downstream analyses
    tmp <- OUT[,c(i, i+1)]
    tmp2 <- cbind(rep(ceiling(i/2), length(tmp[,1])), tmp)
    OUT2 <- rbind(OUT2, tmp2)
  } 

  OUT2 <- cbind(OUT2, OUT2[, 1])                        # copy first column to the last column
  if (initialize == FALSE) {
    values <- data.frame(table(capacity[, 1]))
    OUT2[, 1] <- rep(values[ ,1], (values[, 2] * 100))  
  } else { 
    values <- sort(rep(1:4, length(OUT2[, 1])/4))
    OUT2[, 1] <- values}                                 # remove this line if you want contemporary ID; adds in original ID
  return(OUT2)
}

