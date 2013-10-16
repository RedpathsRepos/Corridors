PropaguleDistances2D <- function(n.propagules, std.dev, initialize, distances, rs.shape, shape, scale) {
  # creates locations of prpoagules after dispersal in a 2D framework; currently multivariate normally distributed
  # requires library mnormt
  ## area: the total length of the 2 dimensional habitat, is unitless
  ## std.dev: the standard deviation to use for generating dispersal kernels
  ## n.propagules:  how many propagues to disperse per sites
  ## initalize: if TRUE assumes this is first run; if FALSE will use existing distance object capacity
  ## distances: if initialize == TRUE, then a distances object must be supplied
  ## rs.shape: the distribution of reproductive success values characterized by function 
  ## shape and scale: characterize the gamma distribution for the dispersal kernnel
  OUT <- NULL
  if (initialize == TRUE){
    x <- sqrt(area)                                           # the desired axis length
    nodes <- c(c(x, x),c(x*2, x),c(x*2, x*2),c(x, x*2))       # node locations: bottom left, bottom right, top right, top left
    nodes <- c(c(0, 0),c(x, 0),c(x, x),c(0, x))               # node locations: bottom left, bottom right, top right, top left
    OUT <- mapply(rmnorm, n.propagules, nodes, std.dev)       # locations will be a vector of starting positions, kept as multivariate normal to fill patch
    ncols <- ncol(OUT)
    OUT2 <- NULL
    for (i in seq(from = 1 , to = ncols, by =2)){             # reformatting of output for downstream analyses
      tmp <- OUT[,c(i, i+1)]
      tmp2 <- cbind(rep(ceiling(i/2), length(tmp[,1])), tmp)
      OUT2 <- rbind(OUT2, tmp2)
      }
    OUT2 <- cbind(1:length(OUT2[, 1]), OUT2)                   # A "distances' object: col1:unique ID, col2:population origion, col3,4: x,y 
    return (OUT2)
  }  
    
  if (initialize == FALSE) {
  # if(length(distances[, 1] < c.capacity)) {                  # This if deals with situations when fall below carrying capacity
  #    needed.individs <- c.capacity - length(distances[, 1])   # How many new individuals are need to get pop back up to carrying capacity
  #    s1 <- sample(1:length(distances[, 1]), needed.individs, replace = TRUE) #sampling with replacement
  #   distances <- rbind(distances, distances[s1, ])
  #    distances <- distances[order(distances[, 1]), ]
  # }
    
    OUT1.1 <- mapply(rgamma, rs.shape, shape, scale)
    OUT1.2 <- mapply(rgamma, rs.shape, shape, scale)
    ncols <- length(OUT1.1)
    ncols2 <- length(distances[, 1])
    ncols = min(ncols, ncols2)
    OUT2 <- NULL
    for (i in 1:ncols){                                        # reformatting of output for downstream analyses
      tmp <- cbind(OUT1.1[[i]], OUT1.2[[i]])                   # deals with strange formatting of list
      if (length(tmp > 0)) {                                   # only include individuals with > 0 reproductive success
        m1 <- distances[i, 1:4]                                # match back to original
        tmp[, 1] <- (tmp[, 1] * sample(c(1,-1), length(tmp[, 1]), replace = TRUE)) + m1[3] #add distance to old x, sampling is to randomize direction  
        tmp[, 2] <- (tmp[, 2] * sample(c(1,-1), length(tmp[, 1]), replace = TRUE)) + m1[4]
        m2 <- cbind(m1[1], m1[2], tmp)
        OUT2 <- rbind(OUT2, m2)
        }
      } 
    return(OUT2)
  }
}
  



