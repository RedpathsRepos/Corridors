MigrationMatrices <- function(distances, deme.length, range, n.sites, n.isolates) {
  ## creates migration matrices from output of PropaguleDistances
  # requires library plotrix
  ## distances: the 1 dimensional location of a propagule after dispersal.  propagules in rows, demes in coloumns
  ## deme.length:  the proportion of the total area to be occupied by each deme
  ## range:  recycled from PopaguleDistances, equal to the total 1D distance of they study system
  ## n.sites: recycled from PropaguleDistances, total number of sites
  ## n.isolates:  number of sites to isolate from any migration (will still undergo drift though)
  #calculate survival ranges
  
  x <- sqrt(area)
  nodes <- c(c(x, x),c(x*2, x),c(x*2, x*2),c(x, x*2))                           # find center of each deme
  node.area <- 10
  node.length <- sqrt(10)/2
  xs <- nodes[seq(1, length(nodes), 2)]
  ys <- nodes[seq(2, length(nodes), 2)]
  x1 <- xs-node.length 
  x2 <- xs+node.length
  y1 <- ys-node.length
  y2 <- ys+node.length
  point1 <- cbind(x1, y1)  # bottom left, right, top right, left
  point2 <- cbind(x2, y1)
  point3 <- cbind(x2, y2)
  point4 <- cbind(x1, y2)
  segments(point1[, 1], point1[, 2], point2[, 1], point2[, 2], col="purple", lwd = 3)
  segments(point3[, 1], point3[, 2], point2[, 1], point2[, 2], col="purple", lwd = 3)
  segments(point3[, 1], point3[, 2], point4[, 1], point4[, 2], col="purple", lwd = 3)
  segments(point1[, 1], point1[, 2], point4[, 1], point4[, 2], col="purple", lwd = 3)
  
  #bottom corridor
  width <- 0.5
  xleft1 <- point2[1, 1]
  xright1 <- point2[4, 2]
  ytop1 <- ys[1]+width
  ybottom1 <- ys[1]-width
  rect(xleft1, ybottom1, xright1, ytop1, border="purple", lwd=2)
  
  #top corridor
  xleft1 <- point2[1, 1]
  xright1 <- point2[4, 2]
  ytop1 <- ys[3]+width
  ybottom1 <- ys[3]-width
  rect(xleft1, ybottom1, xright1, ytop1, border="purple", lwd=2)
  
  #left corridor
  xleft1 <- xs[2]-width
  xright1 <- xs[2]+width
  ytop1 <- point2[3, 2]
  ybottom1 <- point3[1, 2]
  rect(xleft1, ybottom1, xright1, ytop1, border="purple", lwd=2)
  
  #right corridor
  xleft1 <- xs[1]-width
  xright1 <- xs[1]+width
  ytop1 <- point2[3, 2]
  ybottom1 <- point3[1, 2]
  rect(xleft1, ybottom1, xright1, ytop1, border="purple", lwd=2)
  
  
  
  segments(xs[1], ys[1]+width, xs[2], ys[1]+width, col="purple", lwd=3)
  segments(xs[1], ys[1]-width, xs[2], ys[1]-width, col="purple", lwd=3)
  
  corx1 <- xs-width
  corx2 <- xs+width
  cory1 <- ys-width
  cory2 <- ys+width
  
  
  
  deme.distance <- deme.length * range                                          # calculate total length of each deme
  ranges <- cbind(locations - (deme.distance/2), locations + (deme.distance/2)) # calcualte deme boundaries
  DIST <- NULL                                                                  # reformat distances
  for (n in 1:n.sites) {
    dist <- distances[, n]
    dist <- cbind(n, dist)
    DIST <- rbind(DIST, dist)   #plot DIST if want dispersal kernnel figure, consider smoothScatter
  }
  # plot(DIST[, 1], DIST[, 2])                                                   # comment out as needed
  m.matrix <- matrix(nrow = n.sites, ncol = n.sites)                             # initialize migration matrix, col represent from, rows represent to
  for (i in 1:length(ranges[,1])) {                                              # begin calculation of migration matrix
    minr <- ranges[i, 1]                                                         # find min of deme i
    maxr <- ranges[i, 2]                                                         # find max of deme i
    recruits <- DIST[which(DIST[, 2] > minr & DIST[, 2] < maxr), ]               # find distances between range, for each deme
    recruits <- factor(recruits[, 1], levels <- 1:n.sites)                       # transform to factors, so missing values will be counted
    recruits <- table(recruits)                                                  # count recruits
    m.matrix[i, ] <- recruits                                                    # add to each row of matrix
    }
  r.isolate <- 0:n.isolates
  m.matrix[r.isolate, ] <- 0                                                      #remove dispersal for isolated site
  return(m.matrix)
}



