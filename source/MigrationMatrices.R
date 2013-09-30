MigrationMatrices <- function(distances, deme.length, range, n.sites, n.isolates) {
  ## creates migration matrices from output of PropaguleDistances
  ## distances: the 1 dimensional location of a propagule after dispersal.  propagules in rows, demes in coloumns
  ## deme.length:  the proportion of the total area to be occupied by each deme
  ## range:  recycled from PopaguleDistances, equal to the total 1D distance of they study system
  ## n.sites: recycled from PropaguleDistances, total number of sites
  ## n.isolates:  number of sites to isolate from any migration (will still undergo drift though)
  #calculate survival ranges
  locations <- seq(from=0, to=range, by=range/(n.sites-1))                      # find center of each deme
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



