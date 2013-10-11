InitializeModel <- function() {
  # Intialize Model
  # Really an organizational function to minimize lines of code
  # Initalize model; Note uses multivariate normal distribution to initally fill out patches
  distances <- PropaguleDistances2D(n.propagules = 10000, std.dev=5, initialize = TRUE, distances = NULL)
  plot1     <- PlotPatch(distances, area, node.area, width)
  attrition <- InsideOutTest(polygons = plot1, distances)
  capacity  <- CarryingCapacity(distances = attrition, g.types = NULL, c.capacity) 
  plot3     <- PlotPatch(distances = capacity, area, node.area, width)
  genotypes <- InitialAlleleFrequencies(n.individs = c.capacity, n.loci, n.alleles) 
  distances.new <- cbind(capacity, genotypes)                                 # individual ids are a little strange because of carrying capacity
  capacity  <- cbind(1:length(capacity[, 1]), distances.new[order(distances.new[, 2]), -1])  # this fixes the above
  new.list  <- list("capacity" = capacity, "plot1" = plot1)
  return(new.list)
}