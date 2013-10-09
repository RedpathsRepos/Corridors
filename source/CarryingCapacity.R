CarryingCapacity <- function(distances, g.types, c.capacity) {
  # reduces the entire population size to the desired carrying capacity
  ## distances: output from function PropaguleDistances2D or Raycasting (in distances format) 
  ## gtypes: a distances object complete with last generations genotypes
  ## c.capacity: the entire carrying capacity of the metapopulation
  keepers <- sample(1:length(distances[, 1]), c.capacity, replace = FALSE)
  individuals <- distances[keepers, ]
  m1 <- match(individuals[, 1], g.types[, 1])  # could get previous generation dispersal distances here
  inds <- cbind(individuals, g.types[m1, -(1:4)])
  OUT <- inds[order(inds[, 1]), ]
  return(OUT)
}
