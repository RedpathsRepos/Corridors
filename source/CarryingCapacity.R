CarryingCapacity <- function(distances, c.capcity) {
  # reduces the entire population size to the desired carrying capacity
  ## distances: output from function PropaguleDistances2D or Raycasting (in distances format) 
  ## c.capacity: the entire carrying capacity of the metapopulation
  keepers <- sample(1:length(distances[, 1]), c.capcity, replace = FALSE)
  OUT <- distances[keepers, ]
  return(OUT)
}
