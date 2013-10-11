MutationModule <- function(distances, mutation.rate) {
  # takes a distances object and adds in mutation to the genotypes and returns a distances object
  ## distances: output from function PropaguleDistances2D or Raycasting (in distances format) 
  ## mutation.rate:  the rate of mutation 
  coords <- distances[, 1:4]
  gtypes <- distances[, -c(1:4)]
  mutants <- 100:199              # not quite infinite alleles models , 99 random possible mutations
  positions <- length(gtypes) * mutation.rate   # could cause problem if position < 1
  if (positions <= 1) {positions <- 1 ; mutants <- 100}       # takes care of the above problem 
  positions2 <- sample(1:length(gtypes), positions)
  mutants2 <- sample(mutants, positions, replace = TRUE)
  gtypes[positions2] <- mutants2  
  OUT <- cbind(coords, gtypes)
  return(OUT)
}