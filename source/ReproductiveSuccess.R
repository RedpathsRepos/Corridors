ReproductiveSuccess <- function(c.capacity, mu, size) {
  # defines the distribution of reproductive success values for each individual
  ## c.capacity: the carrying capacity of the entire matrix
  ##  mu and size describe the distribution
  dist <- rnbinom(c.capacity, mu, size) 
  hist(dist, breaks = 30)
  return(dist)
}