PropaguleDistances <- function(range, n.sites, std.dev, n.propagules) {
  # creates locations of prpoagules after dispersal in a 1D steeping stone; currently normally distributed
  ## range: the total length of the 1 dimensional habitat, is unitless
  ## n.sites:  the total number of sites to create along the 1D habitat, will create sites at the both ends
  ## std.dev: the standard deviation to use for generating dispersal kernels
  ## n.propagules:  how many propagues to disperse per sites
  locations <- seq(from=0, to=range, by=range/(n.sites-1))
  OUT <- mapply(rnorm, n.propagules, locations, std.dev)  #locations will be a vector of starting positions
  return(OUT)
}




