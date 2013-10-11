DispersalKernnel <- function(shape, scale) {
  # Shows gamma distribution for desired dispersal kernnel
  # Not used directly, parameters are implemented in PropaguleDistances2D
  #  Useful as function if want to save plots, for instance
  hist(rgamma(5000, shape, scale), breaks = 50, freq = FALSE, xlab = "Distance")
  #OUT <- cbind(coords, gtypes)
  #return(OUT)
}