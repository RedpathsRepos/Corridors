FunctionSourcer <- function() {
  # Set working directory, import packages, source functions, 
  library(hierfstat)
  library(mnormt)   # used for multivariate normal distribution in initalizing model  
  source(paste(getwd(), "/source/ReproductiveSuccess.R", sep = ''))
  source(paste(getwd(), "/source/DispersalKernnel.R", sep = ''))
  source(paste(getwd(), "/source/PropaguleDistances2D.R", sep = ''))
  source(paste(getwd(), "/source/PlotPatch.R", sep = ''))
  source(paste(getwd(), "/source/InsideOutTest.R", sep = ''))
  source(paste(getwd(), "/source/CarryingCapacity.R", sep = ''))
  source(paste(getwd(), "/source/InitialAlleleFrequencies.R", sep = ''))
  source(paste(getwd(), "/source/SummaryStats.R", sep = ''))
  source(paste(getwd(), "/source/MutationModel.R", sep = ''))
  source(paste(getwd(), "/source/InitializeModel.R", sep = ''))
  source(paste(getwd(), "/source/RunModel.R", sep = ''))
  setwd("C:/Dropbox/InPrep/corridors/working")                        # set actual working directory 
}