#FunctionSourcer <- function() {
  # Set working directory, import packages, source functions, 
  setwd(paste(base.directory,"/rscript.corridors/", sep = ''))    # set temp working directory 
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
  source(paste(getwd(), "/source/RunModel.R", sep = ''))
  source(paste(getwd(), "/source/PlotPatchNoPlot.R", sep = ''))
  setwd(paste(base.directory,"/working/", sep = ''))  # set actual working directory
  #}

# deprecated
# source(paste(getwd(), "/source/InitializeModel.R", sep = ''))