#======================================================================================================================#
# Script created by Mark Christie, contact at Redpath.Christie@gmail.com
# Script created in version R 3.0.1 
# This script:  Is the basic model (desribed in notes) for marine speciation project
# Usage notes:  s
#======================================================================================================================#
# Set working directory, import packages, source functions, initialize global variables
setwd("C:/Dropbox/InPrep/corridors/rscript.corridors")

library(mnormt)
library(hierfstat)
#diveRsity

source(paste(getwd(), "/source/ReproductiveSuccess.R", sep = ''))
source(paste(getwd(), "/source/DispersalKernnel.R", sep = ''))
source(paste(getwd(), "/source/PropaguleDistances2D.R", sep = ''))
source(paste(getwd(), "/source/PlotPatch.R", sep = ''))
source(paste(getwd(), "/source/InsideOutTest.R", sep = ''))
source(paste(getwd(), "/source/CarryingCapacity.R", sep = ''))
source(paste(getwd(), "/source/InitialAlleleFrequencies.R", sep = ''))
source(paste(getwd(), "/source/SummaryStats.R", sep = ''))
source(paste(getwd(), "/source/MutationModel.R", sep = ''))

mu <- 2
size <- 0.3
shape <- 2
scale <- 1

area <- 1000             # the total area of the matrix (theoretocally is unitless)
node.area <- 100         # the area for each patch
width <- .5              # the width of the corridor
std.dev <- 5          # the standard deviation to use for generating dispersal kernels
n.propagules <- 20    # how many propagues to disperse per sites
c.capacity <- 3000      # the total carrying capacity (currently regulated across entire metapopulation)
n.alleles <- 2
n.loci <- 100
mutation.rate <- 0.00001

#======================================================================================================================#
#dat <- read.table(paste(getwd(), "/data/.txt", sep = ''), header=TRUE, sep="\t", na.strings="?", dec=".", strip.white=TRUE)
#write.table(output, paste(getwd(), "/output/.txt", sep = ''), col.names = FALSE, sep="\t", append = TRUE)   

# initialize reprductive succes
rs.shape <- ReproductiveSuccess(c.capacity, mu, size)
sum(rs.shape); mean(rs.shape)
if(sum(rs.shape)<c.capacity) {print("WARNING: CHANGE REPRODUCTIVE SUCCESS!!!")}  # should be greater than carrying capacity

# initialize dispersal kernnel
kernnel <- DispersalKernnel(shape, scale)  # if dispersal distances are too large, will drop below carrying capacity

distances <- PropaguleDistances2D(n.propagules = 10000, std.dev, initialize = TRUE, distances = NULL)
plot1 <- PlotPatch(distances, area, node.area, width)

attrition <- InsideOutTest(polygons = plot1, distances)
plot2 <- PlotPatch(distances = attrition, area, node.area, width)

capacity <- CarryingCapacity(distances = attrition, g.types = NULL, c.capacity) 
plot3 <- PlotPatch(distances = capacity, area, node.area, width)
genotypes <- InitialAlleleFrequencies(n.individs = c.capacity, n.loci, n.alleles) 

distances.new <- cbind(capacity, genotypes)                   # individual ids are a little strange because of carrying capacity
capacity <- cbind(1:length(capacity[, 1]), distances.new[order(distances.new[, 2]), -1])  # this fixes the above

OUT = NULL
for (i in 1:50){
distances.new <- PropaguleDistances2D(n.propagules, std.dev, initialize = FALSE, distances = capacity, rs.shape, shape, scale)
#plotx <- PlotPatch(distances = distances.new, area, node.area, width)
attrition <- InsideOutTest(polygons = plot1, distances = distances.new)
capacity <- CarryingCapacity(distances = attrition, g.types = capacity, c.capacity) 
capacity <- MutationModule(distances = capacity, mutation.rate)
plot3 <- PlotPatch(distances = capacity, area, node.area, width)
if(length(capacity[, 1]) < 50) {break}
sum <- SummaryStats(polygons = plot1, distances = capacity)
if(sum[1]=="OnePopOnly") {next}
output <- t(data.frame(c(i, sum)))
OUT <- rbind(OUT, output)
}

OUT <- NULL
for (i in 1:n.generations){
  distances.new <- PropaguleDistances2D(n.propagules, std.dev, initialize = FALSE, distances = capacity, rs.shape, shape, scale)
  #plotx <- PlotPatch(distances = distances.new, area, node.area, width)
  attrition <- InsideOutTest(polygons = plot1, distances = distances.new)
  capacity <- CarryingCapacity(distances = attrition, g.types = capacity, c.capacity) 
  capacity <- MutationModule(distances = capacity, mutation.rate)
  plot3 <- PlotPatch(distances = capacity, area, node.area, width)
  if(length(capacity[, 1]) < 50) {break}
  sum <- SummaryStats(polygons = plot1, distances = capacity)
  if(sum[1]=="OnePopOnly") {next}
  output <- t(data.frame(c(i, sum)))
  OUT <- rbind(OUT, output) 
}




