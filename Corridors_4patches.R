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

source(paste(getwd(), "/source/PropaguleDistances2D.R", sep = ''))
source(paste(getwd(), "/source/PlotPatch.R", sep = ''))
source(paste(getwd(), "/source/InsideOutTest.R", sep = ''))
source(paste(getwd(), "/source/CarryingCapacity.R", sep = ''))
source(paste(getwd(), "/source/InitialAlleleFrequencies.R", sep = ''))
source(paste(getwd(), "/source/SummaryStats.R", sep = ''))


area <- 100             # the total area of the matrix (theoretocally is unitless)
node.area <- 30         # the area for each patch
width <- 2              # the width of the corridor
std.dev <- 2          # the standard deviation to use for generating dispersal kernels
n.propagules <- 20    # how many propagues to disperse per sites
c.capacity <- 4000      # the total carrying capacity (currently regulated across entire metapopulation)
n.alleles <- 2
n.loci <- 100

#======================================================================================================================#
#dat <- read.table(paste(getwd(), "/data/.txt", sep = ''), header=TRUE, sep="\t", na.strings="?", dec=".", strip.white=TRUE)
#write.table(output, paste(getwd(), "/output/.txt", sep = ''), col.names = FALSE, sep="\t", append = TRUE)   

distances <- PropaguleDistances2D(n.propagules = 1000, std.dev, initialize = TRUE, distances = NULL)
plot1 <- PlotPatch(distances, area, node.area, width)

attrition <- InsideOutTest(polygons = plot1, distances)
plot2 <- PlotPatch(distances = attrition, area, node.area, width)

capacity <- CarryingCapacity(distances = attrition, g.types = NULL, c.capacity) 
plot3 <- PlotPatch(distances = capacity, area, node.area, width)
genotypes <- InitialAlleleFrequencies(n.individs = c.capacity, n.loci, n.alleles) 

distances.new <- cbind(capacity, genotypes)                   # individual ids are a little strange because of carrying capacity
capacity <- cbind(1:length(capacity[, 1]), distances.new[order(distances.new[, 2]), -1])  # this fixes the above

for (i in 1:250){
distances.new <- PropaguleDistances2D(n.propagules, std.dev, initialize = FALSE, distances = capacity)
#plot1 <- PlotPatch(distances = new.distances, area, node.area, width)
attrition <- InsideOutTest(polygons = plot1, distances = distances.new)
capacity <- CarryingCapacity(distances = attrition, g.types = capacity, c.capacity) 
#capacity <- mutation module
plot3 <- PlotPatch(distances = capacity, area, node.area, width)
}

sum <- SummaryStats(polygons = plot1, distances = capacity)
sum
# reproduction
# dispersal
# measure relevant parameters
# repeat



