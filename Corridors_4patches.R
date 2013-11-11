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
library(plotrix)
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
source(paste(getwd(), "/source/PlotPatch.Show.R", sep = ''))

mu <- 1
size <- 0.1

# initialize dispersal kernnel
shape <- 1.2
scale <- .15
kernnel <- DispersalKernnel(shape, scale)  # if dispersal distances are too large, will drop below carrying capacity


area <- 2500             # the total area of the matrix (theoretocally is unitless)
sqrt(area)
node.area <- 100        # the area for each patch
width <- .8             # the width of the corridor
std.dev <- 3            # the standard deviation to use for generating dispersal kernels ; default is 3
n.propagules <- 20      # how many propagues to disperse per sites
c.capacity <- 500      # the total carrying capacity (currently regulated across entire metapopulation)
n.alleles <- 2
n.loci <- 100
mutation.rate <- 0.00000001

#======================================================================================================================#
#dat <- read.table(paste(getwd(), "/data/.txt", sep = ''), header=TRUE, sep="\t", na.strings="?", dec=".", strip.white=TRUE)
#write.table(output, paste(getwd(), "/output/.txt", sep = ''), col.names = FALSE, sep="\t", append = TRUE)   

# initialize reprductive succes
rs.shape <- ReproductiveSuccess(c.capacity, mu, size)
sum(rs.shape); mean(rs.shape)
hist(rs.shape, breaks = 50)
if(sum(rs.shape)<c.capacity) {print("WARNING: CHANGE REPRODUCTIVE SUCCESS!!!")}  # should be greater than carrying capacity

distances <- PropaguleDistances2D(n.propagules = 10000, std.dev, initialize = TRUE, distances = NULL)
plot1 <- PlotPatch.Show(distances, area, node.area, width)

attrition <- InsideOutTest(polygons = plot1, distances)
plot2 <- PlotPatch.Show(distances = attrition, area, node.area, width)

capacity <- CarryingCapacity(distances = attrition, g.types = NULL, c.capacity) 
plot3 <- PlotPatch.Show(distances = capacity, area, node.area, width)
genotypes <- InitialAlleleFrequencies(n.individs = c.capacity, n.loci, n.alleles) 

distances.new <- cbind(capacity, genotypes)                   # individual ids are a little strange because of carrying capacity
capacity <- cbind(1:length(capacity[, 1]), distances.new[order(distances.new[, 2]), -1])  # this fixes the above

OUT = NULL
for (i in 1:70){
  distances.new <- PropaguleDistances2D(n.propagules, std.dev, initialize = FALSE, distances = capacity, rs.shape, shape, scale)
  plotx <- PlotPatch.Show(distances = distances.new, area, node.area, width)
  attrition <- InsideOutTest(polygons = plot1, distances = distances.new)
  capacity <- CarryingCapacity(distances = attrition, g.types = capacity, c.capacity) 
  capacity <- MutationModule(distances = capacity, mutation.rate)
  plot3 <- PlotPatch.Show(distances = capacity, area, node.area, width)
  if(length(capacity[, 1]) < 50) {break}
  sum <- SummaryStats(polygons = plot1, distances = capacity)
  if(sum[1]=="OnePopOnly") {next}
  output <- t(data.frame(c(i, sum)))
  OUT <- rbind(OUT, output)
}


plot(OUT[, 1], OUT[, 9])   # Fst
plot(OUT[, 1], OUT[, 2])   # Ho
plot(OUT[, 1], OUT[, 11])  # FIS

