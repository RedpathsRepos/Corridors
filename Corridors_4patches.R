#======================================================================================================================#
# Script created by Mark Christie, contact at Redpath.Christie@gmail.com
# Script created in version R 3.0.1 
# This script:  Is the basic model (desribed in notes) for marine speciation project
# Usage notes:  s
#======================================================================================================================#
# Set working directory, import packages, source functions, initialize global variables
setwd("C:/Dropbox/InPrep/corridors/rscript")

library(mnormt)
library(hierfstat)
#diveRsity

source(paste(getwd(), "/source/PropaguleDistances2D.R", sep = ''))
source(paste(getwd(), "/source/PlotPatch.R", sep = ''))
source(paste(getwd(), "/source/InsideOutTest.R", sep = ''))
source(paste(getwd(), "/source/CarryingCapacity.R", sep = ''))
source(paste(getwd(), "/source/InitialAlleleFrequencies.R", sep = ''))

area <- 100             # the total area of the matrix (theoretocally is unitless)
node.area <- 10         # the area for each patch
std.dev <- 3           # the standard deviation to use for generating dispersal kernels
n.propagules <- 100     # how many propagues to disperse per sites
c.capacity <- 400      # the total carrying capacity (currently regulated across entire metapopulation)
n.alleles <- 2
n.loci <- 10

#======================================================================================================================#
#dat <- read.table(paste(getwd(), "/data/.txt", sep = ''), header=TRUE, sep="\t", na.strings="?", dec=".", strip.white=TRUE)
#write.table(output, paste(getwd(), "/output/.txt", sep = ''), col.names = FALSE, sep="\t", append = TRUE)   

distances <- PropaguleDistances2D(n.propagules = 1000, std.dev, initialize = TRUE)
plot1 <- PlotPatch(distances, area, node.area)

attrition <- InsideOutTest(polygons = plot1, distances)
plot2 <- PlotPatch(distances = attrition, area, node.area)

capacity <- CarryingCapacity(distances = attrition, c.capacity) 
plot3 <- PlotPatch(distances = capacity, area, node.area)
genotypes <- InitialAlleleFrequencies(n.individs = c.capacity, n.loci, n.alleles) 

for (i in 1:150){
new.distances <- PropaguleDistances2D(n.propagules, std.dev, initialize = FALSE)
#plot1 <- PlotPatch(distances = new.distances, area, node.area)
attrition <- InsideOutTest(polygons = plot1, distances = new.distances)
capacity <- CarryingCapacity(distances = attrition, c.capacity) 
plot3 <- PlotPatch(distances = capacity, area, node.area)
}


# reproduction
# dispersal
# measure relevant parameters
# repeat






migration.matrix <- MigrationMatrices(distances, deme.length, range, n.sites, n.isolates)      # distances must come from PropaguleDistances
m.matrix <- t(migration.matrix)



#isolate desired number of pops (check when more than 1)
pops <- 1
reduction <- 0.1  #really 1 - reduction, so smaller values equal bigger reduction
m.matrix[-pops, pops] <- floor(m.matrix[-pops, pops] * reduction)
m.matrix[pops, -pops] <- floor(m.matrix[pops, -pops] * reduction)

#change to propotions
n.matrix <- matrix(ncol=n.sites, nrow=n.sites)
for (i in 1:n.sites) {
  rowi <- m.matrix[i, ]
  totals <- sum(rowi)
  props <- rowi/totals
  n.matrix[i, ] <- props
}
rowSums(n.matrix)   #should all be 1, serves as check

#n.matrix <- cbind.data.frame("{", n.matrix, "}")
n.matrix <- cbind("{", n.matrix, "}")
n.matrix[1, 1] <- "{{"
n.matrix[length(n.matrix)] <- "}}"
matrix1 <- n.matrix

#################matrix 2
pops <- 1
reduction <- 0  #really 1 - reduction, so smaller values equal bigger reduction
m.matrix[-pops, pops] <- floor(m.matrix[-pops, pops] * reduction)
m.matrix[pops, -pops] <- floor(m.matrix[pops, -pops] * reduction)

#change to propotions
n.matrix <- matrix(ncol=n.sites, nrow=n.sites)
for (i in 1:n.sites) {
  rowi <- m.matrix[i, ]
  totals <- sum(rowi)
  props <- rowi/totals
  n.matrix[i, ] <- props
}
rowSums(n.matrix)   #should all be 1, serves as check

#n.matrix <- cbind.data.frame("{", n.matrix, "}")
n.matrix <- cbind("{", n.matrix, "}")
n.matrix[1, 1] <- "{{"
n.matrix[length(n.matrix)] <- "}}"
matrix2 <- n.matrix


##################formatting
n.matrix <- rbind(matrix1, matrix2)


write.table(n.matrix, "/home/miles/Programs/quantinemo/dispersalfile.txt", col.names = FALSE, row.names = FALSE, sep=" ", append = FALSE, quote = FALSE)   # note use of quote = False to strip quotes ! :) 



