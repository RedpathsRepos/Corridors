#============================================================================================================================#
# Script created by Mark Christie, contact at Redpath.Christie@gmail.com
# Script created in version R 3.0.1 
# This script:  Is the basic model for the corridors project
# Usage notes:  set variables, fine tune as needed
#============================================================================================================================#
setwd("C:/Dropbox/InPrep/corridors/")  # Set main directory, use getwd() for use on cluster 
base.directory <- getwd()
source(paste(base.directory, "/rscript.corridors/source/FunctionSourcer.R", sep = '')) # loads packages, sources functions, and sets directory

area           <- 01000               # the total area of the matrix (theoretocally is unitless)
node.area      <- 00100               # the area for each patch
width.set      <- c(0.8, 2, 5)        # the width of the corridor
c.capacity.set <- c(3000, 2000, 1000)       # the total carrying capacity (currently regulated across entire metapopulation)

mu         <- 001                 # Reproductive Success11000
size       <- 0.1
rs.shape   <- ReproductiveSuccess(max(c.capacity.set), mu, size)  # Note this is just for max c.capcity if multiple values used
# rs.shape   <- rep(9, max(c.capacity))  - for no variance in RS
sum(rs.shape); mean(rs.shape)

shape     <- 002                  # Dispersal Kernnel
scale     <- 001
#kernnel   <- DispersalKernnel(shape, scale)                   # if dispersal distances are too large, will drop below carrying capacity

n.alleles     <- 2                # Marker and Genotype Characteristics
n.loci        <- 100
mutation.rate <- 0.00000001

n.reps        <- 0002             # model run parameters, numer pf replicate runs
n.generations <- 0002             # number of generations to run model for
#============================================================================================================================#


for (p in c.capacity.set) {
  c.capacity <- p
  rs.shape   <- ReproductiveSuccess(c.capacity, mu, size)
  for (z in width.set){
    width <- z
    model <- RunModel(n.reps, n.generations)
  }
}









