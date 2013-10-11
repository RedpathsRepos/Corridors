#============================================================================================================================#
# Script created by Mark Christie, contact at Redpath.Christie@gmail.com
# Script created in version R 3.0.1 
# This script:  Is the basic model for the corridors project
# Usage notes:  set variables, fine tune as needed
#============================================================================================================================#
setwd("C:/Dropbox/InPrep/corridors/rscript.corridors")        # Source functions and load packages 
source(paste(getwd(), "/source/FunctionSourcer.R", sep = ''))
FunctionSourcer()         # loads packages, sources functions, and sets the working directory

area       <- 01000       # the total area of the matrix (theoretocally is unitless)
node.area  <- 00100       # the area for each patch
width      <- 00005       # the width of the corridor
c.capacity <- 02000       # the total carrying capacity (currently regulated across entire metapopulation)

mu         <- 005         # Reproductive Success
size       <- 0.3
rs.shape   <- ReproductiveSuccess(c.capacity, mu, size)
sum(rs.shape); mean(rs.shape)

shape     <- 006          # Dispersal Kernnel
scale     <- 001
kernnel   <- DispersalKernnel(shape, scale)                   # if dispersal distances are too large, will drop below carrying capacity

n.alleles     <- 2        # Marker and Genotype Characteristics
n.loci        <- 100
mutation.rate <- 0.00001

n.reps        <- 0030     # model run parameters, numer pf replicate runs
n.generations <- 0100     # number of generations to run model for
#============================================================================================================================#

width <- c(0.8, 2, 5)

for (i in width){
  width <- i
  initial <- InitializeModel()
  model <- RunModel(n.reps, n.generations)
}








