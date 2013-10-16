#============================================================================================================================#
# Script created by Mark Christie, contact at Redpath.Christie@gmail.com
# Script created in version R 3.0.1 
# This script:  Is the basic model for the corridors project
# Usage notes:  set variables, fine tune as needed
#============================================================================================================================#
setwd("C:/Dropbox/InPrep/corridors/working")        # Source functions and load packages 
dat <- read.table(paste(getwd(), "/sim_stats.txt", sep = ''), header=FALSE, sep="\t", na.strings="?", dec=".", strip.white=TRUE)
colnames(dat) <- c("area", "node.area", "width", "c.capacity", "mu", "size", "shape", "scale", "n.alleles", "n.loci", "mutation.rate", "rep.no","generation", "Ho","Hs","Ht","Dst","Htp","Dstp","Fst","Fstp","Fis", "Dest", "N")
dat = dat[dat[, 4]==3000, ]


dat2 <- dat[dat$width == 0.8, ]
plot(dat2[, 13], dat2[, 20], xlab = "Generations", ylab= "Fst")

dat3 <- dat[dat$width == 2, ]
points(dat3[, 13], dat3[, 20], col = "blue")

dat3 <- dat[dat$width == 5, ]
points(dat3[, 13], dat3[, 20], col = "green")


key=c("Width = 0.8", "Width = 2", "Width = 5")
legend(1,0.06327, key, pch=c(1,1,1), col = c("black", "blue", "green"), bty= "n")

