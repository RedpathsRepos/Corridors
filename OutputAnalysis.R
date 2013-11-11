#============================================================================================================================#
# Script created by Mark Christie, contact at Redpath.Christie@gmail.com
# Script created in version R 3.0.1 
# This script:  Is the basic model for the corridors project
# Usage notes:  set variables, fine tune as needed
#============================================================================================================================#
setwd("C:/Dropbox/InPrep/corridors/data_and_output/test4/working")        # Source functions and load packages 
dat.all <- read.table(paste(getwd(), "/sim_stats.txt", sep = ''), header=FALSE, sep="\t", na.strings="?", dec=".", strip.white=TRUE)
colnames(dat.all) <- c("area", "node.area", "width", "c.capacity", "mu", "size", "shape", "scale", "n.alleles", "n.loci", "mutation.rate", "rep.no","generation", "Ho","Hs","Ht","Dst","Htp","Dstp","Fst","Fstp","Fis", "Dest", "N")

#pdf("Figure 5.pdf", onefile=FALSE, width = 3.5, height=10, paper="letter", title="Figure 5", pointsize=10)
#par(mfrow=c(1,5), omi=c(4,0,2,.1), mai=c(1,1,0.5,0.1) ,cex=1.5, las=1)   # here I played with omi and pdf height to raise figure  above center

par(mfrow=c(2,3))


for(i in unique(dat.all[, 1])){
  
dat = dat.all[dat.all[, 1]==i, ]

OUT <- NULL
for (w in unique(dat[, 3])){
  dat1 <- dat[dat[, 3]==w, ]
  for (n in unique(dat1[, 13])){
    dat2 <- dat1[dat1[, 13]==n, ]
    output <- mean(dat2[, 20])   # 20 mean Fst, 14 = ho
    output <- cbind(n,dat2[1, 3], dat2[1, 4], output)
    OUT <- rbind(OUT, output)
  }
}


dat2 <- OUT
dat3 <- dat2[dat2[, 2] == 0.8, ]
plot(dat3[, 1], dat3[, 4], xlab = "Generations", ylab= "Fst", ylim = c(0, 0.075), main = paste("Length = ",i))

dat3 <- dat2[dat2[, 2] == 2, ]
points(dat3[, 1], dat3[, 4], xlab = "Generations", ylab= "Fst", col = "red")

dat3 <- dat2[dat2[, 2] == 3, ]
points(dat3[, 1], dat3[, 4], xlab = "Generations", ylab= "Fst", col = "orange")

dat3 <- dat2[dat2[, 2] == 4, ]
points(dat3[, 1], dat3[, 4], xlab = "Generations", ylab= "Fst", col = "blue")

dat3 <- dat2[dat2[, 2] == 5, ]
points(dat3[, 1], dat3[, 4], xlab = "Generations", ylab= "Fst", col = "green")


key=c("Width = 0.8", "Width = 2", "Width = 3", "Width = 4", "Width = 5")
legend(-1.1,0.08, key, pch=c(1,1,1), col = c("black", "red", "orange", "blue", "green"), bty= "n")

}

shape <- dat.all[1, 7]
scale <- dat.all[1, 8]
N <- dat.all[1, 4]
hist(rgamma(5000, shape, scale), breaks = 50, freq = FALSE, xlab = "Distance", main = paste("N=",N,sep=''), xlim=c(0,60))

