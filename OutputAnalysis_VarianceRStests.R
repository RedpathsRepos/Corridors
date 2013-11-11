#============================================================================================================================#
# Script created by Mark Christie, contact at Redpath.Christie@gmail.com
# Script created in version R 3.0.1 
# This script:  Is the basic model for the corridors project
# Usage notes:  set variables, fine tune as needed
#============================================================================================================================#
setwd("C:/Dropbox/InPrep/corridors/data_and_output/VarianceReproductiveSuccessTest/NoVarianceRS")    # Source functions and load packages 
dat <- read.table(paste(getwd(), "/sim_stats.txt", sep = ''), header=FALSE, sep="\t", na.strings="?", dec=".", strip.white=TRUE)
colnames(dat) <- c("area", "node.area", "width", "c.capacity", "mu", "size", "shape", "scale", "n.alleles", "n.loci", "mutation.rate", "rep.no","generation", "Ho","Hs","Ht","Dst","Htp","Dstp","Fst","Fstp","Fis", "Dest", "N")

#pdf("Figure 5.pdf", onefile=FALSE, width = 3.5, height=10, paper="letter", title="Figure 5", pointsize=10)
#par(mfrow=c(1,5), omi=c(4,0,2,.1), mai=c(1,1,0.5,0.1) ,cex=1.5, las=1)   # here I played with omi and pdf height to raise figure  above center

OUT <- NULL
for (w in unique(dat[, 4])){
  dat1 <- dat[dat[, 4]==w, ]
  for (n in unique(dat1[, 13])){
    dat2 <- dat1[dat1[, 13]==n, ]
    output <- mean(dat2[, 20])   # 20 mean Fst, 14 = ho
    output <- cbind(n,dat2[1, 3], dat2[1, 4], output)
    OUT <- rbind(OUT, output)
  }
}



dat5 <- OUT


setwd("C:/Dropbox/InPrep/corridors/data_and_output/VarianceReproductiveSuccessTest/VarianceRS")    # Source functions and load packages 
dat <- read.table(paste(getwd(), "/sim_stats.txt", sep = ''), header=FALSE, sep="\t", na.strings="?", dec=".", strip.white=TRUE)
colnames(dat) <- c("area", "node.area", "width", "c.capacity", "mu", "size", "shape", "scale", "n.alleles", "n.loci", "mutation.rate", "rep.no","generation", "Ho","Hs","Ht","Dst","Htp","Dstp","Fst","Fstp","Fis", "Dest", "N")

OUT <- NULL
for (w in unique(dat[, 4])){
  dat1 <- dat[dat[, 4]==w, ]
  for (n in unique(dat1[, 13])){
    dat2 <- dat1[dat1[, 13]==n, ]
    output <- mean(dat2[, 20])   # 20 mean Fst, 14 = ho
    output <- cbind(n,dat2[1, 3], dat2[1, 4], output)
    OUT <- rbind(OUT, output)
  }
}


dat2 <- OUT

par(mfrow=c(2,3))
dat3 <- dat2[dat2[, 3] == 1000, ]
plot(dat3[, 1], dat3[, 4], xlab = "Generations", ylab= "Fst", main = "N = 1000", col = "blue")
dat3 <- dat5[dat5[, 3] == 1000, ]
points(dat3[, 1], dat3[, 4], col = "green")
key=c("Variance in RS", "No Variance in RS")
legend(1,0.08, key, pch=c(1,1,1), col = c("blue", "green"), bty= "n")

dat3 <- dat2[dat2[, 3] == 2000, ]
plot(dat3[, 1], dat3[, 4], xlab = "Generations", ylab= "Fst", main = "N = 2000", col = "blue", ylim = c(0, 0.08))
dat3 <- dat5[dat5[, 3] == 2000, ]
points(dat3[, 1], dat3[, 4], col = "green")
legend(1,0.08, key, pch=c(1,1,1), col = c("blue", "green"), bty= "n")

dat3 <- dat2[dat2[, 3] == 3000, ]
plot(dat3[, 1], dat3[, 4], xlab = "Generations", ylab= "Fst", main = "N = 3000", col = "blue", ylim = c(0, 0.08))
dat3 <- dat5[dat5[, 3] == 3000, ]
points(dat3[, 1], dat3[, 4], col = "green")
legend(1,0.08, key, pch=c(1,1,1), col = c("blue", "green"), bty= "n")

dat3 <- dat2[dat2[, 3] == 4000, ]
plot(dat3[, 1], dat3[, 4], xlab = "Generations", ylab= "Fst", main = "N = 4000", col = "blue", ylim = c(0, 0.08))
dat3 <- dat5[dat5[, 3] == 4000, ]
points(dat3[, 1], dat3[, 4], col = "green")
legend(1,0.08, key, pch=c(1,1,1), col = c("blue", "green"), bty= "n")

dat3 <- dat2[dat2[, 3] == 5000, ]
plot(dat3[, 1], dat3[, 4], xlab = "Generations", ylab= "Fst", main = "N = 5000", col = "blue", ylim = c(0, 0.08))
dat3 <- dat5[dat5[, 3] == 5000, ]
points(dat3[, 1], dat3[, 4], col = "green")
legend(1,0.08, key, pch=c(1,1,1), col = c("blue", "green"), bty= "n")


