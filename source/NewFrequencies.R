NewFrequencies <- function(population.frequencies, migration.matrix, n.sites, deme.size) {
  ## creates new allele frequencies after dispersal
  ## population.frequencies :  output from InitalAlleleFrequencies or previous generation
  ## migration.matrix : Output from MigrationMatrices, rows == to, cols == from
  ## n.sites: number of demes or sites
  ## deme.size:  the population size of each deme (used for adjusting incoming alleles)
  assign("NEW", NULL, envir = .GlobalEnv)                     # note assigning to global environment
  for (n in 1:n.sites) {                                      # go through each site (row) in a migration matrix
    site.n <- migration.matrix[n, ]                       
    to <- population.frequencies[which(population.frequencies[, 1] == n), ]
    for (l in unique(to[, 2])){                               # perform movement of alleles, 1 locus at a time
      to2 <- to[which(to[, 2] == l), ]
      OUT = NULL
      for (i in 1:length(site.n)) {                         # go through each element of (from) for each site
        from <- population.frequencies[which(population.frequencies[, 1] == i), ]
        from <- from[which(from[, 2] == l), ] 
        new.alleles <- cbind(from[, 3], site.n[i] * from[, 4])
        OUT <- rbind(OUT, new.alleles)
      }
      new.alleles <- rep(OUT[, 1], OUT[, 2])                 # may not be time or memeory efficient with large data sets, see commented out "merge" below, for alternative
      old.alleles <- rep(to2[, 3], deme.size * to2[, 4])     # create vectors with all alleles and then retabulate
      alleles <- c(new.alleles, old.alleles)
      alleles <- data.frame(table(alleles))
      alleles2 <- cbind(as.numeric(as.character(alleles[, 1])), alleles[, 2]/sum(alleles[, 2]))
      alleles2 <- cbind(n, l, alleles2)
      NEW <<- rbind(NEW, alleles2)
      #print(NEW)
      # possible alternative
      # merged <- merge(old.alleles, new.alleles, by = 1, all.y = TRUE)
      # merged <- cbind(merged[, 1], merged[, 2] + merged[3])
      # merged <- cbind(merged[, 1], merged[, 2] / sum(merged[, 2]))
      # merged <- cbind(n, l, i, merged)
      # write.table(merged, paste(getwd(), "/output/freqs.txt", sep = ''), col.names = FALSE, sep="\t", append = TRUE) 
      # print(merged)
    }
  }
}


