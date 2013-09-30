#UNFINISHED <<_ TRIED TO USE LISTS INSTEAD OF DATAFRAMES...

NewFrequencies <- function(population.frequencies, migration.matrix, n.sites, deme.size) {
  ## creates new allele frequencies after dispersal
  ## population.frequencies :  output from InitalAlleleFrequencies or previous generation
  ## migration.matrix : Output from MigrationMatrices, rows == to, cols == from
  ## n.sites: number of demes or sites
  ## deme.size:  the population size of each deme (used for adjusting incoming alleles)
  assign("NEW", NULL, envir = .GlobalEnv)                     # note assigning to global environment
  for (n in 1:n.sites) {                                      # go through each site (row) in a migration matrix
    site.n <- migration.matrix[n, ]
    site.n2 <- rep(site.n, each = 2)
    for (l in 1:length(population.frequencies)){
      population.frequencies[[l]] <- cbind(population.frequencies[[l]], site.n2[l])
    }
    pop.freq <- population.frequencies
    for (l in 1:length(population.frequencies)){
      asp <- population.frequencies[[l]]
      asp <- rep(asp[, 1], asp[, 2] * asp[, 3])
      pop.freq[[l]] <- asp
    }
    
    
    
    
    for (l in 1:n.loci) {
      loci <- which(as.numeric(t(data.frame(strsplit(names(population.frequencies), "")))[, 3])==l)
      pops <- population.frequencies[loci]                     #get all the pops for a given locus
      for (s in 1:n.sites) {
        pop <- pops[[s]]
        alleles <- cbind)
        
      }
      
    }
    
    
    
    
    
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


