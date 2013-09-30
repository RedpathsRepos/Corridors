InitialAlleleFrequencies <- function(n.sites, n.loci, n.alleles) {
  ## creates allele frequencies for an initial population
  ## n.sites: total number of sites
  ## n.loci: number of desired loci
  ## n.alleles: number of desired alleles per locus
  ## output by column, pop number, locus number, allele name, allele frequency
  #create allele freqs
  a = c(1:n.alleles)
  b = rev(a)
  c = b/a
  d = sum(c)
  freqs = c/d
  lowestallele = 100
  alleles2 = cbind(seq(lowestallele, lowestallele+n.alleles-1, 1), freqs)
  l.names <- expand.grid(1:n.sites, 1:n.loci)
  l.names <- l.names[order(l.names[, 1]), ]
  l.names <- paste(l.names[, 1], l.names[, 2])
  OUT <- list()
  for (i in l.names) {
    OUT[[i]] <- alleles2    
  }
  return(OUT)
}
