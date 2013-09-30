GeneticDrift <- function(freqs = NEW, ne.n) {
  ## Imposes drift for a given set of allele freq with cols: 1=population, 2=locus, 3=allelename, 4=frequency
  ## freqs: the set of allele freqs
  ## ne.n the NE:n ratio to apply, eg. if 0.01, then only only 10 out of 1000 alleles will be selected, 0.001 is smallest allowable value
  ## if want smaller Ne:n, then change pop.size below (1000) to a larger value
  pop.size <- 1000
  #create list out of matrix or data frame
  l.names = cbind(paste(NEW[, 1], NEW[, 2]), NEW)
  alist <- list()
  for(i in l.names[, 1]) {
    dat <- NEW[l.names[, 1] == i, ]
    alleles <- rep(dat[, 3], (dat[, 4] * pop.size))
    alist[[i]] <- alleles
  }
  alist2 <- lapply(alist, sample, pop.size*ne.n, replace = FALSE)    # genetic drift
  alist3 <- lapply(alist2, table)  
  alist4 <- lapply(alist3, function(x) x/(pop.size*ne.n))
  #convert back to data.frame, whole operation could be expidited by rewriting NewFrequencies to accept list input
  #create colnames
  NAMES = NULL
  for (i in 1:length(alist4)) {
    out <- rep(names(alist4[i]), length(alist4[[i]]))
    NAMES <- c(NAMES, out)
  }
  names <- cbind(as.numeric(t(data.frame(strsplit(NAMES, " ")))[,1]), as.numeric(t(data.frame(strsplit(NAMES, " ")))[,2]))
  NAMES2 = NULL
  for (i in 1:length(alist4)) {
    out <- names(alist4[[i]])
    NAMES2 <- c(NAMES2, out)
  }  
  drift.freqs <- data.frame(unlist(alist4, use.names = FALSE))
  OUT <- cbind(names, as.numeric(NAMES2), drift.freqs)
  colnames(OUT) <- c("site", "locus", "allele","freq")
  return(OUT)
}
