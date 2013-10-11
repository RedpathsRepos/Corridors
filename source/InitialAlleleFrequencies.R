InitialAlleleFrequencies <- function(n.individs, n.loci, n.alleles) {
  ## creates allele frequencies for an initial population in accordance with HWE; modified from Module2 of Solomon
  ## n.sites: total number of individuals == carrying capcity of entire metapopulation
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
  alleles3=cbind(100:(99+(length(alleles2[,1]))),alleles2[,-1])                   # table allele frequencies
  homos=(alleles3[,2])^2                                                          # create homozygote allele frequencies
  homos2=cbind(as.character(alleles3[,1]),as.character(alleles3[,1]),homos)
  hets=t(combn(alleles3[,2],2))                                                   # create heterozygote allele frequencies
  hetfreq=2*(hets[,1]*hets[,2])
  hetvals=t(combn(as.character(alleles3[,1]),2))                                  # create heterozygote allele names
  hets2=cbind(hetvals,hetfreq)
  gfreqs=rbind(hets2,homos2)                                                      # combine hets and homos and create genotypes
  n=c.capacity                                                                          # sample size of all simulated genotypes (
  gfreqs1=rep(gfreqs[,1],(n*(as.numeric(gfreqs[,3]))))                            # create genotypes(by coloumn, 1 for each allele)
  gfreqs2=rep(gfreqs[,2],(n*(as.numeric(gfreqs[,3]))))
  gtypes=cbind(gfreqs1,gfreqs2)
  gtypes=gtypes[sample(1:length(gtypes[,1]),replace=FALSE),]                          # shuffle genotypes, consider omitting?
  gtypes <- cbind(as.numeric(gtypes[, 1]), as.numeric(gtypes[, 2]))
  OUT=NULL  
  for (l in 1:n.loci){ 
    sg1=gtypes[sample(1:length(gtypes[,1]),n.individs),]
    OUT <- cbind(OUT, sg1)
  }
  #OUT <- cbind(1:length(OUT[, 1]), OUT)
  return(OUT)
}
