SummaryStats <- function(polygons, distances) {
  ## calculate summary stats for corridors (Fst and heterozygoisty)
  # distances is the output from PropaguleDistances2D
  ## polygons equals the x,y coordinates of the patches, output from PlotPatch
  # need to get individuals based on what patch they are currently in, not what patch they are from, recycle from InsideOut
  # tests whether points are inside or outside of the specified polygons and returns a new distance objects with individuals inside
  polygons <- polygons[1:4, ]  # remove individuals still in corridors
  OUT <- NULL
  for (p in 1:length(polygons[, 1])){
    polygon <- polygons[p, ]
    test1 <- distances[, 3] < polygon[1]  # test x's against left and right
    test2 <- distances[, 3] > polygon[3]
    test3 <- distances[, 4] < polygon[2]  # test y's against top and bottom
    test4 <- distances[, 4] > polygon[8]
    all <- cbind(test1, test2, test3, test4)  # A False across all columns is inside the rectangle
    all[all == "FALSE"] <- 0
    test5 <- rowSums(all)
    retain <- which(test5 == 0)
    if (length(retain) < 10) {next}             # if less than 10 individuals in a patch, move to next patch
    points.in <- distances[retain, ]
    points.in <- cbind(p, points.in)
    OUT <- rbind(OUT, points.in)
  }
  # first coloumn of OUT is the patch they are now residing in. 1=bottomleft, 2=bottomright, 3=topright, 4=topleft
  gdata <- as.data.frame(OUT[, -(2:5)])
  n.each.patch <- data.frame(table(gdata[, 1]))                               # number of individuals in each patch
  n.each.patch <- t(cbind(c(paste(n.each.patch[, 1], n.each.patch[, 2]))))
  if (length(n.each.patch) < 4) {n.each.patch = c(n.each.patch, rep(0, 4-length(n.each.patch)))}  
  n.unique.genotypes <- gdata[, -1]                                            # number of unique genotypes
  n.unique.genotypes <- pasteCols(n.unique.genotypes)                          # requires library plotrix 
  n.unique.genotypes <- length(unique(n.unique.genotypes))
  if (length(unique(gdata[, 1]))==1) {return("OnePopOnly")} else {
    nams = 1:length(gdata[, 1])
    rownames(gdata) <- make.names(nams, unique = TRUE)   # not sure why this formatting was needed, kind of annoying really
    #allele.count(gdata, diploid = TRUE)
    #allelic.richness(gdata, min.n=NULL, diploid = TRUE)
    stats <- basic.stats(gdata, diploid=TRUE)
    stats <- stats$overall
    N <- length(distances[, 1])
    stats <- c(stats, N)
    #asp = indpca(gdata)
    #plot.indpca(asp)
    OUT <- c(stats, n.each.patch, n.unique.genotypes)
    return(OUT)
  }
}  
  