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
    if (length(retain) < 15) {next}             # if less than 15 individuals in a patch, move to next patch
    points.in <- distances[retain, ]
    points.in <- cbind(p, points.in)
    OUT <- rbind(OUT, points.in)
  }
  # first coloumn of OUT is the patch they are now residing in. 1=bottomleft, 2=bottomright, 3=topright, 4=topleft
  gdata <- as.data.frame(OUT[, -(2:5)])
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
    OUT <- stats
    return(OUT)
  }
}  
  