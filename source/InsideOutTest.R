InsideOutTest <- function(polygons, distances) {
  # tests whether points are inside or outside of the specified polygons and returns a new distance objects with individuals inside
  # polygons equals the x,y coordinates of the patches, output from PlotPatch
  # distances equal the x,y cooridnates of the individuals, output from PropaguleDistances2D
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
    points.in <- distances[retain, ]
    OUT <- rbind(OUT, points.in)
  }
  return(OUT)
}  