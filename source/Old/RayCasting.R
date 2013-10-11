RayCasting <- function(distances, plot1) {
  ## removes points that are outside nodes and corridors
  ## Ray-casting implementation from rosettacode.org/wiki/Ray-casting_algorithm
  #  
  
  point_in_polygon <- function(polygon, p) {
    count <- 0
    for(side in polygon) {
      if ( ray_intersect_segment(p, side) ) {
        count <- count + 1
      }
    }
    if ( count %% 2 == 1 )
      "INSIDE"
    else
      "OUTSIDE"
  }
  
  ray_intersect_segment <- function(p, side) {
    eps <- 0.0001
    a <- side$A
    b <- side$B
    if ( a$y > b$y ) {
      a <- side$B
      b <- side$A
    }
    if ( (p$y == a$y) || (p$y == b$y) ) {
      p$y <- p$y + eps
    }
    if ( (p$y < a$y) || (p$y > b$y) )
      return(FALSE)
    else if ( p$x > max(a$x, b$x) )
      return(FALSE)
    else {
      if ( p$x < min(a$x, b$x) )
        return(TRUE)
      else {
        if ( a$x != b$x )
          m_red <- (b$y - a$y) / (b$x - a$x)
        else
          m_red <- Inf
        if ( a$x != p$x )
          m_blue <- (p$y - a$y) / (p$x - a$x)
        else
          m_blue <- Inf
        return( m_blue >= m_red )
      }
    }
  }
  
  point <- function(x,y) list(x=x, y=y)
  
  # pts = list(p1, p2, ... )... coords
  # segs = list(c(1,2), c(2,1) ...) indices
  createPolygon <- function(pts, segs) {
    pol <- list()
    for(pseg in segs) {
      pol <- c(pol, list(list(A=pts[[pseg[1]]], B=pts[[pseg[2]]])))
    }
    pol
  }
  
  pts <-  list(point(plot1[1,1],plot1[1, 2]), point(plot1[1,3],plot1[1,4]), point(plot1[1,5],plot1[1,6]), point(plot1[1,7],plot1[1,8]))# the boundaries of test node i
  pts2 <- list(point(plot1[2,1],plot1[2, 2]), point(plot1[2,3],plot1[2,4]), point(plot1[2,5],plot1[2,6]), point(plot1[2,7],plot1[2,8]))# the boundaries of test node i
  pts3 <- list(point(plot1[3,1],plot1[3, 2]), point(plot1[3,3],plot1[3,4]), point(plot1[3,5],plot1[3,6]), point(plot1[3,7],plot1[3,8]))# the boundaries of test node i
  pts4 <- list(point(plot1[4,1],plot1[4, 2]), point(plot1[4,3],plot1[4,4]), point(plot1[4,5],plot1[4,6]), point(plot1[4,7],plot1[4,8]))# the boundaries of test node i
  pts5 <- list(point(plot1[5,1],plot1[5, 2]), point(plot1[5,3],plot1[5,4]), point(plot1[5,5],plot1[5,6]), point(plot1[5,7],plot1[5,8]))# the boundaries of test node i
  pts6 <- list(point(plot1[6,1],plot1[6, 2]), point(plot1[6,3],plot1[6,4]), point(plot1[6,5],plot1[6,6]), point(plot1[6,7],plot1[6,8]))# the boundaries of test node i
  pts7 <- list(point(plot1[7,1],plot1[7, 2]), point(plot1[7,3],plot1[7,4]), point(plot1[7,5],plot1[7,6]), point(plot1[7,7],plot1[7,8]))# the boundaries of test node i
  pts8 <- list(point(plot1[8,1],plot1[8, 2]), point(plot1[8,3],plot1[8,4]), point(plot1[8,5],plot1[8,6]), point(plot1[8,7],plot1[8,8]))# the boundaries of test node i
  
  
  polygons = NULL
  polygons <- list(
    square = createPolygon(pts, list(c(1,2), c(2,3), c(3,4), c(4,1))),
    square2 = createPolygon(pts2, list(c(1,2), c(2,3), c(3,4), c(4,1))),
    square3 = createPolygon(pts3, list(c(1,2), c(2,3), c(3,4), c(4,1))),
    square4 = createPolygon(pts4, list(c(1,2), c(2,3), c(3,4), c(4,1))),
    square5 = createPolygon(pts5, list(c(1,2), c(2,3), c(3,4), c(4,1))),
    square6 = createPolygon(pts6, list(c(1,2), c(2,3), c(3,4), c(4,1))),
    square7 = createPolygon(pts7, list(c(1,2), c(2,3), c(3,4), c(4,1))),
    square8 = createPolygon(pts8, list(c(1,2), c(2,3), c(3,4), c(4,1)))
  )  # creates the node
  
  distance.ids <- distances[, 1]
  distances2 <- distances[, -1]                                                  # combine all test points into 2 columns

  
  testpoints = NULL
  for (i in 1:length(distances2[, 1])) {                                         # format testpoints 
    testpoints[i] <- list(point(distances2[i, 1], distances2[i, 2]))
  }
  
  OUT = NULL
  for(p in testpoints) {
    for(polysi in 1:length(polygons)) {
      test = point_in_polygon(polygons[[polysi]], p)
      position = cbind(p$x, p$y, test) 
      OUT = rbind(OUT, position)
    }
  }
  
  OUT2 <- cbind(sort(rep(distance.ids,length(polygons))), OUT[, 1], OUT[, 2], OUT[, 3])    # add pop ids back to data
  OUT2 <- OUT2[which(OUT2[, 4]=="INSIDE"), ] 
  OUT3 <- cbind(as.numeric(OUT2[, 1]), as.numeric(OUT2[, 2]), as.numeric(OUT2[, 3]))
  return(OUT3)
}  

  
  