PlotPatch <- function(distances, area, node.area) {
  ## plots the 2d distribution of individuals, the nodes, and the corridors
  # distances is the output from PropaguleDistances2D
  # area is the area for the entire metapopulation
  # node.area is the area for each of the nodes (i.e., reserves)
  
  # seperate pops
  pop1 <- distances[distances[, 2] == 1, 1:4]
  pop2 <- distances[distances[, 2] == 2, 1:4]
  pop3 <- distances[distances[, 2] == 3, 1:4]
  pop4 <- distances[distances[, 2] == 4, 1:4]  
  
  # add nonsense points if population has gone extinct
  if(length(pop1) < 4) { pop1 <- matrix(-4, 4, 4)}
  if(length(pop2) < 4) { pop2 <- matrix(-4, 4, 4)}
  if(length(pop3) < 4) { pop3 <- matrix(-4, 4, 4)}
  if(length(pop4) < 4) { pop4 <- matrix(-4, 4, 4)}

  # reformat if only a single individual
  if(length(pop1) == 4) { pop1 <- as.matrix(t(pop1))}
  if(length(pop2) == 4) { pop2 <- as.matrix(t(pop2))}
  if(length(pop3) == 4) { pop3 <- as.matrix(t(pop3))}
  if(length(pop4) == 4) { pop4 <- as.matrix(t(pop4))}
  
  # plot organisms
  plot(pop1[, 3], pop1[, 4], xlab = "Distance (km)", ylab = "Distance (km)", xlim = c(min(distances[, 3])-1, max(distances[, 3]+1)), ylim = c(min(distances[, 4]-1), max(distances[, 4]+1)))
  points(pop2[, 3],pop2[, 4], col = "blue")
  points(pop3[, 3],pop3[, 4],, col = "green")
  points(pop4[, 3],pop4[, 4], col = "red")
  
  # plot node boundaries (reserves)
  x <- sqrt(area)
  nodes <- c(c(x, x),c(x*2, x),c(x*2, x*2),c(x, x*2))  # find center of each deme
  nodes <- c(c(0, 0),c(x, 0),c(x, x),c(0, x))  # node locations: bottom left, bottom right, top right, top left
  node.length <- sqrt(node.area)/2
  xs <- nodes[seq(1, length(nodes), 2)]
  ys <- nodes[seq(2, length(nodes), 2)]
  x1 <- xs-node.length 
  x2 <- xs+node.length
  y1 <- ys-node.length
  y2 <- ys+node.length
  point1 <- cbind(x1, y1)  # bottom left, right, top right, left
  point2 <- cbind(x2, y1)
  point3 <- cbind(x2, y2)
  point4 <- cbind(x1, y2)
  segments(point1[, 1], point1[, 2], point2[, 1], point2[, 2], col="purple", lwd = 3)
  segments(point3[, 1], point3[, 2], point2[, 1], point2[, 2], col="purple", lwd = 3)
  segments(point3[, 1], point3[, 2], point4[, 1], point4[, 2], col="purple", lwd = 3)
  segments(point1[, 1], point1[, 2], point4[, 1], point4[, 2], col="purple", lwd = 3)
  
  square1 <- c(point1[1, 1], point1[1, 2], point2[1, 1], point2[1, 2], point2[1, 1], point3[1, 2], point4[1, 1], point4[1, 2])
  square2 <- c(point1[2, 1], point1[2, 2], point2[2, 1], point2[2, 2], point2[2, 1], point3[2, 2], point4[2, 1], point4[2, 2])
  square3 <- c(point1[3, 1], point1[3, 2], point2[3, 1], point2[3, 2], point2[3, 1], point3[3, 2], point4[3, 1], point4[3, 2])
  square4 <- c(point1[4, 1], point1[4, 2], point2[4, 1], point2[4, 2], point2[4, 1], point3[4, 2], point4[4, 1], point4[4, 2])
  
  # plot corridors
  # bottom corridor
  width <- 0.5
  xleft1 <- point2[1, 1]
  xright1 <- point2[4, 2]
  ytop1 <- ys[1]+width
  ybottom1 <- ys[1]-width
  rect(xleft1, ybottom1, xright1, ytop1, border="purple", lwd=2)
  square5 <- c(xleft1, ybottom1, xright1, ybottom1, xright1, ytop1, xleft1, ytop1)
    
  # top corridor
  xleft1 <- point2[1, 1]
  xright1 <- point2[4, 2]
  ytop1 <- ys[3]+width
  ybottom1 <- ys[3]-width
  rect(xleft1, ybottom1, xright1, ytop1, border="purple", lwd=2)
  square6 <- c(xleft1, ybottom1, xright1, ybottom1, xright1, ytop1, xleft1, ytop1)
  
  # left corridor
  xleft1 <- xs[2]-width
  xright1 <- xs[2]+width
  ytop1 <- point2[3, 2]
  ybottom1 <- point3[1, 2]
  rect(xleft1, ybottom1, xright1, ytop1, border="purple", lwd=2)
  square7 <- c(xleft1, ybottom1, xright1, ybottom1, xright1, ytop1, xleft1, ytop1)
  
  # right corridor
  xleft1 <- xs[1]-width
  xright1 <- xs[1]+width
  ytop1 <- point2[3, 2]
  ybottom1 <- point3[1, 2]
  rect(xleft1, ybottom1, xright1, ytop1, border="purple", lwd=2)
  square8 <- c(xleft1, ybottom1, xright1, ybottom1, xright1, ytop1, xleft1, ytop1)
  
  coords <- rbind(square1, square2, square3, square4, square5, square6, square7, square8)
  return(coords)
}


