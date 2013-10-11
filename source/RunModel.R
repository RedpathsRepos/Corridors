RunModel <- function(n.reps, n.generations) {
  # Run Model
  # Really an organizational function to Run hundreds of lines of code  return(capacity)  
  for (r in 1:n.reps){  
    OUT <- NULL
    for (i in 1:n.generations){
      
      if(i == 1) {distances.new <- PropaguleDistances2D(initialize = FALSE, distances = initial$capacity, rs.shape=rs.shape, shape=shape, scale=scale)
      } else {distances.new <- PropaguleDistances2D(initialize = FALSE, distances = capacity, rs.shape=rs.shape, shape=shape, scale=scale)}
      
      #plotx    <- PlotPatch(distances = distances.new, area, node.area, width)
      
      attrition <- InsideOutTest(polygons = initial$plot1, distances = distances.new)
      
      if(i ==1) {capacity  <- CarryingCapacity(distances = attrition, g.types = initial$capacity, c.capacity) 
      } else {capacity  <- CarryingCapacity(distances = attrition, g.types = capacity, c.capacity)}
      
      capacity  <- MutationModule(distances = capacity, mutation.rate)
      
      #plot3    <- PlotPatch(distances = capacity, area, node.area, width)
      
      sum       <- SummaryStats(polygons = initial$plot1, distances = capacity)
      output    <- t(data.frame(c(i, sum$overall)))
      OUT       <- rbind(OUT, output)
      #plot(1,1,xlim = c(-1, n.generations), ylim = c(.8,1.2), cex = 9, pch = 21, bg = "green")
      #points(i, 1)
    }
    OUT <- cbind(r, OUT)
    run.info <- t(as.data.frame(c(area, node.area, width, c.capacity, mu, size, shape, scale, n.alleles, n.loci, mutation.rate)))
    run.info2 <- NULL
    for (z in 1:n.generations) {run.info2 <- rbind(run.info2, run.info)}
    OUT <- cbind(run.info2, OUT)
    colnames(OUT) <- c("area", "node.area", "width", "c.capacity", "mu", "size", "shape", "scale", "n.alleles", "n.loci", "mutation.rate", "rep.no"      ,"generation", "Ho","Hs","Ht","Dst","Htp","Dstp","Fst","Fstp","Fis", "Dest")
    
    write.table(OUT, "sim_stats.txt", col.names = TRUE, row.names = FALSE, sep="\t", append = TRUE)
  }
}