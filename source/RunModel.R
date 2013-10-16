RunModel <- function(n.reps, n.generations) {
  # Run Model
  # Really an organizational function to Run hundreds of lines of code  return(capacity)  
  for (r in 1:n.reps){  
    distances <- PropaguleDistances2D(n.propagules = 10000, std.dev=3, initialize = TRUE, distances = NULL)
    plot1 <- PlotPatch(distances, area, node.area, width)
    
    attrition <- InsideOutTest(polygons = plot1, distances)
    plot2 <- PlotPatch(distances = attrition, area, node.area, width)
      
    capacity <- CarryingCapacity(distances = attrition, g.types = NULL, c.capacity) 
    #plot3 <- PlotPatch(distances = capacity, area, node.area, width)
    genotypes <- InitialAlleleFrequencies(n.individs = c.capacity, n.loci, n.alleles) 
      
    distances.new <- cbind(capacity, genotypes)                   # individual ids are a little strange because of carrying capacity
    capacity <- cbind(1:length(capacity[, 1]), distances.new[order(distances.new[, 2]), -1])  # this fixes the above
      
    OUT <- NULL
    for (i in 1:n.generations){
      distances.new <- PropaguleDistances2D(n.propagules, std.dev, initialize = FALSE, distances = capacity, rs.shape, shape, scale)
      #plotx <- PlotPatch(distances = distances.new, area, node.area, width)
      attrition <- InsideOutTest(polygons = plot1, distances = distances.new)
      capacity <- CarryingCapacity(distances = attrition, g.types = capacity, c.capacity) 
      capacity <- MutationModule(distances = capacity, mutation.rate)
      plot3 <- PlotPatch(distances = capacity, area, node.area, width)
      if(length(capacity[, 1]) < 50) {break}
      sum <- SummaryStats(polygons = plot1, distances = capacity)
      if(sum[1]=="OnePopOnly") {next}
      output <- t(data.frame(c(i, sum)))
      OUT <- rbind(OUT, output) 
    }
    OUT <- cbind(r, OUT)
    run.info <- t(as.data.frame(c(area, node.area, width, c.capacity, mu, size, shape, scale, n.alleles, n.loci, mutation.rate)))
    run.info2 <- NULL
    for (z in 1:length(OUT[, 1])) {run.info2 <- rbind(run.info2, run.info)}
    OUT <- cbind(run.info2, OUT)
    #colnames(OUT) <- c("area", "node.area", "width", "c.capacity", "mu", "size", "shape", "scale", "n.alleles", "n.loci", "mutation.rate", "rep.no","generation", "Ho","Hs","Ht","Dst","Htp","Dstp","Fst","Fstp","Fis", "Dest", "N")
    write.table(OUT, "sim_stats.txt", col.names = FALSE, row.names = FALSE, sep="\t", append = TRUE)
  }
}