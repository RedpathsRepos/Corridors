IsolationMatrices <- function(migration.matrix, n.isolates) {
  ## change migration matrix to allow for isolation in n.isolates sites
  r.isolate <- 1:n.isolates
  migration.matrix[r.isolate, ] <- 0
  return(migration.matrix)
}


