# Author: wyj \
# Version: 0.0.2 \
# Time: 2018-4-11 \

cat("-----distance is callable!","\n")
cal.distance = function(af.distance,population,population.size,n.nodes,debug = FALSE){
  
  if(debug)
  {
    cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~wyj distance~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
  }
  #af.distance is the latests distance maxtrix (50*50) which will be output
  af.distance <- matrix(0L,nrow = population.size,ncol = population.size)
  if(debug)
  {
    cat("order:\n")
  }
  for(i in 1:population.size)
  {
  # The outer `for` will run for 50 rounds. Since af.distance is symmetrical, there will be calculation of 25 rounds. 
    #graph.population.i is transformed from the i-th adj matrix, `topo_sort` is only applicable for graph.
    graph.population.i <- graph.adjacency(population[,,i])
    #i.order是第i个图的节点顺序
    i.order <- topo_sort(graph.population.i)
    if(debug)
    {
      cat(i)
      cat(":")
      cat(i.order)
      cat("\n")
    }
    for(j in 1:population.size){
      # To save time, the calculation will be carried for population.size/2 times
      if(i < j){
        graph.population.j <- graph.adjacency(population[,,j])
        j.order <- topo_sort(graph.population.j)
        # Initialize distance matrix (int) for counting.
        distance = 0
        # Compare each node in graph i and graph j respectively. Increase `distance` by 1 when inconsistency occurs. 
        for(k in 1:n.nodes){
          if(i.order[k] != j.order[k])
            distance = distance+1
          af.distance[i,j] = distance
        }#for
      }#if
      # Assign
      else if(i > j)
        af.distance[i,j] = af.distance[j,i]
      # Self-to-self distance is infinite
      else if(j == i)
        af.distance[i,j] = Inf
    }#for
  }#for
  if(debug){
    cat("distance:\n")
    print(af.distance)
  }
  if(debug)
  {
     cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~wyj distance end~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
  }
  return(af.distance)
}