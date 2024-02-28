#name:bulletin1.0
#author:wlg
#time:2018/4/12

bulletin <- function(x, f_n, f_n.score, cur, n.nodes, population, population.score, af.distance, population.size, score, network.temp){
  cat("~~~~~~~~~~~~~~~~~~~bulletin of No.",cur," fish~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  len <- n.nodes
  maxi <- 1
  cat("fish1:")
  print(f_n[,,1])
  cat("fish2:")
  print(f_n[,,2])
  cat("fish3:")
  print(f_n[,,3])
  # Score the current fish
  amat(network.temp) = population[,,cur]
  cur.score = score(network.temp,x,type = score)
  cat("compute score of this fish, score = ",cur.score,"\n")
  # Score the fish after prey, swarm, and follow
  for(i in 1:3)
  {
    amat(network.temp) = f_n[,,i]
    f_n.score[i] = score(network.temp,x,type = score)
    cat("compute score of each individual in f_n, right now is No.",i," , score = ",f_n.score[i],"\n")
  }
  # Find the fish with the highest score
  max <- f_n.score[1]
  for(i in 2:3){
    if(max < f_n.score[i]){
      max <- f_n.score[i]
      maxi <- i
    }
  }
  cat("fish:",cur," bulletin maxi:",i)
  
  # If current fish scores higher than max, the current fish stays still.

  if(cur.score > max){
    max <- cur.score
    if(max > best.fish.score){
      best.fish.score <<- max
      best.fish <<- population[,,cur]
    }
    cat("cur.score > max")
    print(best.fish)
  }else{           # Or the current fish has to move
    population[,,cur] <- f_n[,,maxi]
    population.score[cur] <- max
    # Update the distance
    for(i in 1:population.size){
      graph.population.cur <- graph.adjacency(population[ , ,cur])
      cur.order <- topo_sort(graph.population.cur)
      if(i != cur){
        graph.population.i <- graph.adjacency(population[ , ,i])
        i.order <- topo_sort(graph.population.i)
        distance = 0
        for(k in 1:len){
          if(cur.order[k] != i.order[k])
            distance = distance + 1
          af.distance[cur,i] = distance
          af.distance[i,cur] = distance
        }
      }
      else if(cur == i)
        af.distance[cur,i] = Inf
    }
    #if(debug){
    cat("distance:\n")
    print(af.distance)
    #}
    # Update the bulletin
    if(max > best.fish.score){
      best.fish.score <<- max
      best.fish <<- f_n[,,maxi]
    }
    cat("cur.score < max")
    print(best.fish)
  }
  cat("~~~~~~~~~~~~~~~~~~~~~~~~bulletin of No.",cur," fish ends~~~~~~~~~~~~~~~~~~~~")
}