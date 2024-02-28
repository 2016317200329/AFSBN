# This is a fork from gy's ga-hc tweak alpha \
# Author: guoyang@webmail.hzau.edu.cn, wlg, wyj, fy, gyg, djw \
# Version: 0.0.5 \
# Time: 2017-11-24 \
# NOTE: cat command should be included in a if(debug) snippet when program fixed
# unified hill climbing implementation (both optimized and by spec).
hill.climbing = function(x, start, whitelist, blacklist, score, extra.args,
                         restart, perturb, max.iter, maxp, optimized, debug = FALSE) {
  
  cat("=======$gy: initial start============\n")
  print(start)
  cat("=======$gy: initial whitelist============\n")
  print(whitelist)
  cat("=======$gy: initial blacklist============\n")
  print(blacklist)
  cat("=======$gy: initial extra.args============\n")
  print(extra.args)
  cat("-------------------------------------------hjfok1---------------------------------------------------------------------------\n")
  #gy: Initilization
  # cache nodes' labels.
  nodes = names(x)
  # cache the number of nodes.
  n.nodes = length(nodes)
  
  #need to avoid print() without enough output++++++++++++++++++++++++++
  options(max.print=1000000)
  
  # set the iteration counter.
  iter = 0
  # check whether the score is score-equivalent.
  score.equivalence = is.score.equivalent(score, nodes, extra.args)
  # check whether the score is decomposable.
  score.decomposability = is.score.decomposable(score, extra.args)
  # allocate the cache matrix.
  cache = matrix(0, nrow = n.nodes, ncol = n.nodes)
  cache_ref = matrix(0, nrow = n.nodes, ncol = n.nodes)#+++++++++++++++++++++++++++++++++++++++++++++
  # nodes to be updated (all of them in the first iteration).
  updated = seq_len(n.nodes) - 1L
  
  cat("\n===========$gy: all the initiate value are presented below============\n")
  cat("nodes:", nodes,"\n");
  cat("n.nodes:", n.nodes,"\n");
  cat("score.equivalence:", score.equivalence,"\n");
  cat("score.decomposability:", score.decomposability,"\n");
  cat("updated:", updated,"\n");
  
  cat("-------------------------------------------hjfok2---------------------------------------------------------------------------\n")
  
  #set the population size in all
  #gy：`restart` in the `hc()` represents the population size, which will be assigned here to population.size
  population.size = restart #gy：Use 20~100 when generating the initial population later.
  
  cat("\n~~~~~~~~~~~~~~~~~~~~~$gy: all the initiate value for ga are presented below~~~~~~\n")
  cat("population.size(50):", population.size,"\n");
  
  # set the reference score.
  #gy： Initilize scoring
  #gy：`per.node.score` could be found in file of per.node.score.c
  reference.score = per.node.score(network = start, score = score,
                                   targets = nodes, extra.args = extra.args, data = x)
  
  cat("\n~~~~~~~~~~~~~~~~~~~~~$gy: reference.score are presented below(network = start)~~~~~~\n")
  print(reference.score)
  
  
  #gy：convert the blacklist to an adjacency matrix for easy use.
  if (!is.null(blacklist))
    blmat = arcs2amat(blacklist, nodes)
  else
    blmat = matrix(0L, nrow = n.nodes, ncol = n.nodes)
  #gy：convert the whitelist to an adjacency matrix for easy use.
  if (!is.null(whitelist))
    wlmat = arcs2amat(whitelist, nodes)
  else
    wlmat = matrix(0L, nrow = n.nodes, ncol = n.nodes)
  
  # cat("=======$gy: after adjacent transform: whitelist============\n")
  # 	print(wlmat)
  
  # cat("=======$gy: after adjacent transform: blacklist============\n")
  # 	print(blmat)
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  amat_ref = arcs2amat(start$arcs, nodes)
  .Call("score_cache_fill",
        nodes = nodes,
        data = x,
        network = start,#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        #network = chowliu.start,#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        score = score,
        extra = extra.args,
        reference = reference.score,
        equivalence = score.equivalence && optimized,
        decomposability = score.decomposability,
        updated = (if (optimized) updated else seq(length(nodes)) - 1L),
        amat = amat_ref,#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        #amat = chowliu.amat,#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        cache = cache_ref,
        blmat = blmat,
        debug = debug)
  cat("=======$gy: after score_cache_fill, cache_ref becomes========\n")
  print(cache_ref)
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #---------------------beginning generate the primary individual-------------------------
  #get a network using MWST(an undirected graph)
  chowliu.start = chow.liu(x)#gy：example: use mydata(Alarm1_s500_v1.txt), see chowliu.pdf in ./8-31
  #the matirx of the network chouliu.start
  chowliu.amat = arcs2amat(chowliu.start$arcs,nodes)
  if(debug)
  {
    cat("* get network using MWST:\n")
    print(chowliu.start)
  }
  cat("\n============initial chowliu.amat============================\n")
  print(chowliu.amat)
  #gy: `start` is null now. `ga` will pass `mydata` to `x` not `start`
  #the matrix of the original graph (start)
  #amat = arcs2amat(start$arcs, nodes)
  amat = arcs2amat(chowliu.start$arcs, nodes)#make a copy
  #amat1 = arcs2amat(start$arcs,nodes)
  #start1 = chowliu.start
  #gy： Score network using MWST
  # reference.score = per.node.score(network = chowliu.start, score = score,
  # 					targets = nodes, extra.args = extra.args, data = x)
  
  # cat("\n~~~~~~~~~~~~~~~~~~~~~$gy: reference.score are presented below(network = chowliu.start)~~~~~~\n")
  # 	print(reference.score)
  cat("\n~~~~~~~~~~~~~~~~~~~~~$gy: score are presented below(network = chowliu.start)~~~~~~\n")
  print(score)
  # cat("\n~~~~~~~~~~~~~~~~~~~~~$gy: cache are presented below(network = chowliu.start)~~~~~~\n")
  # 	print(cache)
  # cat("\n============amat: arcs2amat(start$arcs, nodes)===============\n")
  # 	print(amat)
  #gy：.call() -> Functions to pass R objects to compiled C/C++ code that has been loaded into R.
  #gy：`score_cache_fill` is in hc.cache.lookup
  #gy：The two nodes of all the edges are used as parent and child nodes respectively for scoring.
  # Whichever directed edge scoring higher is retained and the resulting individual is used as the initial individual.
  #-----------------------------------------------------------------------
  for(i in seq(from=2,to=n.nodes,by=1))#i = 2,3,4,...,37
  {
    for(j in seq(from=1,to=i-1,by=1))#j = 1,2,3,...,36
    {
      if(chowliu.amat[i,j])
      {
        if(runif(1,0,1)>runif(1,0,1))
          chowliu.amat[j,i]=0L
        else
          chowliu.amat[i,j]=0L
      }
    }
  }
  amat(chowliu.start)=chowliu.amat
  reference.score = per.node.score(network = chowliu.start, score = score,
                                   targets = nodes, extra.args = extra.args, data = x)
  cat("\n~~~~~~~~~~~~~~~~~~~~~$gy: reference.score are presented below(network = chowliu.start)~~~~~~\n")
  print(reference.score)
  
  cat("\n============chowliu.x after runif============================\n")
  print(chowliu.start)
  print(chowliu.amat)
  #-----------------------------------------------------------------------
  .Call("score_cache_fill",
        nodes = nodes,
        data = x,
        #network = start,#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        network = chowliu.start,#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        score = score,
        extra = extra.args,
        reference = reference.score,
        equivalence = score.equivalence && optimized,
        decomposability = score.decomposability,
        updated = (if (optimized) updated else seq(length(nodes)) - 1L),
        #amat = amat,#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        amat = chowliu.amat,#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        cache = cache,
        blmat = blmat,
        debug = debug)
  
  cat("=======$gy: after score_cache_fill, cache becomes========\n")
  print(cache)
  print(chowliu.amat)
  print(amat)
  
  for(i in seq(from=2,to=n.nodes,by=1))#i = 2,3,4,...,37
  {
    for(j in seq(from=1,to=i-1,by=1))#j = 1,2,3,...,36
    {
      if(amat[i,j])
      {
        #cache_a = cache[i,j]
        #cache_b = cache[j,i]
        cat("\ncache[",i,",",j,"]=",cache[i,j]," ~?~ cache[",j,",",i,"]",cache[j,i],"\n")
        cat("abs(cache[i,j])-abs(cache[j,i])=",abs(cache[i,j])-abs(cache[j,i]),"\n")
        
        # if((abs(abs(cache_a)-abs(cache_b)) < 1e-11) & cache_a < 0){# Get the absolute value
        # 	chowliu.amat[i,j]=1L#+++++++++++++++++++++++++++++++++++++++++++++++
        # 	chowliu.amat[j,i]=0L
        # 	cat("cache[",i,",",j,"]~=cache[",j,",",i,"], choose cache[",i,",",j,"]\n")
        # }
        
        if(cache[i,j] > cache[j,i]){# Choose the one with higher score
          chowliu.amat[i,j]=1L#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          chowliu.amat[j,i]=0L
          cat("cache[",i,",",j,"]>cache[",j,",",i,"], choose cache[",i,",",j,"]\n")
        }
        
        else{
          cat("abs(cache[i,j]) - abs(cache_ref[i,j])=",abs(cache[i,j]) - abs(cache_ref[i,j]),"\n")
          if((abs(cache[i,j]) - abs(cache_ref[i,j]) >= 0) | ((abs(abs(cache[i,j])-abs(cache_ref[i,j])) < 1e-7) & cache[i,j] < 0) | (abs(abs(cache[i,j])-abs(cache_ref[i,j])) < abs(abs(cache[j,i])-abs(cache_ref[j,i])))){
            
            chowliu.amat[i,j]=1L
            chowliu.amat[j,i]=0L
            cat("abs(cache-->",cache[i,j],") >= abs(cache_ref-->",cache_ref[i,j],"), or ","abs(",cache[i,j],") is closer to (",cache_ref[i,j],"), choose cache[",i,",",j,"]\n")
          }
          else{
            chowliu.amat[i,j]=0L#+++++++++++++++++++++++++++++++++++++++++++++++
            chowliu.amat[j,i]=1L
            cat("cache[",i,",",j,"]<cache[",j,",",i,"], choose cache[",j,",",i,"]\n")
          }
        }
        cat("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
      }
    }
  }
  cat("=======$gy: after change, chowliu.amat becomes========\n")
  print(chowliu.amat)
  amat(chowliu.start) = chowliu.amat
  if(debug)
  {
    cat("* the primary individual is:\n")
    print(chowliu.start)
  }
  
  #---------------------ends of generate the primary individual-------------------------
  
  #the population used to storege all the individual 
  population = array(-1L,dim = c(n.nodes,n.nodes,population.size))
  #gy：Storage container for the initial population in the format n.nodes*n.nodes population.size matrix
  
  #---------------------beginning generate the primary polulation-------------------------
  .Call("set_rand_seed")
  for(i in 1:population.size)
  {
    #ga_generate_ind 在hc.cache.lookup中
    .Call("ga_generate_ind",
          amat = chowliu.amat,
          nodes = nodes,
          wlmat = wlmat,
          blmat = blmat,
          debug = debug)
    # print(i)#gy：previously omitted by hjf
    # print(chowliu.amat)#gy：previously omitted by hjf
    population[,,i] = chowliu.amat
  }
  
  #---------------------ends of generate the primary polulation-------------------------
  
  if(debug)#gy：Output initial population
  {
    cat("* the primary population is:\n")
    print(population)
  }
  
  #keep the score of each individual
  population.score = array(-1,population.size)#gy：Initialize an all-1 array of 1*population.size
  #used to calculate the score of each individual
  network.temp = chowliu.start #gy：Use the initial population to initialise network.temp
  
  
  #compute score of each individual in population
  for(i in 1:population.size)
  {
    amat(network.temp) = population[,,i]
    population.score[i] = score(network.temp,x,type = score)# Scores here are not counted before `mydata`****************************************
    cat("compute score of each individual in population, right now is No.",i," , score = "
        ,population.score[i],"\n")
  }
  cat("after loop, show the score of each individual in population:\n") #gy：previously omitted by hjf
  print(population.score) #gy：previously omitted by hjf
  
  
  cat("-------------------------------------AFSA START-----------------------","\n")	
  
  best.fish <- matrix(0L, nrow = n.nodes, ncol = n.nodes)
  best.fish.score <- -Inf
  f_n.score <- matrix(0L,nrow = 1, ncol = 3)
  now.num <- 0
  iter.num <- 1
  f_n <- array(-1L,dim = c(n.nodes,n.nodes,3))
  source("D:/software/R/new_bnlearn/bnlearn/R/argSet.R")
  
  #network.temp = chow.liu(mydata)
  
  source("D:/software/R/new_bnlearn/bnlearn/R/distance.R")
  af.distance <- matrix(0L,nrow = 50,ncol = 50)
  af.distance = cal.distance(af.distance,population,population.size,n.nodes,debug = TRUE)
  #print(af.distance) # af.distance is already output in the function above without printing it.
  
  #wyj: Adding a bunch of `source` avoids compiling each time when change the four R files and `cal.distance` above.
  #wyj: But to change any contents of the `hill-climbing.R`, you have to compile it.
  source("D:/software/R/new_bnlearn/bnlearn/R/prey.R")
  source("D:/software/R/new_bnlearn/bnlearn/R/swarm.R")
  source("D:/software/R/new_bnlearn/bnlearn/R/follow.R")
  #source("D:/software/R/new_bnlearn/bnlearn/R/bulletin.R")
  bulletin <- function(x, f_n, f_n.score, cur, n.nodes, population, population.score, af.distance, population.size, score, network.temp){
    cat("~~~~~~~~~~~~~~~~~~~wlg bulletin of No.",cur," fish~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    len <- n.nodes
    maxi <- 1
    cat("fish1:")
    print(f_n[,,1])
    cat("fish2:")
    print(f_n[,,2])
    cat("fish3:")
    print(f_n[,,3])
    #wlg: Score the current fish
    amat(network.temp) = population[,,cur]
    cur.score = score(network.temp,x,type = score)
    cat("compute score of this fish, score = ",cur.score,"\n")
    # Score fish performing the three actions.
    for(i in 1:3)
    {
      amat(network.temp) = f_n[,,i]
      f_n.score[i] = score(network.temp,x,type = score)
      cat("compute score of each individual in f_n, right now is No.",i," , score = ",f_n.score[i],"\n")
    }
    #wlg: Find the fish with highest score
    max <- f_n.score[1]
    for(i in 2:3){
      if(max < f_n.score[i]){
        max <- f_n.score[i]
        maxi <- i
      }
    }
    cat("fish:",cur," bulletin maxi:",i)
    
    #wlg: If the current fish scores higher than max, stay still
    if(cur.score > max){
      max <- cur.score
      if(max > best.fish.score){
        best.fish.score <<- max
        best.fish <<- population[,,cur]
      }
      cat("cur.score > max")
      print(best.fish)
    }else{           #wlg: Or move
      population[,,cur] <- f_n[,,maxi]
      population.score[cur] <- max
      #wlg: Update the distance
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
      #wlg: Update the bulletin
      if(max > best.fish.score){
        best.fish.score <<- max
        best.fish <<- f_n[,,maxi]
      }
      cat("cur.score < max")
      print(best.fish)
    }
    cat("~~~~~~~~~~~~~~~~~~~~~~~~wlg bulletin of No.",cur," fish ends~~~~~~~~~~~~~~~~~~~~")
  }
  while(now.num < iter.num){
    for (cur in 1:population.size) {
      f_n[,,1] <- prey(x,cur,population,population.size,population.score,n.nodes,af.distance,score,trynumber,visual,discount,network.temp,debug=TRUE)
      cat("AFSA-fish1:")
      print(f_n[,,1])
      f_n[,,2] <- swarm(x,cur,population,population.size,population.score,n.nodes,af.distance,score,trynumber,visual,discount,middle.fish.discount,network.temp,delta,debug=TRUE)
      cat("AFSA-fish2:")
      print(f_n[,,2])
      f_n[,,3] <- follow(x,cur,population,population.size,population.score,n.nodes,af.distance,score,trynumber,visual,discount,network.temp,delta,debug=TRUE)
      cat("AFSA-fish3:")
      print(f_n[,,3])
      bulletin(x, f_n, f_n.score, cur, n.nodes, population, population.score, af.distance, population.size, score, network.temp)
    }
    now.num <- now.num + 1
  }
  
  
  cat("-------------------------------------AFSA ENDS-----------------------","\n")		
  cat("Final best.fish")
  print(best.fish)
  network.temp.best.fish = network.temp
  amat(network.temp.best.fish) = best.fish
  cat("best.fish:","\n")
  print(network.temp.best.fish)
  return (network.temp.best.fish)
  
}#HILL.CLIMBING

