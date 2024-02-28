# name:swarm
# author:wlg1996@webmail.hzau.edu.cn, wyj
# time:5/7/2018
# update time:2018-6-10

swarm=function(x,cur,population,population.size,population.score,n.nodes,af.distance,score,trynumber,visual,discount,middle.fish.discount,network.temp,delta,debug=FALSE){
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~wlg:swarm of No.",cur," fish is calling~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n")
  # wlg:Initialization
  len=n.nodes
  population.cur=population[,,cur]
  num.neighbor <- 0
  random_tmp = network.temp
  
  # wlg: Travele through each neighbour to find the central fish
  judge <- matrix(0L, nrow = len, ncol = len)
  for(i in 1:population.size){
    if(af.distance[cur,i] <= visual && i != cur){
      num.neighbor = num.neighbor + 1
      for ( m in 1:(len*len)){
        j = floor(m/len)+1
        k = m - (j-1)*len
        if((m - (j-1)*len)==0){
          k <- len
        }
        if((m %% len) == 0){
          j = j - 1
        }
        if(population[j,k,i] == 1){
          judge[j,k] <- judge[j,k] + 1
        }
      }
    }
  }
  #cat("judge:\n")
  #print(judge)
  cat("neighbor num:",num.neighbor,"\n")
  
  #wlg：Generate the central fish adjacency matrix based on the result of traveling
  middle_fish_adj <- matrix(0L, nrow = len, ncol = len)
  for ( i in 1:(len*len)){
    j = floor(i/len)+1
    k = i - (j-1)*len
    if((i - (j-1)*len)==0){
      k <- len
    }
    if((i %% len) == 0){
      j = j - 1
    }
    if(judge[j,k] >= num.neighbor * middle.fish.discount){
      middle_fish_adj[j,k] <- 1
      graph.population.mid <- graph.adjacency(middle_fish_adj) # Convert the current fish matrix to an adjacency matrix.
      midflag = is_dag(graph.population.mid)
      if(!midflag){ # wlg：To prevent circles
        middle_fish_adj[j,k] <- 0
      }
    }
  }
  cat("middle.dish.adj:",middle_fish_adj,"\n")
  
  #wlg: Score the central fish
  #random_tmp=chow.liu(x)
  amat(random_tmp) = middle_fish_adj 
  score_new = score(random_tmp,x,type = score)
  cat("score_new:",score_new,"\n")
  #wlg: Move towards the central fish if central place is full of food and not crowded
  if(score_new / num.neighbor > delta*population.score[cur]) # Change again
  {
    difcount <- 0L
    difference = array()
    for ( i in 1:(len*len)){
      j = floor(i/len)+1
      k = i - (j-1)*len
      if((i - (j-1)*len)==0){
        k <- len
      }
      if((i %% len) == 0){
        j = j - 1
      }
      if(population.cur[j,k]!=middle_fish_adj[j,k])
      {
        difcount = difcount + 1;
        difference = append(difference,i,after = length(difference))
      }
    }
    difference <- difference[2:length(difference)]
    
    try_max = 10
    try_num = 0
    flag = 1
    while (flag){
      one_step = sample(difference,floor(difcount*discount),FALSE)  
      try_num = try_num + 1
      for(k in one_step){
        i = floor(k/len) + 1
        j = k - (i-1) * len
        if((k - (i-1)*len)==0){
          j <- len
        }
        if((k %% len) == 0){
          i = i - 1
        }
        population.cur[i,j] = middle_fish_adj[i,j]
      }
      graph.population.cur <- graph.adjacency(population.cur)
      isdag = is_dag(graph.population.cur)
      if(try_num < try_max && !isdag) {
        flag = 1
      }else{
        flag = 0
      }
    }
    
    #wlg: If the max number of endurance is reached, move the current fish directly to the central fish and then move it randomly
    if(try_num == try_max){
      population.ran <- middle_fish_adj
      p<-sample(1:len,1)
      q<-sample(1:len,1)
      while(q==p)# Ensure that q and p are not the same
      {
        q = sample(1:len,1)
      }
      # A temporary array holding information about p, q
      tmp.1= array(-1,len)
      tmp.2= array(-1,len)
      tmp.3= array(-1,len)
      tmp.4= array(-1,len)
      # information about p, q
      for(n in 1:len)
      {
        tmp.1[n]=population.ran[p,n]
        tmp.2[n]=population.ran[q,n]
      }
      # Exchange
      for(n in 1:len)
      {
        population.ran[p,n]=tmp.2[n]
        population.ran[q,n]=tmp.1[n]
      }
      for(n in 1:len)
      {
        tmp.3[n]=population.ran[n,p]
        tmp.4[n]=population.ran[n,q]
      }
      # Exchange
      for(n in 1:len)
      {
        population.ran[n,p]=tmp.4[n]
        population.ran[n,q]=tmp.3[n]
      }
      # Score the movement. If the movement is not better, do `prey`
      amat(random_tmp) = population.ran 
      score_new1 = score(random_tmp,x,type = score)
      cat("after random move to middle compute score:",score_new1,"\n")
      
      if(score_new1 > population.score[cur]){
        population.cur = population.ran
        cat("new random fish score is higher than curent score,move\n")
        # cat("after move , fish :" ,population[,,1],"\n")
      }else{ 
        population.cur = prey(x,cur,population,population.size,population.score,n.nodes,af.distance,score,trynumber,visual,discount,network.temp,debug=FALSE)
        cat("new fish score is lower than curent score,prey \n")
      }
    }
    #cat("after move,now the adjacency of the fish:***\n")
    
    # #Temp：Check the fish after moving
    # amat(random_tmp) = population.cur 
    # score_new2 = score(random_tmp,x,type = score)
    # cat("after move to middle compute score:",score_new2,"\n")
  }else{
    population.cur = prey(x,cur,population,population.size,population.score,n.nodes,af.distance,score,trynumber,visual,discount,network.temp,debug=FALSE)
  }#else, too crowed here, do `prey`
  amat(network.temp) = population.cur
  tem.score = score(network.temp,x,type = score)
  cat("compute score swarm score = ",tem.score,"\n")
  return(population.cur)
}
