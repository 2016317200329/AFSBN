###################### fy: Follow；##############################
# Author: fy, wlg, wyj
# time: 2018-06-13

follow=function(x,cur,population,population.size,population.score,n.nodes,af.distance,score,trynumber,visual,discount,network.temp,delta,debug=FALSE){
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~fy: follow of No.",cur," fish ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
  len = n.nodes
  size = population.size      
  distance = af.distance
  #discount = 0.5 #fy: this argument means how many nodes should be changed in one step
  population.cur=population[,,cur] 

  #fy:find the best fish among current fish's neighbers 
  max_score = population.score[1]
  max_score_index = 1 #fy：mark the best fish index
  for(j in 1:size){
    if(distance[j] < visual & population.score[j] > max_score){
      max_score = population.score[j]
      max_score_index = j
    }#if
  }#for
  
  #fy：if the best fish's score is lower than current fish's score , prey.
  if(max_score < population.score[cur]){
    cat("the best score is lower than current score, prey\n")
    population.cur = prey(x,cur,population,population.size,population.score,n.nodes,af.distance,score,trynumber,visual,discount,network.temp,debug=FALSE) #fy： 觅食，？？不知道prey里面形参是啥？？
  }else{
    neighbers = 0
    for(i in 1:population.size){
        if( distance[cur,i] < visual){
          neighbers = neighbers + 1
        }
    }
    if(max_score > population.score[cur] && max_score/neighbers > delta*population.score[cur]){  #delta < 1
      cat("the best score is higher than current score, move\n")
    
     difference = array() #fy:record the different nodes between current fish and max_score_index fish
     count = 0
     for ( i in 1:(len*len)){
      j = floor(i/len) + 1
      k = i - (j-1)*len
      if (k == 0){
        k = len
      }
      if ((i %% len) == 0){
        j = j - 1 
      }
       if(population.cur[j,k] != population[j,k,max_score_index]){
          count < count + 1 
          difference = append(difference,i,after = length(difference))
       }
     }
     difference = difference[2:length(difference)]
     #try_max = 10
     try_num = 0
     try_max = 10
     flag = 1
     while (flag){
      one_step = sample(difference,floor(count*discount),FALSE)  
      try_num = try_num + 1
      for(k in one_step){
        i = floor(k/len)+1
        j = k - (i-1)*len
        if((k-(i-1)*len) == 0){
          j = len
        }
        if((k%%len)== 0 ){
          i = i - 1
        }
        population.cur[i,j] = population[i,j,max_score_index]
      }
      graph.population.cur <- graph.adjacency(population.cur)
      isdag = is_dag(graph.population.cur)
       if(try_num < try_max && !isdag) {
        flag = 1
       }else{
        flag = 0
       }
     }
    
     
     #fy: if there is a circle in current fish's graph, first move to the best fish, then move one step randomly
     if(try_num == try_max){
      #population.ran = population.cur 
      population.ran = population[,,max_score_index]
      p<-sample(1:len,1) 
      q<-sample(1:len,1) 
      while(q==p) # Ensure that q and p are not the same
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
      # p = sample(2:len,1)
      # p_i = floor(p/len) + 1
      # p_j = p - (p_i-1)*len
      # if (p_j == 0) p_j = len
      # if ((p %% len) == 0) p_i = p_i - 1
      # 
      # q = 1
      # q_i = floor(q/len) + 1
      # q_j = q - (q_i -1)*len
      # if( q_j == 0) q_j = len
      # if( (q %% len) == 0)  q_i = q_i - 1
      # flag = 1
      # while(flag){
      #  while(population.ran[p_i,p_j] == population.ran[q_i,q_j] || (p == q) && q < len) { q = q + 1 }
      #   q_i = floor(q/len) + 1
      #   q_j = q - (q_i -1)*len
      #   if( q_j == 0) q_j = len
      #   if( (q %% len) == 0)  q_i = q_i - 1
      #   #fy:use a temporary array to record p,q
      #   tmp.1= array(-1,len)
      #   tmp.2= array(-1,len)
      #   tmp.3= array(-1,len)
      #   tmp.4= array(-1,len)
      #   #fy: store p,q
      #   for(n in 1:len)
      #   {
      #     tmp.1[n]=population.ran[p,n]
      #     tmp.2[n]=population.ran[q,n]
      #   }
      #   #fy: exchange p,q in cols
      #   for(n in 1:len)
      #   {
      #     population.ran[p,n]=tmp.2[n]
      #     population.ran[q,n]=tmp.1[n]
      #   }
      #   for(n in 1:len)
      #   {
      #     tmp.3[n]=population.ran[n,p]
      #     tmp.4[n]=population.ran[n,q]
      #   }
      #   #fy: exchange p,q in rows
      #   for(n in 1:len)
      #   {
      #     population.ran[n,p]=tmp.4[n]
      #     population.ran[n,q]=tmp.3[n]
      #   }
      #   graph.population.ran <- graph.adjacency(population.ran)
      #   if(!is_dag(graph.population.ran)){
      #     flag = 1
      #   }else{
      #     flag = 0
      #   }
        #fy: score fish after moved 
        amat(network.temp) = population.ran 
        score_new = score(network.temp,x,type = score)
        
        if(score_new >= population.score[cur]){ #fy: if fish score after moved is higher than current fish, move to best fish derictly
          population.cur = population.ran 
          cat("new fish score is higher than curent score,move\n")
        }else{ #fy：else prey
          population.cur = prey(x,cur,population,population.size,population.score,n.nodes,af.distance,score,trynumber,visual,discount,network.temp,debug=FALSE)
          cat("new fish score is lower than curent score,prey \n")
        }
      #}#while
     }#if
    }else{
      population.cur = prey(x,cur,population,population.size,population.score,n.nodes,af.distance,score,trynumber,visual,discount,network.temp,debug=FALSE)
    }
    #fy: move current fish according to fish: max_score_index, and score again
    #fy: if fish score after moved is higher , move to new fish
    
  }#else
  cat("~~~~~~~~~~~~~~~~~~~ fy:end follow of No.",cur," fish ~~~~~~~~~~~~~~~~~~~~~~~\n")
  return(population.cur)
}