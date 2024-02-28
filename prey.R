#name: prey
#author: djw, wlg, wyj


#djw:求出目前状态的节点顺序。
prey=function(x,cur,population,population.size,population.score,n.nodes,af.distance,score,trynumber,visual,discount,network.temp,debug=FALSE){
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~djw:prey of No.",cur," fish~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n")
  len=n.nodes
  cnt=0
  population.cur=population[,,cur]
  dif =array()
  #cat("asd\n")
  #寻找优于当前网络的网络 如果到达trynumber后找不到则随机移动一步
  y <- sample(1:population.size, population.size, FALSE)
  y_cnt <- 1
  while(cnt < trynumber)
  {
    population.tem=population.cur
    cnt=cnt+1
    #y <-sample(1:population.size,1)
    cat(af.distance[cur,y[y_cnt]])
    cat("circle: " ,cnt, ", sample: ",y[y_cnt],"\n")
    while(y_cnt<=population.size&af.distance[cur,y[y_cnt]]>visual)
    {
      y_cnt=y_cnt+1
    }
    if(y_cnt<=population.size&population.score[cur] < population.score[y[y_cnt]] && cur != y[y_cnt]&&af.distance[cur,y[y_cnt]]<visual)
    {
      c=0L
      for( i in 1:len*len){
        if(i%%len ==  0){
          j = i/len
          k = len
        }else{
          j = floor(i/len)+1
          k = i - (j-1)*len
        }
        if( population.cur[j,k] != population[j,k,y[y_cnt]]){
          dif = append(dif,i,after = length(dif))
          c = c + 1 
        }
      }
      dif = dif[2:length(difference)]
      one_step = sample(dif,floor(c*discount),FALSE)
      for(k in one_step){
        if(k%%len ==  0){
          i = k/len
          j = len
        }else{
          i = floor(k/len)+1
          j = k - (i-1)*len
        }
        population.tem[i,j] = population[i,j,y[y_cnt]]
      }
      graph.population.tem <- graph.adjacency(population.tem)
      if(is_dag(graph.population.tem))
      {
        population.cur=population.tem
        break;
      }
      y_cnt = y_cnt + 1
    }#if
  }#while
  if(cnt==trynumber)
  {
    population.ran<-population.cur
    p<-sample(1:n.nodes,1)#设置p节点
    q<-sample(1:n.nodes,1)#设置q节点
    while(q==p)#保证p q 结点不一样
    {
      q<-sample(1:n.nodes,1)
    }
    cat("cur")
    print(population.cur)
    cat("p q")
    print(p)
    print(q)
    #设置临时数列保存P q的信息
    tmp.1= array(-1,n.nodes)
    tmp.2= array(-1,n.nodes)
    tmp.3= array(-1,n.nodes)
    tmp.4= array(-1,n.nodes)
    #存储p q的信息
    for(n in 1:n.nodes)
    {
      tmp.1[n]=population.ran[p,n]
      tmp.2[n]=population.ran[q,n]
    }
    #交换p q
    for(n in 1:n.nodes)
    {
      population.ran[p,n]=tmp.2[n]
      population.ran[q,n]=tmp.1[n]
    }
    for(n in 1:n.nodes)
    {
      tmp.3[n]=population.ran[n,p]
      tmp.4[n]=population.ran[n,q]
    }
    #交换p q
    for(n in 1:n.nodes)
    {
      population.ran[n,p]=tmp.4[n]
      population.ran[n,q]=tmp.3[n]
    }
    #移动
    population.cur<-population.ran
  }
   cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~djw:end of prey of No.",cur," fish~~~~~~~~~~~~~~~~~~~~~~~~~~~~","\n")
   amat(network.temp) = population.cur
   tem.score = score(network.temp,x,type = score)
   cat("compute score prey score = ",tem.score,"\n")
   return(population.cur)
}