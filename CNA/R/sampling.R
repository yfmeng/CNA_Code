sampling<-function(id.names,data,method,...){
  warning('snowball method needs debugging')
  # methods: induced, incident, star, snowball, tracing

  data$edge.index<-1:nrow(data)
  sampled<-c()
  location<-which(names(data)%in%id.names)
  nodes<-c()
  for (i in 1:length(location)){
    nodes<-c(nodes,data[,location[i]])
  }
  nodes<-unique(nodes)
  population<-length(nodes)
  edge.size<-nrow(data)
  if (!sampling.method(method)$valid) {
    warning('Invalid method parameters.')
    stop
  } else{ 
    method<-sampling.method(method)
    if (method$name=='induced'){
      # induced
      p.node<-method$p.node
      if (p.node<=1){p.node<-ceiling(p.node*population)}
      included<-sample.int(population,size=p.node)
      included<-nodes[included]
      temp.sampled<-rep(F,nrow(data))
      for (i in length(location)){
        temp.sampled<-temp.sampled|(data[,location[i]]%in%included)
      }
      sampled<-data[temp.sampled,]
    }# induced
    
    if (method$name=='incident'){
     # incident
     p.edge<-method$p.edge
     if(p.edge<=1){p.edge<-ceiling(p.edge*edge.size)}
     included<-sample.int(edge.size,size=p.edge)
     sampled<-data[included,]
    }# incident
    
    if (method$name=='snowball'){
      # snowball
      n.start<-method$n.start
      if(n.start>population){n.start<-population}
      
      increase<-method$increase
      resample<-method$resample
      
        if(exists('initial')){
          if(length(initial)>population) {
            warning("number of seeds exceeds target population size")
            stop
          }
          sample.node<-nodes[nodes%in%init]
        }else{
        sampled.node<-nodes[sample.int(population,n.start)]
        }
      
      sampled<-c()
      last.size<-n.start
      this.size<-length(sampled.node)
      temp.node<-c()
      wave<-0
      
     repeat{
       if (length(sampled.node)>=population|wave>=method$waves) { break }
       
       if(this.size==last.size){
         # reach the boundary of the components
         if (resample){
           temp.node<-nodes[!nodes%in%sampled.node]
           temp.node<-temp.node[sample.int(length(temp.node),increase)]
         }else{
           warning('Boundary reached'); break
         }
       }  
       last.size<-length(sampled.node)
       sampled.node<-c(sampled.node,temp.node)
       sampled<-rbind(sampled,neighbour(id.names[1],id.names[2],sampled.node,data,population))
       sampled<-rbind(sampled,neighbour(id.names[2],id.names[1],sampled.node,data,population))
       sampled<-sampled[!duplicated(sampled$edge.index),]
       sampled.node<-unique(c(sampled.node,sampled$neighbour))   
       this.size<-length(sampled.node)
       wave<-wave+1
     }     
    }# snowball
    
    if (method$name=='star'){
      # star
      n.start<-method$n.start
      if(exists(initial)){
        if(length(initial)>population) {
          warning("number of seeds exceeds target population size")
          stop
        }
        sample.node<-nodes[nodes%in%init]
      }else{
        sampled.node<-nodes[sample.int(population,n.start)]
      }
      sampled<-neighbour(id.names[1], id.names[2], sampled.node, data, population)
      sampled<-rbind(sampled,neighbour(id.names[2], id.names[1], sampled.node, data, population))
      sampled<-sampled[!duplicated(sampled$edge.index),]
    }# star
    
    if (method$name=='tracing'){
      # tracing
      n.start<-method$n.start
      threshold<-method$threshold
      if(exists('initial')){
        if(length(initial)>n.end) {
          warning("number of seeds exceeds target sample size")
          stop
        }
        sampled.node<-nodes[nodes%in%init]
      }else{
        sampled.node<-nodes[sample.int(population,n.start)]
      }
      
      p<-method$recruit.rate #recruit rate
      this.size<-length(sampled.node)
      last.size<-0
      wave<-0
      repeat{
        if (length(sampled.node)>=population) { break }
        if (this.size-last.size<=threshold&wave>=1) { break }
        last.size<-length(sampled.node)
        
        for (i in sampled.node){
          temp<-neighbour(id.names[1],id.names[2],i,data,population)
          temp<-rbind(temp,neighbour(id.names[2],id.names[1],i,data,population))
          temp.size<-nrow(temp)
          if(temp.size>=1){
          temp<-temp[sample.int(temp.size,1),]
          sampled<-rbind(sampled,temp)
          } 
        }
        sampled<-sampled[!duplicated(sampled$edge.index),]
        sampled.node<-c(sampled.node,sampled$neighbour)
        sampled.node<-unique(sampled.node)
        this.size<-length(sampled.node)
        wave<-wave+1
      }
    }# tracing
  }
  invisible(sampled)
}