sampling<-function(g,method,...){
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
      
      sampled<-data[data[,location[1]]%in%sampled.node|data[,location[2]]%in%sampled.node,]
      sampled<-sampled[!duplicated(sampled$edge.index),]      
      last.size<-0
      this.size<-length(sampled.node)
      temp.node<-c()
      wave<-0
      
     repeat{
       if (length(sampled.node)>=population|wave>=method$waves) { break 
       }else{
       if(this.size==last.size){
         # reach the boundary of the components
         if (resample){
           temp.node<-nodes[!nodes%in%sampled.node]
           temp.node<-temp.node[sample.int(length(temp.node),increase)]
           last.size<-length(sampled.node)   
           sampled.node<-c(sampled.node,temp.node)
           sampled<-data[data[,location[1]]%in%sampled.node|data[,location[2]]%in%sampled.node,]
           sampled<-sampled[!duplicated(sampled$edge.index),]
         }else{
           warning('Boundary reached')
         }
       }   else { # boundary not reached
       last.size<-length(sampled.node)   
       sampled<-rbind(sampled,neighbour(id.names[1],id.names[2],sampled.node,data))
       sampled<-sampled[!duplicated(sampled$edge.index),]
       }
       sampled.node<-c(sampled.node,sampled[,location[1]],sampled[,location[2]])   
       sampled.node<-sampled.node[!duplicated(sampled.node)]
       this.size<-length(sampled.node)
       wave<-wave+1
      }
     }
    }# snowball
    
    if (method$name=='star'){
      # star
      n.start<-method$n.start
      if(exists('initial')){
        if(length('initial')>population) {
          warning("number of seeds exceeds target population size")
          stop
        }
        sample.node<-nodes[nodes%in%init]
      }else{
        sampled.node<-nodes[sample.int(population,n.start)]
      }
      sampled<-neighbour(id.names[1], id.names[2], sampled.node, data, population)
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
        if ((length(sampled.node)>=population)) { break }
        if (this.size-last.size<=threshold&wave>=1) { break }
        last.size<-length(sampled.node)
        
        for (i in sampled.node){
          temp<-neighbour(id.names[1],id.names[2],i,data,size=population)
          temp<-temp[sample.int(temp.size,1),]
          temp.size<-nrow(temp)
          if((p<1&runif(1)<=p)|p>=1){
            sampled<-rbind(sampled,temp)
          }           
        }
        sampled<-sampled[!duplicated(sampled$edge.index),]
        sampled.node<-c(sampled.node,sampled[,location[1]],sampled[,location[2]])
        sampled.node<-sampled.node[!duplicated(sampled.node)]
        this.size<-length(sampled.node)
        wave<-wave+1
      }
    }# tracing
  }
  sampled<-sampled[!duplicated(sampled$edge.index),]
  invisible(sampled)
}