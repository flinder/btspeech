makeDataset = function(ws_output){
  
  scores_matrix = ws_output$results
  se_matrix = ws_output$se

  scores_matrix[,grep('Solms',colnames(scores_matrix))]
  for(k in 1:nrow(scores_matrix)){
    scores = scores_matrix[k,]
    ses = se_matrix[k,]
    names(scores) = colnames(scores_matrix)
    names(ses) = colnames(se_matrix)
    
    dataset = matrix(NA,nr=length(scores),nc=8)
    dataset = as.data.frame(dataset)
    colnames(dataset) = c('position','error','term','direct','party','name','dimension','filename')
  
  
    for(i in 1:length(scores)){
      
      name = names(scores)[i]
      dataset$filename[i] = name
      split = unlist(strsplit(name, ' '))
      #if(grepl('[.]',split[2])) pname = paste(split[1],split[2],split[3])
      #else pname = paste(split[1],split[2])
       
      pname = paste(split[1],split[2],split[3])
      pname = gsub('\\(.+','',pname)
      pname = gsub(' $','',pname)
      
      dataset$position[i] = scores[i]
      dataset$error[i] = ses[i]
      dataset$name[i] = pname
      if(grepl('\\(15\\)',name)) dataset$term[i] = 15
      if(grepl('\\(16\\)',name)) dataset$term[i] = 16
      if(grepl('\\(17\\)',name)) dataset$term[i] = 17
      if(grepl('list',name)) dataset$direct[i] = 0
      if(grepl('direct',name)) dataset$direct[i] = 1
      if(grepl('LINKE',name)) dataset$party[i] = 1
      if(grepl('90-',name)) dataset$party[i] = 2
      if(grepl('SPD',name)) dataset$party[i] = 3
      if(grepl('CDU-',name)) dataset$party[i] = 4
      if(grepl('FDP',name)) dataset$party[i] = 5
    }
    
    dataset$dimension = k
    
    if(k == 1) data = dataset
      else data = rbind(data,dataset)
  }
  return(data) 
}


