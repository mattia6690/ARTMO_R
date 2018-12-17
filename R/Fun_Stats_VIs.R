#' Functions for Extracting the VI Results
#' 
#' 
################################'

# Core ---------------------------------------
# Extract the General Information from raw
.vi.general<-function(tab){
  
  tab2<-tab %>% 
    mutate(general_info=map(general_info,function(x){
    
    gt<-readMat(x)
    gt<-gt$data[,,1]$user[,,1] %>% 
      melt %>% 
      dplyr::select(Information=L1,Value=value) %>% 
      filter(Information != "path") %>% 
      as.tibble
    
  })) %>% rename(Model=test_name)
  
  return(tab2)
}

# Extract the metadata for sensor
.vi.class<-function(tab){
  
  tab2<-tab %>% 
    mutate(data=map(data,function(x){
      
      gt <- readMat(x)
      specs<-gt$data[[2]][[1]][,,1]$spectral
      wl <- gt$data[[2]][[1]][,,1]$wl
      salidas <- gt$data[[2]][[1]][,,1]$vsalidas
      salidas2 <- salidas[,,1] %>% melt %>% spread(L1,value) %>% select(-c(Var1,Var2)) %>% t %>% as.data.frame()%>% rownames_to_column()
      
      b1<-cbind(wl,specs) %>% as.tibble
      colnames(salidas2)<-colnames(b1)
      b2<-rbind(salidas2,b1)
      
    }))
  
  return(tab2)
}

# Extract the Sensor band information
.vi.bandas<-function(tab){
  
  tab2<-tab %>% 
    mutate(Bands=map(bandas,function(x){
      
      gt<-rawTrans(x)$numbers %>% unlist(use.names = F) 
      
    }))
  
  tab2<-tab2 %>% select(-bandas)
  
  return(tab2) 
  
}

# Get the Indices table
.vi.indices<-function(tab){
  
  tab2<-tab %>% 
    mutate(indices=map(indices,function(x){
      
      gt <- readMat(x)
      gt <- gt$data[[2]][[1]] %>% as.tibble
      
    }))
  
  return(tab2)
}
