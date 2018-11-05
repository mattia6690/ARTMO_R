#' Functions for Extracting the COST Functions
#' 
#' The functions are divided by steps taken for accessing the Databases
################################

# Change of the Param_user Matlab Tab
costfun.param_user<-function(table){
  
  istab<-any(names(table)=="param_user")
  if(isTRUE(istab)){
    
    mapper<-map(table$param_user,function(x){
      
      readMat(x)$data[,,1] %>% melt %>% select(param_user=L1,param_user_val=value)
      
    })
    out <-table %>% select(-param_user) %>% mutate(param_user=mapper)
    
  } else {out <- table}
  return(out)
}

# Change of the Spectros Matlab Tab
costfun.spectros<-function(table){
  
  istab<-any(names(table)=="spectros_user")
  if(isTRUE(istab)){
    
    mapper<-map(table$spectros_user,function(x){
      
      s1<-readMat(x)$data[,,1]
      s2<- s1$spectral
      rownames(s2)<-s1$wl
      s3 <- melt(s2) %>% 
        setNames(c("Wavelength","Iteration","Value")) %>% 
        as.tibble %>% 
        mutate(Spectrum=sprintf("%03s",as.character(Iteration)))
      
    })
    
    out <-table %>% select(-spectros_user) %>% mutate(spectros_user=mapper)
    
  } else {out <- table}
  return(out)
}

costfun.lut<-function(table){
  
  istab<-any(names(table)=="lut")
  if(isTRUE(istab)){
    
    lut<-readMat(table$lut[[1]])$data
    extracts<-c("nom.modelo","vinput","vclass","vmodel","tablasocio","output")
    mp <- map(extracts,function(x,l=lut) { l[x,,1] %>% unlist(use.names = F)})
    
    master<- mp %>% bind_cols %>% setNames(extracts)
    master$spectral<-lut["spectral",,1] %>% unlist(use.names = F) %>% list
    master$vsalidas<-lut["vsalidas",,1]$vsalidas[,,1] %>% unlist(use.names = F) %>% list
    master$id.mod  <-lut["id.modelo",,1] %>% bind_cols %>% unnest %>% list
    
    out <-table %>% select(-lut) %>% mutate(lut=list(master))
    
  } else {out <- table}
  return(out)
}


