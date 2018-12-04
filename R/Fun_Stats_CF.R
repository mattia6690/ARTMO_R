
# General -----------------------------------
# Create the Real names and Type of Table. Somehow the Fist CF Table has an INV in between
cost.tableclass<-function(x){
  
  y1<-str_replace(x,"_inv","")
  y2<-str_split(y1,"_") %>% unlist
  y3<-c(paste(y2[1],y2[2],sep = "_"),y2[3])
  return(y3)
}

# Core --------
# Change of the Param_user Matlab Tab
.cf.generalinfo<-function(table){
  
  mapper<-map(table$general_info,function(x){
    
    matall<-readMat(x)$data[,,1]
    matall.rtm<-matall$rtm[,,1] %>% 
      melt %>% 
      select(Type=L1,Value=value) %>% 
      filter(Type!="rtm") %>% t %>% as.tibble
    colnames(matall.rtm)<-matall.rtm[1,]
    matall.rtm<-matall.rtm %>% dplyr::select(database,project,id,date)
    matall.rtm<-matall.rtm[-1,]
    colnames(matall.rtm)<-c("Database","Project","PY_ID","Date")
    matall.rtm<-as.tibble(matall.rtm)
    
    return(matall.rtm)
    
  })
  
  out <-table %>% select(-general_info) %>% mutate(general_info=mapper)
  return(out)
  
}

.cf.paramuser<-function(table){
  
  mapper<-map(table$param_user,function(x){
    
    readMat(x)$data[,,1] %>% 
      melt %>% 
      select(param_user=L1,param_user_val=value) %>% 
      filter(param_user=="label") %>% 
      select(param_user_val) %>% 
      unlist(use.names = F) %>% 
      as.numeric
    
  })
  out <-table %>% select(-param_user) %>% mutate(param_user=mapper)
  return(out)
}

# Change of the Spectros Matlab Tab
.cf.spectros<-function(table){
  
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
  return(out)
}


# Change the LUT in the "Class" Table

.cf.lut<-function(table){
  
  lut<-tryCatch(readMat(table$lut[[1]])$data,error=function(e) NULL)
  
  if(!is.null(lut)) {
    
    extracts<-c("nom.modelo","vinput","vclass","vmodel","tablasocio","output")
    mp <- map(extracts,function(x,l=lut) { l[x,,1] %>% unlist(use.names = F)})
    
    master<- mp %>% bind_cols %>% setNames(extracts)
    spectral<-lut["spectral",,1] %>% unlist(use.names = F)
    vsalidas<-lut["vsalidas",,1]$vsalidas[,,1] %>% unlist(use.names = F)
    
    diff<-length(spectral)/length(vsalidas)
    vsalidas.rep<- rep(vsalidas,each=diff)
    Spec_id<-seq(1,length(vsalidas),1) %>% rep(each=vsalidas)
    
    lut<-cbind(Spec_id,vsalidas.rep,spectral) %>% 
      as.tibble %>% 
      setNames(c("LUT_ID","Vsalidas","Spectra")) %>% 
      list
    
    master$LUT<-lut
    out <-table %>% select(-lut) %>% mutate(lut=list(master))
    #id.mod  <-lut["id.modelo",,1] %>% bind_cols %>% unnest %>% list
    #out <-table %>% select(-lut) %>% mutate(lut=list(master))
    
  } else {out <- table}
  return(out)
}

.cf.resultados<-function(table){
  
  raws<-table$resultados
  numbs<-map(raws,function(x) rawTrans(x)$numbers)
  out<-table %>% mutate(resultados=numbs)
  return(out)
}

