#' Functions for Extracting the MLA Results
#' 
#' 
################################'

#### NOTE: Spelling mistakes are in the MySQL Database. No Errors


# Core --------------------------------------
# Plotcv Table
.mlr.plotcv<-function(jtable){
  
  tabout<-jtable %>% 
    mutate(plotcv=map(plotcv,function(x) {
      
      mat<-readMat(x)
      mat.melt<- mat$data[,,1] %>% melt %>% select(ID=Var1,L1,value)
      mat.spread<- mat.melt %>% spread(L1,value)
    }))
  
  return(tabout)
  
}

# Statisticscv Table
.mlr.staticscv<-function(jtable){
  
  tabout<-jtable %>% 
    mutate(staticscv=map(staticscv,function(x) {readMat(x) %>% .$data %>% as.numeric}))
  return(tabout)
  
}

# WL Table
.mlr.wl<-function(jtable){
  
  tabout<-jtable %>% 
    mutate(wl=map(wl,function(x) as.numeric(rawTrans(x)$numbers))) %>% 
    rename(Model=test_name)
  return(tabout)
  
}
