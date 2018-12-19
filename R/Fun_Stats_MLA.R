#' Functions for Extracting the MLA Results
#' 
#' 
################################'

#### NOTE: Spelling mistakes are in the MySQL Database. No Errors


# Core --------------------------------------
# Plotcv Table
.mlr.plotcv<-function(jtable){
  
  tabout<-jtable %>% 
    dplyr::mutate(plotcv=map(plotcv,function(x) {
      
      if(is.null(x)) return(NULL)
      
      mat<-readMat(x)
      mat.melt<- mat$data[,,1] %>% melt %>% dplyr::select(ID=Var1,L1,value)
      mat.spread<- mat.melt %>% spread(L1,value)
      return(mat.spread)
    }))
  
  return(tabout)
  
}

# Statisticscv Table
.mlr.staticscv<-function(jtable){
  
  tabout<-jtable %>% 
    dplyr::mutate(staticscv=map(staticscv,function(x) {
      
      if(is.null(x)) return(NULL)
      mat<-readMat(x) %>% .$data %>% as.numeric
      returna(mat)
      }))
  
  return(tabout)
  
}

# WL Table
.mlr.wl<-function(jtable){
  
  tabout<-jtable %>% 
    dplyr::mutate(wl=map(wl,function(x) as.numeric(rawTrans(x)$numbers))) %>% 
    rename(Model=test_name)
  return(tabout)
  
}
