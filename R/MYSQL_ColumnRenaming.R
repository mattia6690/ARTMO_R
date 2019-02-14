#' Functions for renaming the columns in the Scripts from Juan Pablo
#' 
#' Convert any Matlab matrix to the right tibble format in R
#' Formats everything in tidy format

# General -----------------------------------

# Create the Real names and Type of Table. 
# Somehow the Fist CF Table has an INV in between
cost.tableclass<-function(x){
  
  y1<-str_replace(x,"_inv","")
  y2<-str_split(y1,"_") %>% unlist
  y3<-c(paste(y2[1],y2[2],sep = "_"),y2[3])
  return(y3)
}

# VIs ---------------------------------------

.vi.general<-function(tab){
  
  tab2<-tab %>% 
    dplyr::mutate(general_info=map(general_info,function(x){
      
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
    dplyr::mutate(data=map(data,function(x){
      
      gt    <- readMat(x)
      specs <-gt$data[[2]][[1]][,,1]$spectral
      wl    <- gt$data[[2]][[1]][,,1]$wl
      salidas   <- gt$data[[2]][[1]][,,1]$vsalidas
      salidas2  <- salidas[,,1] %>% 
        melt %>% 
        spread(L1,value) %>% 
        dplyr::select(-c(Var1,Var2)) %>% 
        t %>% 
        as.data.frame() %>% 
        rownames_to_column()
      
      b1<-cbind(wl,specs) %>% 
        as.tibble
      
      colnames(salidas2)<-colnames(b1)
      b2<-rbind(salidas2,b1)
      
    }))
  
  return(tab2)
}

# Extract the Sensor band information
.vi.bandas<-function(tab){
  
  tab2<-tab %>% 
    dplyr::mutate(Bands=map(bandas,function(x){
      
      gt<-rawTrans(x)$numbers %>% 
        unlist(use.names = F) 
      
    }))
  
  tab2<-tab2 %>% select(-bandas)
  
  return(tab2) 
  
}

# Get the Indices table
.vi.indices<-function(tab){
  
  tab2<-tab %>% 
    dplyr::mutate(indices=map(indices,function(x){
      
      gt <- readMat(x)
      gt <- gt$data[[2]][[1]] %>% 
        as.tibble
      
    }))
  
  return(tab2)
}


# CF ----------------------------------------

.cf.generalinfo<-function(table){
  
  mapper<-map(table$general_info,function(x){
    
    matall<-readMat(x)$data[,,1]
    matall.rtm<-matall$rtm[,,1] %>% 
      melt %>% 
      dplyr::select(Type=L1,Value=value) %>% 
      filter(Type!="rtm") %>% 
      t %>% 
      as.tibble
    
    colnames(matall.rtm)<-matall.rtm[1,]
    matall.rtm<-matall.rtm %>% 
      dplyr::select(database,project,id,date)
    
    matall.rtm<-matall.rtm[-1,]
    colnames(matall.rtm)<-c("Database","Project","PY_ID","Date")
    matall.rtm<-as.tibble(matall.rtm) 
    
    return(matall.rtm)
    
  })
  
  out <-table %>% mutate(general_info=mapper) %>% unnest %>% rename(Model=test_name)
  return(out)
  
}

.cf.paramuser<-function(table){
  
  mapper<-map(table$param_user,function(x){
    
    readMat(x)$data[,,1] %>% 
      melt %>% 
      dplyr::select(param_user=L1,param_user_val=value) %>% 
      filter(param_user=="label") %>% 
      dplyr::select(param_user_val) %>% 
      unlist(use.names = F) %>% 
      as.numeric
    
  })
  out <-table %>% 
    dplyr::select(-param_user) %>% 
    dplyr::mutate(param_user=mapper)
  
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
      dplyr::mutate(Spectrum=sprintf("%03s",as.character(Iteration)))
    
  })
  
  out <-table %>% 
    dplyr::select(-spectros_user) %>% 
    dplyr::mutate(Spectra=mapper)
  
  return(out)
}


# Change the LUT in the "Class" Table

.cf.lut<-function(table){
  
  lut<-tryCatch(readMat(table$lut[[1]])$data,error=function(e) NULL)
  
  if(!is.null(lut)) {
    
    extracts<-c("nom.modelo","vinput","vclass","vmodel","tablasocio","output")
    mp <- map(extracts,function(x,l=lut) { l[x,,1] %>% unlist(use.names = F)})
    
    master<- mp %>% 
      bind_cols %>% 
      setNames(extracts)
    
    spectral<-lut["spectral",,1] %>% 
      unlist(use.names = F)
    vsalidas<-lut["vsalidas",,1]$vsalidas[,,1] %>% 
      unlist(use.names = F)
    
    diff<-length(spectral)/length(vsalidas)
    vsalidas.rep<- rep(vsalidas,each=diff)
    Spec_id<-seq(1,length(vsalidas),1) %>% 
      rep(each=vsalidas)
    
    lut<-cbind(Spec_id,vsalidas.rep,spectral) %>% 
      as.tibble %>% 
      setNames(c("LUT_ID","Vsalidas","Spectra")) %>% 
      list
    
    master$LUT<-lut
    out <-table %>% 
      dplyr::select(-lut) %>% 
      dplyr::mutate(LUT=list(master))
    
  } else {out <- table}
  return(out)
}

.cf.resultados<-function(table){
  
  raws<-table$resultados
  numbs<-map(raws,function(x) {
    res<-rawTrans(x)$numbers %>% 
      matrix(., ncol=2) %>% 
      as.tibble %>% 
      setNames(c("Measured","Estimated"))
  })
  
  out<-table %>% 
    dplyr::mutate(Results=numbs) %>% 
    dplyr::select(-resultados)
  return(out)
}



# MLRA --------------------------------------

# PlotCV Column
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

# Statisticscv Column
.mlr.staticscv<-function(jtable){
  
  tabout<-jtable %>% 
    dplyr::mutate(staticscv=map(staticscv,function(x) {
      
      if(is.null(x)) return(NULL)
      mat<-readMat(x) %>% .$data %>% as.numeric
      return(mat)
    }))
  
  return(tabout)
  
}

# WL TColumn
.mlr.wl<-function(jtable){
  
  tabout<-jtable %>% 
    dplyr::mutate(wl=map(wl,function(x) as.numeric(rawTrans(x)$numbers))) %>% 
    rename(Model=test_name)
  return(tabout)
  
}
