

#' Functions for Combining ARTMo and Spatial Information
#' 
#' The functions are divided by steps taken for accessing the Databases



# Function for Melting the Dataframe based on needed Models, Parameters, Statistics etc.
# Possibility to add GGplot representation
stat.condense<-function(jtable,model=NULL,parameters=NULL,statistics=NULL,addGG=F){
  
  gg1<-jtable %>% 
    select(Model,
           Store.ID,
           eval(statistics),
           eval(parameters),
           Results)
  
  if(!is.null(model)) gg1<-gg1 %>% filter(Model==model)
  if(nrow(gg1)==0) stop("Check your Inputs!")
  
  if(addGG==T){
    
    gg1<-gg1 %>% 
      mutate(ResultGG=pmap(.,function(...,Results){
        
        g1<-ggplot(Results,aes(Measured,Estimated))+
          geom_point()+
          geom_smooth(method="lm")+
          ggtitle(glue("Measured vs. Estimated Samples"))+
          xlim(0,10)+ylim(0,10)+
          geom_abline(intercept=0,slope=1,col="firebrick4",linetype=2)
        return(g1)
        
      }))
  }
  return(gg1)
}


SpatDir<-function(table,dir="",addraster=F){
  
  out<-table %>% 
    mutate(SpatDir=pmap_chr(.,function(...,Model,Database,Table_Type,Store.ID){
      
      have<-glue("{dir}/{Database}/{Table_Type}/{Model}/{Store.ID}/")
      return(have)
    }))
  
  if(addraster==T){
    
    rr1<-out %>% 
      mutate(RasDir=map(SpatDir, function(x){
        
        list<-list.files(x,pattern=".hdr",full.names = T) %>% str_replace(.,".hdr","")
        if(length(list)==0) list<-NULL
        return(list)
        
      }))
    
    rr2<-rr1 %>% 
      filter(!map_lgl(RasDir, is.null)) %>%
      unnest(RasDir,.drop = F)
    
    rr2 <- rr2 %>% 
      mutate(RasName=map_chr(RasDir,basename))
    
    out<-rr2 %>% 
      mutate(Rasters=map(RasDir,brick))
    
  }
  
  return(out)
}

add.spatialresults<-function(spatial.df,measuredCol=""){
  
  data1<-spatial.df %>% 
    mutate(Spat.Result=map2(Rasters,Features,function(x,y){
      
      Simulated <-raster::extract(x[[1]],as_Spatial(y))
      Measured  <-y %>% as.data.frame %>% select(measuredCol)
      
      results   <- cbind(Measured,Simulated) %>% 
        setNames(c("Measured","Simulated")) %>% 
        as.tibble
      return(results)
      
    }))
  
  data2<-data1 %>% 
    mutate(Spat.Lm=map_dbl(Spat.Result,function(l){
      
      lm<-lm(l$Simulated~l$Measured)
      r2<-summary(lm)$r.squared
      return(r2)
    }))
  
  return(data2)
}
