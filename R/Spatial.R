
#' @title Link the Spatial Directories to the Databases
#' @description one
#' @param table Tibble; Tibble retrieved with the MYSQL extraction `getMYSQL`
#' @param dir character; Directory od the Datastructure built by `buildpath`
#' @param addraster boolean; Have the Rasters already been added and do you want to include them in the list?
#' @import purrr
#' @import dplyr
#' @import stringr
#' @importFrom glue glue
#' @importFrom magrittr "%>%"
#' @importFrom raster brick
#' @export
spatDir<-function(table,dir="",addraster=F){
  
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

#' @title Add the Spaial Results to the Dataframe
#' @description two
#' @param table Tibble; Tibble retrieved with the MYSQL extraction `getMYSQL`
#' @param dir character; Directory od the Datastructure built by `buildpath`
#' @param addraster boolean; Have the Rasters already been added and do you want to include them in the list?
#' @importFrom magrittr "%>%"
#' @import purrr
#' @importFrom raster extract
#' @importFrom tibble as.tibble
#' @importFrom stats lm
#' @export
spatRes<-function(spatial.df,measuredCol=""){
  
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
