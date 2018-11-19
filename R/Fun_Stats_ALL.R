
#' Functions for Extracting the COST Functions
#' 
#' The functions are divided by steps taken for accessing the Databases
################################'

# Read the COST Functions based on the Tables (Stat.all)
getStats<-function(con2,stat.all){
  
  temp<-stat.all %>% nest(-c(Database,Table_Type))
  stat.tabs<-temp %>% 
    mutate(Tables=map2(Table_Type,data,function(type,data){
      
      read<-purrr::map(data$Table,function(x){
        
        #' Machine Learning
        if(type=="test_mla"){
          
          # Read Table
          tab<-dbGetQuery(con,glue("select * from {x}"))
          tabn<-colnames(tab)
          
          # Transform Column Values
          tab<-as.tibble(tab)
          if(any(tabn=="wl"))     tab<-.mlr.wl(tab)
          if(any(tabn=="plotcv")) tab<-.mlr.plotcv(tab)
          if(any(tabn=="statcv")) tab<-.mlr.staticscv(tab)
          
          # Delete Columns
          if(any(tabn=="mla_settigs"))  tab<-tab %>% select(-"mla_settigs")
          if(any(tabn=="general_info")) tab<-tab %>% select(-"general_info")
        }
        
        #' Lookup Table Inversion
        if(type=="test_cf"){
          
          # Read Table
          isnoise<- any(grepl("noise",x))
          if(isnoise==F) tab<-dbGetQuery(con,paste("select * from",x))
          if(isnoise==T) tab<-dbGetQuery(con,paste("select ID_T7,id_t6,noise from",x))
          tabn<-colnames(tab)
          
          # Transform Columns
          tab<-as.tibble(tab)
          if(any(tabn=="general_info")) tab<-.cf.generalinfo(tab)
          if(any(tabn=="spectros_user"))tab<-.cf.spectros(tab)
          if(any(tabn=="param_user"))   tab<-.cf.paramuser(tab)
          if(any(tabn=="lut"))          tab<-.cf.lut(tab)
          if(any(tabn=="resultados"))   tab<-.cf.resultados(tab)
          
        }
        
        #' Indices (In the Pipeline, too big to join for now -> Reduce dimensionality?)
        if(type=="vis_test") tab<-NA
        
        return(tab)
      })
      if(!is.na(read)) return(read)
    }))
}

# Join the Diverse tables by their ID
joinTabs<-function(stat.tabs){
  
  temp<-stat.tabs %>% split(1:nrow(stat.tabs))
  stats.join<-map(temp,function(x){
    
    y<-x$Tables %>% unlist(recursive=F)
    
    if(length(y)>2){
      
      for(i in 2:length(y)){
        
        if(i==2)  join<-.sqljoin(y[[i-1]],y[[i]])
        if(i>2)   join<-.sqljoin(join,y[[i]])
        
      }
      #if(noid==T) join<-join %>% select(-contains("id_"))
      join<-as.tibble(join)
    }
  })
  
  out<-stat.tabs %>% mutate(Tables=stats.join)
  return(out)
}

removeid<-function(table){
  
  cn<-colnames(table)
  if(any(grepl("id_",cn))) out<-select(table,-contains("id_"))
  
}

