
#' Functions for Extracting the COST Functions
#' 
#' The functions are divided by steps taken for accessing the Databases
################################'

# Main Functions ------------------------------------

#' Checks which Tables are available in the MYSQL Database 
getLinks<-function(con) {
  
  # Which Columns represent the Results
  results<-c("test_cf_train","test_mla_result","test_mla_resultal","vis_test_matrix","vis_test_test","vis_test_valid")
  
  db<-dbGetInfo(con)$dbname
  # Which Tables are actually available in the MySQL file
  Stab<-dbGetQuery(con,statement=paste("show tables like '%test_%'")) %>% unlist(use.names = F)
  Stab.replace<- map(Stab,function(x) str_replace(x,"_inv",""))
  
  # Collect all the Information from the Tables according to the Test prefix
  cost.tabs.all<-map(Stab,function(x){
    
    cnt  <- dbGetQuery(con,statement=paste("select count(*) from",x))[[1]] %>% as.numeric
    read <- dbGetQuery(con,statement=paste0("select * from information_schema.columns where table_name='",x,"' and column_name like 'id%'")) %>% as.tibble
    r1   <- read %>% 
      filter(TABLE_SCHEMA==db) %>% 
      select(Database=TABLE_SCHEMA,Table=TABLE_NAME,IDs=COLUMN_NAME)
    r1$Count<-cnt
    
    r1$Table_Type<-map_chr(r1$Table,function(x) cost.tableclass(x)[1])
    r1$Table_Name<-map_chr(r1$Table,function(x) cost.tableclass(x)[2])
    
    return(r1)
    
  }) %>% do.call(rbind,.)
  
  rank <- cost.tabs.all %>% 
    filter(grepl("ID_T",IDs))  %>% 
    mutate(Rank=as.numeric(str_replace(IDs,"ID_T",""))) %>% 
    arrange(Table_Type,Rank)
  
  isres <-rank %>% mutate(result=map_dbl(Table,function(x) x %in% results))
  
  ID   <- isres  %>% filter(Count>0)
  return(ID)
  
}

# Read the COST Functions based on the Tables (Stat.all)
getTabs<-function(con,links){

  toload<-links %>% select(Database,Table_Type,Table_Name,Table,Rank,Count,result)
  
  stat.tabs<-toload %>% 
    mutate(MyTables=purrr::map2(Table_Type,Table,function(type,x){
      
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
        if(isnoise==F) tab<-dbGetQuery(con,glue("select * from {x}"))
        if(isnoise==T) tab<-dbGetQuery(con,glue("select ID_T7,id_t6,noise from {x}"))
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
      if(type=="vis_test") {
        
        result<-c("vis_test_test","vis_test_valid")
        
        if(x %in% result){
          
          get<-"ID_T10,id_t9,bands,bands1,parametros,parametros1,ME,RMSE,RELRMSE,MAE,R,R2,NRMSE,TS,NSE"
          tab<-dbGetQuery(con,glue("select {get} from {x}")) %>% as.tibble
          
        } else {
          
          tab<-dbGetQuery(con,glue("select * from {x}")) %>% as.tibble
          tabn<-colnames(tab)
          
          if(any(tabn=="general_info")) tab<-.vi.general(tab)
          if(any(tabn=="data")) tab<-.vi.class(tab)
          if(any(tabn=="bandas")) tab<-.vi.bandas(tab)
          if(any(tabn=="indices")) tab<-.vi.indices(tab)
          
        }
      }
      
      return(tab)
    }))
  
  nulls<-map(stat.tabs$MyTables,is.null) %>% unlist
  stat.tabs<-stat.tabs[!nulls,]
  stat.tabs<-.idcorr(stat.tabs)
  return(stat.tabs)
}

#' Function for Inerative Join (For Loop) of each Table within the same
#' Table_Type (purrr:map)
doJoin<-function(tabs,removeid=F){
  
  # Group and perform operation by Table Type (each class single)
  t1<-tabs %>% group_by(Database,Table_Type) %>% nest
  t2<-t1 %>% mutate(Metrics=map(data,function(x){
    
    # Join all the Metainformation
    meta<- x %>% filter(result==0)
    
    if(nrow(meta)>0){
      
      for(i in 2:nrow(meta)){
        
        if(i==2) tib.meta<-meta$MyTables[[i-1]]
        tib.join<-meta$MyTables[[i]]
        tib.meta<-left_join(tib.meta,tib.join)
        
      }
      
    } else {tib.meta<-NA}
    
    # Join all the Results
    result<- x %>% filter(result==1)
    
    if(nrow(result)>0){
      
      tib.res<-do.call(rbind,result$MyTables)
      
    } else {tib.res<-NA}
    
    # JOin Metainformation and Results
    if(is.na(tib.meta) && !is.na(tib.res)) out<-tib.res
    if(!is.na(tib.meta) && is.na(tib.res)) out<-tib.meta
    if(!is.na(tib.meta) && !is.na(tib.res)) out<-left_join(tib.meta,tib.res)
    
    if(removeid==T) out<-.removeid(out)
    return(out)
    
  }))
  
  t3<-t2 %>% dplyr::select(-data)
  return(t3)
 
}



# Side Functions ----------------------------

# Function for removing all the IDS (Tidying up)
.removeid<-function(table){
  
  cn<-colnames(table)
  if(any(grepl("ID_",cn))) {
    out<-select(table,-contains("ID_"))
    return(out)
  } else { return(table)}
  
}

# Correct the IDs from small to big. Necessary for the join
.idcorr<-function(tabs){
  
  t<-tabs %>% mutate(MyTables=map(MyTables,function(x){
    
    cn<-colnames(x)
    sw<-startsWith(cn,"id_")
    cn[sw]<-toupper(cn[sw])
    colnames(x)<-cn
    return(x)
    
  }))
  
  return(t)
  
}

