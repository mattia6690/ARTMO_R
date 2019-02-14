

# General -----------------------------------
# Correction of the IDs? Capital letters
.idcorr<-function(tabs){
  
  t<-tabs %>% 
    mutate(MyTables=map(MyTables,function(x){
      cn<-colnames(x)
      sw<-startsWith(cn,"id_")
      cn[sw]<-toupper(cn[sw])
      colnames(x)<-cn
      return(x)
    }))
  return(t)
}

# Function for removing all the IDS (Tidying up)
.removeid<-function(table){
  
  cn<-colnames(table)
  if(any(grepl("ID_",cn))) {
    out<-select(table,-contains("ID_"))
    return(out)
  } else { return(table)}
  
}


# Get Data ----------------------------------

#' @title Return ARTMO Table Relationships
#' @description This is the first function necessary for the combination of the MYSQL back.end data.
#' It searches for the links between the single tables stored in MYSQL. 
#' Thereby a tibble is generated containing the name of the databse, Table, its ID as well as the Type of the Table (MLRA, LUT etc.).
#' The dataframe is added by an information of the rank (position in table sequence) as well as whether it is a metadata table or a results table.
#' This dataframe is used for the `getTabs` function.
#' @param con MariaDBConnection; connection to a ARTMO Database
#' @import DBI
#' @import dplyr
#' @import purrr
#' @import stringr
#' @importFrom tibble as.tibble
#' @export
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

#' @title Return ARTMO Tables
#' @description This is the second function necessary for the extraction and transformation of the MYSQL back-end data.
#' The function is necessary for reading the MYSQL Tables based on their link to each other.
#' Aside the **con** parameter the function needs the linkage inormation provided by the `getLinks`.
#' @param con MariaDBConnection; connection to a ARTMO Database
#' @param links tibble; Link between the MySQL tables
#' @import DBI
#' @import dplyr
#' @import purrr
#' @import stringr
#' @importFrom glue glue
#' @importFrom tibble as.tibble
#' @importFrom tidyr separate
#' @export 
getTabs <- function(con,links){

  toload<-links %>% 
    select(Database,Table_Type,Table_Name,Table,Rank,Count,result)
  
  stat.tabs<-toload %>% 
    dplyr::mutate(MyTables=pmap(.,function(...,Table_Type,Table,result){
    
    #' Machine Learning
    if(Table_Type=="test_mla"){
      
      # Read Table
      tab<-dbGetQuery(con,glue("select * from {Table}"))
      tabn<-colnames(tab)
      
      # Transform Column Values
      tab<-as.tibble(tab)
      if(any(tabn=="wl"))     tab<-.mlr.wl(tab)
      if(any(tabn=="plotcv")) tab<-.mlr.plotcv(tab)
      if(any(tabn=="staticscv")) tab<-.mlr.staticscv(tab)
      
      # Delete Columns
      if(any(tabn=="mla_settigs"))  tab<-tab %>% dplyr::select(-"mla_settigs")
      if(any(tabn=="general_info")) tab<-tab %>% dplyr::select(-"general_info")
      if(any(tabn=="name_parameter")) tab<- tab %>% rename(parameter=name_parameter)
      if(any(tabn=="name_algoritmo")) tab<- tab %>% rename(algorithm=name_algoritmo)
    }
      
    #' Lookup Table Inversion
    if(Table_Type=="test_cf"){
      
      # Read Table
      isnoise<- any(grepl("noise",Table))
      if(isnoise==F) tab<-dbGetQuery(con,glue("select * from {Table}"))
      if(isnoise==T) tab<-dbGetQuery(con,glue("select ID_T7,id_t6,noise from {Table}"))
      tabn<-colnames(tab)
      
      # Transform Columns
      tab<-as.tibble(tab)
      if(any(tabn=="general_info")) tab<-.cf.generalinfo(tab)
      if(any(tabn=="spectros_user"))tab<-.cf.spectros(tab)
      if(any(tabn=="param_user"))   tab<-.cf.paramuser(tab)
      if(any(tabn=="lut"))          tab<-.cf.lut(tab)
      if(any(tabn=="resultados"))   tab<-.cf.resultados(tab)
      
      # Rename Columns
      if(any(tabn=="name_parameter")) tab<-tab %>% separate(name_parameter,into=c("Name1","Name2"),sep=":")
      if(any(tabn=="name_algoritmo")) tab<-tab %>% rename(algorithm=name_algoritmo) 
        
    }
    
    #' Indices (In the Pipeline, too big to join for now -> Reduce dimensionality?)
    if(Table_Type=="vis_test") {
      
      if(result==1){
        
        get<-"ID_T10,id_t9,bands,bands1,parametros,parametros1,ME,RMSE,RELRMSE,MAE,R,R2,NRMSE,TS,NSE"
        tab<-dbGetQuery(con,glue("select {get} from {Table}")) %>% as.tibble
        
      } else {
        
        tab<-dbGetQuery(con,glue("select * from {Table}")) %>% as.tibble
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

#' @title Join ARTMO Tables
#' @description This is the third function necessary for the combination of the extracted Tables
#' Here all the Tables generated beforehand are combined in the respective order with the others.
#' The tables are saved as tibbles and differred by the Table Type (MLRA, LUT CF, VIs).
#' Aside the **con** parameter the function needs the linkage inormation provided by the `getTabs`
#' @param tabs nested tibble; table extraction done by `getTabs`
#' @param removeid boolean; Shall the ID be removed?
#' @import dplyr
#' @import purrr
#' @import stringr
#' @importFrom tibble as.tibble
#' @importFrom tidyr nest separate
#' @export
doJoin<-function(tabs,removeid=F){
  
  # Group and perform operation by Table Type (each class single)
  t1<-tabs %>% group_by(Database,Table_Type) %>% nest
  t2<-t1 %>% 
    dplyr::mutate(Metrics=pmap(.,function(...,Database,Table_Type,data){
    
    # Join all the Metainformation
    meta<- data %>% filter(result==0)
    
    if(nrow(meta)>0){
      
      for(i in 2:nrow(meta)){
        
        if(i==2) tib.meta<-meta$MyTables[[i-1]]
        tib.join<-meta$MyTables[[i]]
        tib.meta<-left_join(tib.meta,tib.join)
        
      }
      
    } else {tib.meta<-NA}
    
    # Join all the Results
    result<- data %>% filter(result==1)
    
    if(nrow(result)>0){
      
      tib.res<-do.call(rbind,result$MyTables)
      
    } else {tib.res<-NA}
    
    # JOin Metainformation and Results
    if(is.na(tib.meta) && !is.na(tib.res)) out<-tib.res
    if(!is.na(tib.meta) && is.na(tib.res)) out<-tib.meta
    if(!is.na(tib.meta) && !is.na(tib.res)) out<-left_join(tib.meta,tib.res)
    
    
    if(removeid==T) out<-.removeid(out)
    
    out$Store.ID<-seq(1:nrow(out))
    out$Database<-Database
    out$Table_Type<-Table_Type
    return(out)
    
  }))
  
  t3<-t2 %>% dplyr::select(-data)
  return(t3)
 
}

#' @title MYSQL Processor
#' @description This function unites all steps individually done for accessing the MYSQL back-end.
#' This allows the combined action of the three functions `getLinks`,`getTabs` and `doJoin`
#' @param con MariaDBConnection; connection to a ARTMO Database
#' @param removeid boolean; Shall the ID be removed?
#' @export
getMYSQL<-function(con,removeid=F){
  
  gl<-getLinks(con)
  gt<-getTabs(con,gl)
  dj<-doJoin(gt,removeid)
  return(dj)
  
}

