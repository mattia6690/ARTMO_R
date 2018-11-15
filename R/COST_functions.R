#' Functions for Extracting the COST Functions
#' 
#' The functions are divided by steps taken for accessing the Databases
################################'

source("R/Binary_functions.R")

# General -----------------------------------
# Create the Real names and Type of Table
cost.tableclass<-function(x){
  
  y1<-str_replace(x,"_inv","")
  y2<-str_split(y1,"_") %>% unlist
  y3<-c(paste(y2[1],y2[2],sep = "_"),y2[3])
  return(y3)
}

# Blobs to Tibble--------
# Change of the Param_user Matlab Tab
costfun.param_user<-function(table){
  
  istab<-any(names(table)=="param_user")
  if(isTRUE(istab)){
    
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

# Change the LUT in the "Class" Table
costfun.lut<-function(table){
  
  istab<-any(names(table)=="lut")
  if(isTRUE(istab) ){
    
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
  } else {out <- table}
  return(out)
}

costfun.resultados<-function(table){
  
  istab<-any(names(table)=="resultados")
  if(isTRUE(istab) ){
    
    raws<-table$resultados
    numbs<-map(raws,function(x) rawTrans(x)$numbers)
    out<-table %>% mutate(resultados=numbs)
    
    return(out)
  } else {return(table)}
}

# Tables ------------------------------------
#' Checks which Tables are available in the MYSQL Database relating to LUT inversion
get.stat.tab<-function(con) {
  
  # Which Tables are actually available in the MySQL file
  Stab<-dbGetQuery(con,statement=paste("show tables like '%test_%'")) %>% unlist(use.names = F)
  Stab.replace<- map(Stab,function(x) str_replace(x,"_inv",""))
  
  # Collect all the Information from the Tables according to the Test prefix
  cost.tabs.all<-map(Stab,function(x){
    
    cnt  <- dbGetQuery(con,statement=paste("select count(*) from",x))[[1]] %>% as.numeric
    read <- dbGetQuery(con,statement=paste0("select * from information_schema.columns where table_name='",x,"' and column_name like 'id%'")) %>% as.tibble
    r1   <- read %>% 
      filter(TABLE_SCHEMA==database) %>% 
      select(Database=TABLE_SCHEMA,Table=TABLE_NAME,IDs=COLUMN_NAME)
    r1$Count<-cnt
    
    r1$Table_Type<-map_chr(r1$Table,function(x) cost.tableclass(x)[1])
    r1$Table_Name<-map_chr(r1$Table,function(x) cost.tableclass(x)[2])
    
    return(r1)
    
  }) %>% do.call(rbind,.)
  
  arr<-   cost.tabs.all %>% arrange(Table_Type,IDs) 
  from<-  arr %>% filter(grepl("ID_T",IDs)) %>% mutate(ID_from=IDs) %>% select(-IDs)
  to <-   arr %>% filter(grepl("id_t",IDs)) %>% mutate(ID_to=IDs) %>% select(-IDs)
  cost.tabs     <- left_join(from,to) %>% filter(Count>0)
  
  return(cost.tabs)
}


#' Return the Metainformation stored in the first table
#' The first COST Table (id_T1) is afterwards removed and the data
#' grouped the final output folder too guarantee a right connection
get.stat.meta<-function(con,tables){
  
  # Search for the first Table
  begin<-tables %>% filter(ID_from=="ID_T1")
  cf1.read<-dbGetQuery(con,statement=paste("select * from",begin$Table)) %>% as.tibble
  
  #' Search for the important information (RTM, DB, PROJECT etc.)
  #' It is a Malab File -> Workaround
  meta<-map_df(cf1.read$general_info,function(x){
    
    matall<-readMat(x)$data[,,1]
    matall.rtm<-matall$rtm[,,1] %>% 
      melt %>% 
      select(Type=L1,Value=value) %>% 
      filter(Type!="rtm") %>% t %>% as.tibble
    colnames(matall.rtm)<-matall.rtm[1,]
    matall.rtm<-matall.rtm %>% dplyr::select(database,project,id,date)
    matall.rtm<-matall.rtm[-1,]
    return(matall.rtm)
    
  })
  
  # Bind the results and rename the columns
  cf1<- bind_cols(cf1.read,meta) %>% dplyr::select(-general_info)
  colnames(cf1)<-c("ID_T1","Model","Database","Project","PY_ID","Date")
  
  return(cf1)
}

#' Extracts the value for all the COST Tables (beisides the first one, see above)
#' Builds df returns from Blobs depending on the Column
#' Joins all the MySQL Files to one table
get.stat.metrics<-function(con,table,verbose=F){
  
  ##################################'
  if(verbose==T) cat("Starting...")
  lines<-str_detect(table$ID_to,"id_t[:digit:]") %>% which
  if(length(lines)<1) return(NA)
  cost.tabs.used<- table %>% slice(lines)
  
  tabs<-unique(cost.tabs.used$Table)
  tabs.names<-unique(cost.tabs.used$Table_Name)
  tabs.id<-unique(table$ID_T1)
  
  #################################'
  if(verbose==T) cat("Reading...")
  tabs.all<-map2(tabs,tabs.names,function(x,y,id=tabs.id){
    
    # No Donde and Cuando. Take too much space na dare not crucial
    if(y!="noise") read<-dbGetQuery(con,statement=paste("select * from",x))
    if(y=="noise") read<-dbGetQuery(con,statement=paste("select ID_T7,id_t6,noise from",x))
    if(any(colnames(read)=="id_t1")) read<-filter(read,id_t1==id)
    return(read)
    
  })
  
  #################################'
  if(verbose==T) cat("Transforming...")
  tabs.all2<-map(tabs.all,function(x){
    
    read<-as.tibble(x)
    read<-costfun.spectros(read)
    read<-costfun.param_user(read)
    read<-costfun.lut(read)
    read<-costfun.resultados(read)
    return(read)
    
  })
  
  #################################'
  if(verbose==T) cat("Joining...")
  for(i in 2:length(tabs.all2)){
    
    if(i==2)  join<-.sqljoin(tabs.all2[[i-1]],tabs.all2[[i]])
    if(i>2)   join<-.sqljoin(join,tabs.all2[[i]])
    
  }
  
  #################################'
  if(verbose==T) cat("Formatting...\n")
  join<-join %>% select(-contains("id_"))
  return(join)
}


get.stat.artmo<-function(con){
  
  # Get all cost functions
  costs.all<-get.stat.tab(con) %>% mutate(ID_costs=1:nrow(.))
  
  # Get Metainformation from cOST functions
  costs.temp<-costs.all %>% group_by(Database,Table_Type) %>% nest
  costs.meta<-costs.temp %>% 
    mutate(costs=map(data,function(x,c=con2) get.stat.meta(con,x))) %>% 
    unnest(.preserve=data) %>% 
    unnest 
  
  # Extract the actual Statistics for each Iteration
  costs.nest<-costs.meta %>% 
    group_by(Database,Model,Project,PY_ID,Date) %>% 
    nest %>% 
    mutate(Statistics=map2(data,Model,function(x,y,c=con) {
      
      print(paste("Process Statistics of Model:",y))
      get.stat.metrics(c,x)
      
    })) %>% select(-data)
  
}

# Table Layout

export.lyt<-function(table,what="all"){
  
  if(what=="short") table<-table
  if(what=="long")  table<-unnest(table)
  if(what=="stat"){
    
    stats<-map(table$Statistics, function(x){
      
      x %>% dplyr::select(param_user,name_parameter,
                          name_algoritmo,noise,
                          me,rmse,relrmse,mae,r,r2,nrmse,ts,nse,tictoc,
                          resultados)
    })
    
    table$Statistics<-stats
    
  }
  return(table)
}



