#' Functions for connecting to MySQL databases with R
#' 
#' The functions are divided by steps taken for accessing the Databases
################################

# MySQL Connect ------------------------
#' These two function allow to connect to the MySQL databases. 
#' Suffix '.raw' indicates the overall connection and '.db' to an explicit database
connect.raw <- function(user,password,host) {dbConnect(MariaDB(), user=user, password= password, host=host)}
connect.db  <- function(user,password,host,database) {dbConnect(MariaDB(), user=user, password= password, dbname=database, host=host)}

# General -----------------------------------
#' This function checks wheather the Database is an ARTMO database or not
#' by seaching for a master file. In case other mySQL db have a master table
#' they will be detected as ARTMO Tables (Attention)
is.artmodb<-function(con,user,pw,host){
  
  databs<-dbGetQuery(con,"show databases") %>% unlist
  eval<-map(databs,function(x){
    
    con.db<- connect.db(user=user,password=pw,host=host,database = x)
    tabs  <- dbGetQuery(con.db,"show tables")
    is.element("master",unlist(tabs))
    
  })
  names(eval)<-databs
  choix<-names(which(eval==T))
}

#' Script for reading the Model databases and especially the blobs
#' Returns a tibble with distributions
read.models.artmo<- function(model){
  
  # Convert the Raw values to Numeric values
  # Assess the distribution of the values
  model.numbers<-model %>% 
    mutate(Values=map2(vcount,data,function(x,y){
      
      numbers  <-y[13:length(y)]
      bind <- rep(1:x,each=8)
      bynumber<-cbind.data.frame(bind,numbers) %>% as.tibble %>% group_by(bind) %>% nest
      
      deciph<-map_dbl(bynumber$data, function(x){
        
        raw<-unlist(x,use.names = F)
        ret<-readBin(raw,"numeric")
        
      })
    }))
  
  # Extract the Distributuion of the Parameters and indicate whether they are parametrized at all
  model.numbers.dist<- model.numbers %>% 
    mutate(Distribution=map_chr(DESCRIPCION,function(x){
      
      sp<-str_split(x," ") %>%  unlist
      distribution<-ifelse(length(sp)>1,sp[1],"Single")
      
    }))
  
  # Format the final table output
  model.result <- model.numbers.dist %>% 
    select(ParLong=NAME,ParShort=par,Distribution,Min=vmin,Max=vmax,Count=vcount,Values)
  
  # Return
  return(model.result)
  
}

# Getter -----------------------
#' Get the MASTER File
get.master.artmo<- function(con){
  
  master  <- RMariaDB::dbReadTable(con,"master") %>% 
    as.tibble %>% 
    select(-emulator)
  
}

#' Get the SENSOR File
get.sensor.artmo<-function(x){

  if(is.na(x$TIME_MODEL)){
    
    wavel.raw  <- x$BANDAS %>% 
      str_replace(.,"\\[","") %>% 
      str_replace(.,"\\]","") %>% 
      str_split(.,";") %>% unlist
    wavel <-as.numeric(wavel.raw) %>% as.tibble %>% setNames(x$NAMESENSOR)
    
  } else {
    
    wavel.raw <- x$BANDAS %>% str_split(.,",") %>% unlist
    wavel <-as.numeric(wavel.raw) %>% as.tibble %>% setNames(x$NAMESENSOR)
  }
  
  return(wavel)
}

# Setter -----------------------------
#' Set the Local Directory (Step I)
set.directory.artmo <- function(con,dir){
  
  # Get Masterfile and Set the Database Environmen
  dir.database<-paste0(dir,dbGetInfo(con)$dbname,"/")
  dircheckup(dir.database)
  
  master  <- get.master(con)
  name<-paste0(dir.database,"Master.rds")
  if(!file.exists(name)) saveRDS(master,file=name)
  return(dir.database)
}

#' Move the Project Tables to the Local Directories (Step II)
set.projects.artmo <- function(con,dir){
  
  dir.database<-paste0(dir,dbGetInfo(con)$dbname,"/")
  master<-readRDS(paste0(dir.database,"Master.rds")) %>% as.tibble
  master.n<-master %>% group_by(ID_MASTER) %>% nest
  
  # Extract the Sensor Information by MasterID
  master2<-master.n %>% 
    mutate(Sensor=map(data,function(dat){
      
      dir1 <-paste0(dir.database,dat$NAME_PROYECT,"/",dat$ID_SIMULATION,"/")
      dircheckup(dir1)
      dircheckup(paste0(dir1,"/Scenes/"))
      
      sens.n  <- paste0(dir1,"/Sensor.txt")
      sensor  <- artmo.getSensor(dat)
      if(!file.exists(sens.n)) write.table(sensor,file=sens.n,row.names = F,sep = ",")
      return(sensor)
      
    }))
  
  # Extract the Model Information from each MasterID
  
  for(i in 1:nrow(master)){
    
    x<-master[i,]
    
    dir1 <-paste0(dir.database,x$NAME_PROYECT,"/",x$ID_SIMULATION,"/")
    metric.n  <- paste0(dir1,x$NAME_MODEL,"_Model_Metrics.rds")
    options(warn=-1)
    if(!file.exists(metric.n)){
      
      tabs <- dbGetQuery(con,"show tables") %>% unlist(use.names = F)
      state1<-paste("where ID_MASTER =",x$ID_MASTER)
      model<-purrr::map(tabs,function(x,s=state1){
        
        qr<-tryCatch({
          dbGetQuery(con,statement=paste("select * from",x,s))},
          error=function(e){NULL})
        
        if(length(qr)==0) qr<-NULL
        if(is.data.frame(qr) && nrow(qr)==0) qr<-NULL
        
        if(!is.null(qr)) {
          qr<-qr %>% 
            dplyr::select(one_of("ID_MASTER", "NAME", "DESCRIPCION", "par", "vmin", "vmax", "vcount","data")) %>% 
            as.tibble
        }
        return(qr)
        
      })
      names(model)<-tabs
      model<-Filter(Negate(is.null),model)
      model<-model[[which(lapply(model,ncol)>1) %>% names]]
      model<-artmo_readModel(model)
      model<-model %>% add_column(Model=x$NAME_MODEL,.before = T)
      saveRDS(model,file = metric.n)
      options(warn=0)
    }
  }
}

