# ShinyFuns
library("devtools")
install_github("mattia6690/Mfunctions");library(Mfunctions)
loadandinstall("raster")
loadandinstall("sf")
loadandinstall("tools")
loadandinstall("tidyverse")
loadandinstall("lubridate")
loadandinstall("magrittr")
loadandinstall("R.matlab")
loadandinstall("RMariaDB")


loadandinstall("shiny")
loadandinstall("shinydashboard")
loadandinstall("shinyFiles")

# Connect to MySQL --------------------------
artmo_con<-function(database=NULL,user,password,host){
  
  if(is.null(database)) {
    
    con<-dbConnect(MariaDB(), user=user, password= password, host=host)
    
  } else {
    
    con<-dbConnect(MariaDB(), user=user, password= password, dbname=database, host=host)
    
  }
  return(con)
  
}


# Get the Sensor Information from ARTMO Master File
artmo.getSensor<-function(x){
  
  ti<-x$TIME_MODEL
  if(is.na(ti)){
    
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

# Check whether a MySQL Table is in ARTMO format
artmotab.checker<-function(con,user,pw,host){
  
  databs<-dbGetQuery(con,"show databases") %>% unlist
  
  eval<-map(databs,function(x){
    
    con.db<-artmo_con(user=user,password=pw,host=host,database = x)
    tabs<- dbGetQuery(con.db,"show tables")
    is.element("master",unlist(tabs))
    
  })
  names(eval)<-databs
  choix<-names(which(eval==T))
}

# Connect to Master --------------------------
artmo_getmaster<- function(con){
  
  master  <- RMariaDB::dbReadTable(con,"master") %>% 
    as.tibble %>% 
    select(-emulator)
  
}


# Connect to Project --------------------------
artmo_set_directory <- function(con,dir){
  
  # Get Masterfile and Set the Database Environmen
  dir.database<-paste0(dir,dbGetInfo(con)$dbname,"/")
  dircheckup(dir.database)
  
  
  master<-artmo_getmaster(con)
  
  name<-paste0(dir.database,"Master.rds")
  if(!file.exists(name)) saveRDS(master,file=name)
  return(dir.database)
}

artmo_readModel<- function(model){
  
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

artmo_set_projects <- function(con,dir){
  
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
