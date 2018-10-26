
source("R/Essentials.R")

database  <-"atmotest"
directory <- "K:/SentinelVegetationProducts/S2_LAI/Databases/"
user="root";host="localhost";pw="123456"

# Connect to the ARTMO Server
con    <- dbConnect(MariaDB(), user=user, password= pw, dbname=database, host=host)

# Step 0: Connect to ARTMO

artmo_con<-function(database,user,password,host){
  
  con    <- dbConnect(MariaDB(), user=user, password= password, dbname=database, host=host)
  return(con)
  
}

# Test is the table retrieved in the matlab file mysql_blob and contains an index, the number of slots and the name 

blob_index<-function(text){
  
  i1<-str_split(test,";...")[[1]] %>% 
    map(.,function(x) str_split(x,"\n")) %>%  
    unlist
  
  i2<- i1[-which(i1=="")] %>% 
    trimws() %>% 
    map(.,function(x) str_split(x,"'")) %>% 
    unlist(recursive=F)
  
  id.type<-i2 %>% lapply(.,"[[",2) %>% unlist 
  ids.raw<-i2 %>% lapply(.,"[[",1) %>% unlist %>% str_split(" ")
  id1<- ids.raw %>% lapply(.,"[[",1) %>% unlist
  id2<- ids.raw %>% lapply(.,"[[",2) %>% unlist
  
  bind_cols(Index1=as.integer(id1),Index2=as.integer(id2),Type=id.type)
}



# Step 1: Set the Directory
artmo_set_directory <- function(con,dir){
  
  # Get Masterfile and Set the Database Environmen
  dir.database<-paste0(dir,dbGetInfo(con)$dbname,"/")
  dircheckup(dir.database)
  
  master  <- RMariaDB::dbReadTable(con,"master") %>% 
    as.tibble %>% 
    select(-emulator)
  name<-paste0(dir.database,"Master.rds")
  if(!file.exists(name)) saveRDS(master,file=name)
  return(dir.database)
}



# Step 2: Set the Project Environment
artmo_set_projects <- function(con,dir){
  
  dir.database<-paste0(dir,dbGetInfo(con)$dbname,"/")
  master<-readRDS(paste0(dir.database,"Master.rds")) %>% as.tibble
  master.n<-master %>% group_by(ID_MASTER) %>% nest
  
  # Extract the Sensor Informatuion by MasterID
  master2<-master.n %>% 
    mutate(Sensor=map(data,function(dat){
      
      dir1 <-paste0(dir.database,dat$NAME_PROYECT,"/",dat$ID_PY,"_",dat$ID_SIMULATION,"/")
      dircheckup(dir1)
      
      sens.n  <- paste0(dir1,"/Sensor.txt")
      sensor  <- artmo.getSensor(dat)
      if(!file.exists(sens.n)) write.table(sensor,file=sens.n,row.names = F,sep = ",")
      return(sensor)
      
    }))
  
  # Extract the Model Information from each MasterID
  master3<- master2 %>% 
    mutate(Model=map2(ID_MASTER,data,function(id,dat2){
      
      metric.n  <- paste0(dir,dat2$NAME_MODEL,"_Metrics.rds")
      options(warn=-1)
      if(!file.exists(metric.n)){
        
        state1<-paste("where ID_MASTER =",id)
        pr<-purrr::map(tabs,function(x,s=state1){
          
          qr<-tryCatch({dbGetQuery(con,statement=paste("select * from",x,s))},error=function(e){NULL})
          if(length(qr)==0) qr<-NULL
          if(is.data.frame(qr) && nrow(qr)==0) qr<-NULL
          return(qr)
          
        })
        names(pr)<-tabs
        pr<-Filter(Negate(is.null),pr)
        saveRDS(pr,file = metric.n)
        return(pr)
      }
      options(warn=0)
    }))
}


con<-artmo_con(database,user,pw,host)
direx<-artmo_set_directory(con,directory)

#write.csv(blob_index(test),paste0(getwd(),"/data/Blob_index_file.csv"))


