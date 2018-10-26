
source("R/Essentials.R")

database  <-"artmo_1"
directory <- "K:/SentinelVegetationProducts/S2_LAI/Databases/"
user="root";host="localhost";pw="123456"

# Connect to the ARTMO Server
con    <- dbConnect(MariaDB(), user=user, password= pw, dbname=database, host=host)

# Step 0: Connect to ARTMO

artmo_con<-function(database=NULL,user,password,host){
  
  if(is.null(database)) {
    
    con<-dbConnect(MariaDB(), user=user, password= password, host=host)
    
  } else {
    
    con<-dbConnect(MariaDB(), user=user, password= password, dbname=database, host=host)
    
  }
  return(con)
  
}

# Step 0.5
artmo_getmaster<- function(con){
  
  master  <- RMariaDB::dbReadTable(con,"master") %>% 
    as.tibble %>% 
    select(-emulator)
  
}


# Step 1: Set the Directory
artmo_set_directory <- function(con,dir){
  
  # Get Masterfile and Set the Database Environmen
  dir.database<-paste0(dir,dbGetInfo(con)$dbname,"/")
  dircheckup(dir.database)
  
  master<-artmo_getmaster(con)
  
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
        
        tabs <- dbGetQuery(con,"show tables")
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


artmotab.checker<-function(con){
  
  databs<-dbGetQuery(con,"show databases") %>% unlist
  
  eval<-map(databs,function(x){
    
    con.db<-artmo_con(user=user,password=pw,host=host,database = x)
    tabs<- dbGetQuery(con.db,"show tables")
    is.element("master",unlist(tabs))
    
  })
  names(eval)<-databs
  choix<-names(which(eval==T))
}

con<-artmo_con(user=user,password=pw,database=database,host=host)
direx<-artmo_set_directory(con,directory)

connect.raw<-artmo_con(user=input$sql.user,
                       password=input$sql.pw,
                       database=input$sql.db,
                       host=input$sql.host)

gq<-dbGetQuery(con,"show databases") 
map(gq,function(x){
  db1  <- dbGetQuery(con,paste("connect",x,";"))
  tabs <- dbGetQuery(db1,"show tables")
})

readRDS("C:/ARTMO/artmo_1/Mattia1/1/4SAIL_Model_Metrics.rds")


