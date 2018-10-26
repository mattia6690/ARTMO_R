

# Read the parametrization file
source("R/Essentials.R")

ARTMOdir<-"K:/SentinelVegetationProducts/S2_LAI"
Parfile<-paste0(ARTMOdir,"/Other/test3.mat")
getA.Para<-function(file){
  
  param1<-readMat(file)[[1]][,,1]
  n.para<-param1$param.ind[[1]]
  l1.para<-param1$parametros %>% unlist %>% matrix(nrow=3,byrow=F)
  l2.para<-param1$gparametros %>% unlist %>% matrix(nrow=3,byrow=F)
  noise.para <-noise.para %>% rep(3) %>% matrix(nrow=3,byrow=T)
  r<-cbind(n.para,l1.para,l2.para,noise.para)
  return(r)
  
}

# getA.Para((Parfile))
# 
# 
# 
# Cffile<-paste0(ARTMOdir,"/Other/CF_test.txt")

# COST Functions Table

getA.CFtable<-function(file){
  
  rl<-readLines(file) 
  
  cls <- rl[2] %>% str_split(.,":",simplify = T) %>% .[1,2] %>% str_replace(.," ","")
  var <- rl[3] %>% str_split(.,":",simplify = T) %>% .[1,2] %>% str_replace(.," ","")
  
  stat<- rl[4:length(rl)]
  rl<-stat %>%  map(function(x)str_split(x,"\t")[[1]])
  rl2<-do.call(rbind,rl)
  colnames(rl2)<-rl2[1,]
  rl2<-rl2 %>% as.tibble %>% slice(-1)
  return(rl2)
  
}


modeldir<-paste0(ARTMOdir,"/ARTMO_dir/0810_Prosail_Inversion/0810_Model2/")

lf.meta<-list.files(modeldir,pattern = "meta.txt",full.names = T)
lf.data<-str_replace(lf.meta,"_meta","")

tt<-c(lf.meta[1],lf.data[1])
tt.data<-lf.data[1]

one<-read.table(tt[2],sep=",")
meta<-read_table(tt[1])

nas<-meta %>% {which(is.na(.))}

meta.head<- meta[1:(nas[1]-2),] %>% unlist %>% str_replace(.,":","") %>% str_split(.,"  ")
names(meta.head)<-unlist(lapply(meta.head,"[[",1))
meta.head<-lapply(meta.head,"[[",2)


meta.vars<-meta[(nas[1]+2):(nas[2]-1),] %>% unlist %>% str_replace(.,":","") %>% str_split(.,"\t")
meta.vars<-

loadandinstall("RMySQL")
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


library("RMariaDB")

database  <-"atmotest"
directory <- "K:/SentinelVegetationProducts/S2_LAI/Databases/"
user="root";host="localhost";pw="123456"
artmo_env(database = database,dir=directory)

# Connect to the ARTMO Server
con    <- dbConnect(MariaDB(), user=user, password= pw, dbname=database, host=host)

# Step 0: Connect to ARTMO

artmo_con<-function(database,user,password,host){
  
  con    <- dbConnect(MariaDB(), user=user, password= password, dbname=database, host=host)
  return(con)
  
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

artmo_set_directory(con,directory)

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
}



artmo_env<<-function(database,directory,user="root",host="localhost",pw="123456"){
  
  # Connect to the ARTMO Server
  con    <- dbConnect(MariaDB(), user=user, password= pw, dbname=database, host=host)
  tabs   <- dbListTables(con) %>% .[-{which(.=="master")}]
  
  # Read the masterfile and Create Ws as well as txt output
  master <- RMariaDB::dbReadTable(con,"master") %>% as.tibble
  dir.database<-paste0(directory,database,"/")
  dircheckup(dir.database)
  write.table(master,file=paste0(dir.database,"/Master.txt"),row.names = F,sep = ",")
  
  # Loop through each single row of the masterfile and extract the metrics for each project
  for(i in 1:nrow(master)){
    
    # Masterfile
    master.i  <- master[i,]
    name      <- master.i$NAME_PROYECT
    id.master <- master.i$ID_MASTER
    id.py     <- master.i$ID_PY
    id.sim    <- master.i$ID_SIMULATION
    modelnm   <- master.i$NAME_MODEL %>% str_replace(.," ","")
    
    # Subdirectory and sa
    dir <-paste0(dir.database,name,"/",id.py,"_",id.sim,"/")
    dircheckup(dir)
    
    # Names of the Output File
    sens.n    <- paste0(dir,"Sensor.txt")
    metric.n  <- paste0(dir,modelnm,"_Metrics.rds")
    
    # Write the Sensor File
    sensor  <-artmo.getSensor(master.i)
    if(!file.exists(sens.n)) write.table(sensor,file=sens.n,row.names = F,sep = ",")
    
    #' Iterate through each of the available SQL Tables and extract the Tables with the containing the MASTER_ID of the MySQL iteration
    #' This trick returns a list with names of the respective table the MASTER_ID has been found 
    #' (containing Model Input, Validation Input and Retrieval Tables)
    #' Data is saved as rds if not already available
    options(warn=-1)
    if(!file.exists(metric.n)){
      
      state1<-paste("where ID_MASTER =",id.master)
      pr<-purrr::map(tabs,function(x,s=state1){
        
        qr<-tryCatch({dbGetQuery(con,statement=paste("select * from",x,s))},error=function(e){NULL})
        if(length(qr)==0) qr<-NULL
        if(is.data.frame(qr) && nrow(qr)==0) qr<-NULL
        return(qr)
        
      })
      names(pr)<-tabs
      pr<-Filter(Negate(is.null),pr)
      saveRDS(pr,file = metric.n)
      
    }
    options(warn=0)
  }
}


# Testsection -------------------------------------------------------------


# What I need

# Dplyr

tst<-src_mysql(user=user, password= pw, dbname=database, host=host)

tst.tab<-tbl(tst,"prospect4_t1")

dir.database<-paste0(directory,database,"/")
pt<-readRDS(paste0(dir.database,"0510_Prosail_Inversion_Doc2/14_2/4SAIL_Metrics.rds"))

id<-19

options(warn=-1)
p4.meta <- dbGetQuery(con,statement=paste("select * from prospect4_t1 where ID_MASTER =",id)) %>% as.tibble
p4.data <- dbGetQuery(con,statement=paste("select * from prospect4_t2 where ID_MASTER =",id)) %>% as.tibble


to.read<-p4.meta$data[[3]]
ln<-length(to.read)
vc<-p4.meta$vcount[[3]]
sp<-p4.meta$data
readBin(sp[[1]],what="numeric",n=20)

line1<-c(readBin(to.read,"int",5), 
         readBin(to.read,"double",1,size=4),
         readBin(to.read,"int",2))




s4.meta <- dbGetQuery(con,statement="select * from sail4_t1") %>% as.tibble
s4.data <- dbGetQuery(con,statement="select * from sail4_t2") %>% as.tibble

s.s4.data <- dbGetQuery(con,statement="select * from s_sail4_t2") %>% as.tibble

cf<-dbGetQuery(con,statement=paste("select * from test_cf")) %>% as.tibble
cf.alg<-dbGetQuery(con,statement=paste("select * from test_cf_algoritmo")) %>% as.tibble
cf.par<-dbGetQuery(con,statement=paste("select * from test_cf_parameter")) %>% as.tibble
cf.tra<-dbGetQuery(con,statement=paste("select * from test_cf_train")) %>% as.tibble
cf.mod<-dbGetQuery(con,statement=paste("select * from test_inv_cf_ppal")) %>% as.tibble


cf1<-dbGetQuery(con,statement=paste("select * from test_inv_cf_ppal")) %>% as.tibble


cf.T1<-cf1 %>% 
  mutate(T1=map(general_info,function(x){
    
    matall<-readMat(x)$data[,,1]
    matall.rtm<-matall$rtm[,,1] %>% 
      melt %>% 
      select(Type=L1,Value=value)
    
  })) %>% select(-general_info)

cf2<-cf.class<-dbGetQuery(con,statement=paste("select * from test_cf_class")) %>% as.tibble

map(test.tables,function(x) str_replace(x,"inv",""))

Stab<-dbGetQuery(con,statement=paste("show tables like '%test_%'")) %>% unlist(use.names = F)
Stab.replace<- map(test.tabs$Table,function(x) str_replace(x,"_inv",""))

# Collect all the Information from the Tables according to the Test prefix
test.tabs<-map(Stab,function(x){
  
  cnt  <- dbGetQuery(con,statement=paste("select count(*) from",x))[[1]] %>% as.numeric
  read <- dbGetQuery(con,statement=paste0("select * from information_schema.columns where table_name='",x,"' and column_name like 'id%'")) %>% as.tibble
  r1   <- read %>% 
    filter(TABLE_SCHEMA==database) %>% 
    select(Database=TABLE_SCHEMA,Table=TABLE_NAME,IDs=COLUMN_NAME)
  r1$Count<-cnt
  
  r1$Table_Type<-map_chr(r1$Table,function(x) .tableclass(x)[1])
  r1$Table_Name<-map_chr(r1$Table,function(x) .tableclass(x)[2])
  
  return(r1)
  
}) %>% do.call(rbind,.)

# Create the Real names and Type of Table
.tableclass<-function(x){
  
  y1<-str_replace(x,"_inv","")
  y2<-str_split(y1,"_") %>% unlist
  y3<-c(paste(y2[1],y2[2],sep = "_"),y2[3])
  return(y3)
}

# Sort the Table
test.tabs %>% arrange(Table_Type,IDs) %>% 

spectros <- cf2$spectros_user[[2]] %>% readMat(.)
spectros1<- spectros$data[,,1]
spectros2<- spectros1$spectral
rownames(spectros2)<-spectros1$wl
spectros3<- melt(spectros2) %>% 
  setNames(c("Wavelength","Iteration","Value")) %>% 
  as.tibble %>% 
  mutate(Spectrum=sprintf("%03s",as.character(Iteration)))

cf2$spectros_user

map(cf1$general_info,function(x){
  
  matall<-readMat(x)$data[,,1]
  matall.rtm<-matall$rtm[,,1] %>% 
    melt %>% 
    select(Type=L1,Value=value)
  
  trans<-t(matall.rtm)
  colnames(trans)<-trans[1,]
  trans<-trans[-1,] %>% t
}) %>% do.call(rbind,.)





cf3<-cf2 %>% select(-general_info) %>% unnest

matall<-readMat(cf.mod$general_info[[3]])$data[,,1]
matall.rtm<-matall$rtm[,,1] %>% 
  melt %>% 
  select(Type=L1,Value=value)

row.names(matall.rtm)<-matall.rtm$L1 %>% unlist



temp1<-matall.rtm %>% 
  unlist(recursive = F) %>% 
  as.tibble()


gi<-cf.mod$general_info[[1]] %>% readMat %>% .$data

melt(matall.rtm) %>% select(Type=L1,Value=value)



cf.class <- dbGetQuery(con,statement=paste("select * from test_cf_class")) %>% as.tibble
cf.noise <- dbGetQuery(con,statement=paste("select * from test_cf_noise")) %>% as.tibble
cf.beta  <- dbGetQuery(con,statement=paste("select * from test_cf_beta"))  %>% as.tibble
options(warn=0)

ds<-dbSendQuery(con,statement="select emulator from master;")
ds.f<-dbFetch(ds)
readBin(ds,what="raw")

emul<-master$emulator[[1]]
rm<-readMat(emul,fixNames = FALSE)



ncid<-data["ncid",,] %>% unlist(use.names = F)
datos<-data["datos",,][[1]]


flatten <- function(x) {
  if (!inherits(x, "list")) return(list(x))
  else return(unlist(c(lapply(x, flatten)), recursive = FALSE))
}

fl<-flatten(rm)

unlist(lapply(rm, function(x) 
  if (class(x) == "data.frame") list(x) else x), recursive=FALSE)

# Lookup Table Extraction


for(i in 1:length(rm)){
  
  
  
}




f <- function(x) {
  list(dim = dim(x), names = names(x))
}

lapply(rm,f)

lut1<-cf.class$lut

i<-"list"

while(i=="list"){
  
  data<-rm$data[[1]]
  
  
  
}


# name    <-master.i$NAME_PROYECT
# sensor  <-artmo.getSensor(master.i)
# sims    <-as.numeric(master.i$SIMULACIONES)
# date    <-master.i$DATE1
# modelnm <-master.i$NAME_MODEL
# modelid <-master.i$ID_MODEL
# tmodel  <-master.i$TIME_MODEL
# id      <-master.i$ID_MASTER
# 
# modelnm1<-str_replace(modelnm," ","") %>% str_c(.,c("_t1","_t2"))
# 
# options(warn=-1)
# state1<-paste("where ID_MASTER =",id.master)
# 
# pr<-purrr::map(tabs,function(x,s=state1){
#   
#   qr<-tryCatch({dbGetQuery(con,statement=paste("select * from",x,s))},error=function(e){NULL})
#   if(length(qr)==0) qr<-NULL
#   if(is.data.frame(qr) && nrow(qr)==0) qr<-NULL
#   return(qr)
#   
# })
# 
# names(pr)<-tabs
# pr<-Filter(Negate(is.null),pr)
# options(warn=0)
# 
# 
# map2(pr,names(pr),function(x,y,d=dir) write.table(x,file=paste0(d,y,".txt"),row.names = F,sep=","))
# 
# 
# folder<-
# 
# 
# model_t1 <- dbGetQuery(con,statement=paste("select * from",tabs[1],state1)) %>% dplyr::arrange(.,ID_T1)
# model_t2 <- dbGetQuery(con,statement=paste("select * from",modelnm1[2],state1)) %>% dplyr::arrange(.,ID_T2)
# 
# p4_t2 <- dbGetQuery(con,statement="select * from prospect4_t2")
# s4_t1 <- dbGetQuery(con,statement="select * from sail4_t1")
# s4_t2 <- dbGetQuery(con,statement="select * from sail4_t2")
# 
# vist1.eq <- dbGetQuery(con,statement="select * from vis_eq_t3")
# aux1 <-  dbGetQuery(con,statement="select * from aux_prospect4_t2")


