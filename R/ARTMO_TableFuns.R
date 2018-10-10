

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
    wavel <-as.numeric(wavel.raw) %>% as.tibble %>% setNames(master.i$NAMESENSOR)
    
  } else{
    
    wavel.raw <- x$BANDAS %>% str_split(.,",") %>% unlist
    wavel <-as.numeric(wavel.raw) %>% as.tibble %>% setNames(master.i$NAMESENSOR)
  }
  
  return(wavel)
}

database  <-"atmotest"
directory <- "K:/SentinelVegetationProducts/S2_LAI/Databases/"
artmo_env(database = database,dir=directory)

artmo_env<<-function(database,directory,user="root",host="localhost",pw="123456"){
  
  # Connect to the ARTMO Server
  con    <- dbConnect(MySQL(), user=user, password= pw, dbname=database, host=host)
  tabs   <- dbListTables(con) %>% .[-{which(.=="master")}]
  
  # Read the masterfile and Create Ws as well as txt output
  master <- dbReadTable(con,"master")
  dir.database<-paste(directory,database,"/")
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
      options(warn=0)
    }
  }
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


