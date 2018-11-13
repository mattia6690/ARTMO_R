#' 
#' Modular Script for executing the unction and to access the ARTMO Databases
#' The Codes are divided in 
#' (i)    General Environment and Input 
#' (ii)   Connection to MySQL
#' (iii)  Exploration of Single Databases
#' (iv)   Generation of A File structure and automatically Export the Database information there
#' (v)    Retrieval of Statistics and accuracy
#' (vi)   Deployment of Shiny WebApp

# Environment -------------------------------
source("R/Essentials.R")
source("R/MySQL_Functions.R")
source("R/COST_functions.R")

directory <- "C:/ARTMO/"
user="root"
host="localhost"
pw="123456"

# Connection --------------
# Connect to the ARTMO Server
con <- connect.raw(user=user, password= pw, host=host)

# Check Which of the databases are ARTMO Databases
is<-is.artmodb(con,user=user, pw= pw, host=host)
database<-is[1]

# Connect to Database
con2 <- connect.db (user=user, password= pw, database=database, host=host)

# Exploration -------------------------------
# Tables within a database
db.tabs<-get.tables.db(con2)

# Master table within a database
db.master<- get.master.db(con2) %>% 
  select(ID_MASTER,ID_PY,ID_SIMULATION,
         MODELS=NAME_MODEL,
         PROJECT=NAME_PROYECT,
         DATE1,
         TIME_MODEL,
         NAMESENSOR,
         SIM=SIMULACIONES)


# File Structure -------------------------------
# With these two functions it is possible to build folders and subfolders based 
# Database and Project IDs. This could take a while
set.directory.artmo(con2,directory)
set.projects.artmo(con2,directory)


# Statistics -------------------------------
#' Function to access the COST functions within each database and to extract them in an R friendly way
#' The biggest problems are the Links between the various IDs as well as the conversions of Blob (Binary) to arrays
#' NOT FINISHED YET (09/11/18)

# Get all cost functions
costs.all<-get.stat.tab(con2) %>% mutate(ID_costs=1:nrow(.))

# Get Metainformation from cOST functions
costs.temp<-costs.all %>% group_by(Database) %>% nest
costs.meta<-costs.temp %>% 
  mutate(costs=map(data,function(x,c=con2) get.stat.meta(con2,x))) %>% 
  unnest(.preserve=data) %>% 
  unnest 

# Create the Output path 
costs.path<-costs.meta %>% 
  mutate(Path=pmap_chr(list(Database,Project,PY_ID,Model,ID_costs),
                       function(v,w,x,y,z,dir=directory) {
    
    pt1<-paste(dir,v,w,x,"Statistics/",sep="/")
    dircheckup(pt1)
    pt2<-paste0(y,"-",z)
    
    all<-paste0(pt1,pt2)
    return(all)
    
  }))

# Add the Tables MySQL tables
# ACTUAL Phase of Development
costs.path.split<-split(costs.path,1:nrow(costs.path))
costs.path$costs<-map(costs.path.split,function(x,c=con2) get.stat.metrics(c,x))

costs.path<-costs.meta %>% 
  group_by(Database,Model,Project,PY_ID,Date) %>% 
  nest %>% 
  mutate(Path=pmap_chr(
    list(Database,Project,PY_ID,Model), function(v,w,x,y,dir=directory) {
      
      pt1<-paste(dir,v,w,x,"Statistics",sep="/")
      pt2<-paste(pt1,y,sep="_")
      
    }))

costs.list<-costs.path %>% 
  mutate(Statistics=map(data,function(x,c=con2) ret<-get.stat.metrics(c,x)))

costs.list2<-costs.list %>% select(-data)

# Write the COST Tables
a<-map2_chr(costs.list$Path,costs.list$Statistics,function(x,y){
  
  name<-paste0(x,".rds")
  saveRDS(y,file = paste0(x,".rds"))
  paste(name,"    Successfully Exported")
  
})

# Shiny -------------------------------------
#' The Shiny Web-Application provides a possibility to use the presented functions
#' interactively within the browser. Works ONLY locally. 
#' MySQL not adressable when deployed in the Web (e.g. shinyapps.io)

source("R/ui.R")
source("R/server.R")
shinyApp(ui=ui,server=server)
