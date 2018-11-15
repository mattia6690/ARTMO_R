
#' Modular Script for executing the unction and to access the ARTMO Databases
#' The Codes are divided in 
#' (i)      General Environment and Input 
#' (ii)     Connection to MySQL
#' (iii)    Exploration of Single Databases
#' (iv)     Generation of A File structure and automatically Export the Database information there
#' (v)      Retrieval of Statistics and accuracy
#' (vi)     Basic Plots
#' (vii)    Deployment of Shiny WebApp

# Environment -------------------------------
source("R/Essentials.R")
source("R/MySQL_Functions.R")
source("R/COST_functions.R")
source("R/Plot_functions.R")

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

costs.list<-get.stat.artmo(con2)
db.path<-costs.list %>% glue_data('{directory}{database}/{Project}/{PY_ID}')
costs.list<-costs.list %>% mutate(Path=db.path)

# Different forms of Table representation
costs.list.short<-export.lyt(costs.list,what="short")
costs.list.long <-export.lyt(costs.list,what="long")
costs.list.stat <-export.lyt(costs.list,what="stat")
costs.list2<-costs.list %>% unnest

#saveRDS(costs.list.stat,file = paste0(directory,"/",database,"/Model_ouptut.rds"))

# Plotting ----------------------------------
#' Exploration of a Model of our choice within the costs.list

model<-"Model1"

#' Plot the Spectral Statistics generated
explore.spectral(model,costs)
explore.spectral(model,costs,colpal=rainbow(50))

# Plot the Model Statistics
explore.model.gg(model,costs.list.short)
explore.model.gg(model,costs.list.short,stat.subset="r2")

# Shiny -------------------------------------
#' The Shiny Web-Application provides a possibility to use the presented functions
#' interactively within the browser. Works ONLY locally. 
#' MySQL not adressable when deployed in the Web (e.g. shinyapps.io)

source("R/ui.R")
source("R/server.R")
shinyApp(ui=ui,server=server)

