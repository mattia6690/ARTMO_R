# Script for genrally Executing the access to MySQL


# Environment -------------------------------
source("R/Essentials.R")
source("R/MySQL_Functions.R")
source("R/COST_functions.R")
source("R/COST_tables.R")


database  <-"artmo_1"
directory <- "K:/SentinelVegetationProducts/S2_LAI/Databases/"
user="root";host="localhost";pw="123456"

# Connect to the ARTMO Server
con <- connect.raw(user=user, password= pw, host=host)
con <- connect.db (user=user, password= pw, database=database, host=host)

costs<-get.cost.tabs(con)




# Run the Shiny WebApp
source("R/ui.R")
source("R/server.R")
shinyApp(ui=ui,server=server)
