
# Load Functions
source("Essentials.R")
source("MySQL_Functions.R")

# Load Shiny
library("shiny")
library("shinydashboard")

# Shiny Functions
#' Get the Tables of the Models
get.simtable<-function(path){
  
  path %>% 
    list.files(.,pattern = "_Model_Metrics",full.names = T) %>% 
    map(.,readRDS) %>% 
    do.call(rbind,.)
}

