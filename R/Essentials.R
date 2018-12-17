
# Loadandinstall
loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1]))
{install.packages(mypkg)}; library(mypkg, character.only=TRUE) }

# Dircheckup
dircheckup     <- function(mydir) {
  
  sp<-str_split(mydir,"/")[[1]]
  for(i in 2:length(sp)){
    mydir2<-paste(sp[1:i],collapse="/")
    if(dir.exists(mydir2)==F){dir.create(mydir2)}
  }
}

# Required Packages -------------------------------------------------------

library("raster")
library("sf")
library("tools")
library("tidyverse")
library("lubridate")
library("magrittr")
library("R.matlab")
library("RMariaDB")
library("reshape2")
library("glue")

# General Functions ---------------------------------------------------------------

# Transforms an lm function output to text usable for ggplots
# Greetings to Jodie Burchell (http://t-redactyl.io/)
r2.equation = function(x) {
  lm_coef <- list(a = round(as.numeric(coef(x)[1]), digits = 2),
                  b = round(as.numeric(coef(x)[2]), digits = 2),
                  r2 = round(summary(x)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq))
}

# Lists the Models per Table_Type as well as the Path
# If foldersetup is true the Folders will automatically be generated
buildpath<-function(dbjoin,dir="",foldersetup=F){
  
  db1<-dbjoin %>% 
    mutate(Dir=pmap(.,function(...){
    
    #met<-Metrics[[1]]
    gl<-glue("{dir}/{Database}/{Table_Type}/{Metrics[[1]]$Model}/")
    return(as.character(gl))
    
  }))
  
  db2<-db1 %>% select(-Metrics) %>% unnest %>% unique
  
  if(foldersetup==T) map(db1$Dir,dircheckup)

  return(db2)
}

# Format the Result to a tidy version:
# Statistics in one Column and Value representing the Value for each Statistics
FormatTidy<-function(jtable,what="CF"){
  
  tab<-jtable %>% 
    dplyr::filter(!is.na(R2)) %>% 
    tidyr::gather(.,key=Statistic,value=Value,ME,RMSE,RELRMSE,MAE,R,R2,NRMSE,NSE)
  
  return(tab)
  
}
