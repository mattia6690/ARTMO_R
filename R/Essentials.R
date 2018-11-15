
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



