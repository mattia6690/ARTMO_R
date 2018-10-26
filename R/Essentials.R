

# Required Packages -------------------------------------------------------
library("devtools")
install_github("mattia6690/Mfunctions");library(Mfunctions)
loadandinstall("raster")
loadandinstall("sf")
loadandinstall("tools")
loadandinstall("tidyverse")
loadandinstall("lubridate")
loadandinstall("magrittr")
loadandinstall("R.matlab")
loadandinstall("RMariaDB")


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