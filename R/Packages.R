
# Loadandinstall
loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1]))
{install.packages(mypkg)}; library(mypkg, character.only=TRUE) }

# Required Packages -------------------------------------------------------

# Init
loadandinstall("plyr")

# MySQL
loadandinstall("RMariaDB")

# Matlab
loadandinstall("R.matlab")

# String
loadandinstall("stringr")
loadandinstall("glue")

# Spatial
loadandinstall("raster")
loadandinstall("sf")

# Iterate
loadandinstall("purrr")

# Visualize
loadandinstall("ggplot2")

# Other
loadandinstall("tools")
loadandinstall("lubridate")

# Tidy (Always Last)
loadandinstall("readr")
loadandinstall("dplyr")
loadandinstall("tidyr")
loadandinstall("reshape2")
loadandinstall("magrittr")
loadandinstall("tibble")

