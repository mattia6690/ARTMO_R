#### Test script for Dessis DB

# Environment -------------------------------
source("R/Packages.R")
source("R/Essentials.R")
source("R/Fun_MySQL.R")
source("R/Fun_Binary.R")
source("R/Fun_Stats_CF.R")
source("R/Fun_Stats_MLA.R")
source("R/Fun_Stats_VIs.R")
source("R/Fun_Stats_All.R")
source("R/Fun_Plot.R")

directory <- "C:/ARTMO/"
user <- "root"
host <- "localhost"
password <- "123456"

# Connection --------------
con.raw <- connect.raw(user, password, host)
is<-is.artmodb(con.raw, user, password, host)

database<-is[1]
con <- connect.db(user, password, host, database)

# Database ----------------------------------
db.tabs<-get.tables.db(con)

# Gather --------------------------------
dblinks  <- getLinks(con)
dbtabs   <- getTabs(con,dblinks)
dbjoin   <- doJoin(dbtabs,removeid = T)

#saveRDS(dbjoin$Metrics[[1]],"docs/data/demo.rds")

# Analysis --------------------------------
#* Type --------------------------------------
stat    <- dbjoin$Metrics[[1]]
stat.td <- FormatTidy(stat,what="CF")

#saveRDS(dbjoin$Metrics[[1]],"docs/data/demo.rds")


# End ---------------------------------------

#' For all other Visualizations etc. please consult the /docs section and run the code embedded
#' in the Rmarkdown Files
#' Enjoy!!