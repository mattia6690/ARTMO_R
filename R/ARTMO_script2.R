#### Test script for Dessis DB

# Environment -------------------------------
source("R/Essentials.R")
source("R/Fun_MySQL.R")
source("R/Fun_Binary.R")
source("R/Fun_Stats_CF.R")
source("R/Fun_Stats_MLA.R")
source("R/Fun_Stats_All.R")
source("R/Fun_Plot.R")

directory <- "C:/ARTMO/"
user <- "root"
host <- "localhost"
password <- "123456"

# Connection --------------
con.raw <- connect.raw(user, password, host)
is<-is.artmodb(con.raw, user, password, host)

database<-is[2]
con <- connect.db(user, password, host, database)

# Database ----------------------------------
db.tabs<-get.tables.db(con)

# Statistics --------------------------------
stat.all<-get.stat.tab(con) 
stat.tabs<-getStats(con,stat.all)
stat.join<-joinTabs(stat.tabs)
stat.join.id<-map(stat.join$Tables,removeid)

