
# Connect -----------------------------------

#' @title Connect to ARTMO
#' @description This function provides the core connection to the MySQL backend. 
#' By digitizing the MySQL credential and optionally the database R automatically connects to the MYSQL backend.
#' @param user character; MYSQL Username (default = root)
#' @param password character; MYSQL password (default = 123456)
#' @param host character; Host of the MySQL Server (default = localhost)
#' @param database character; Name of the Database you would like to use. If left empty all the databases are returned.
#' @param getDB boolean; Would you like to return the Databases available? 
#' This option is only available if the database is not specified before
#' @importFrom DBI dbConnect dbGetQuery
#' @export
connect.artmo <- function(user,password,host,database=NULL,getDB=F){
  
  if(is.null(database)) {
    
    con<-dbConnect(MariaDB(), user=user, password= password, host=host)
    if(isTRUE(getDB)) con<- unlist(dbGetQuery(con,"show databases"))
    
  }else{
    
    con<-dbConnect(MariaDB(), user=user, password= password, dbname=database, host=host)
    if(isTRUE(getDB)) warning("You cannot return a list of Databases when a databases is already specified")
    
  }
  
  return(con)
}


# Getter -----------------------

#' @title List ARTMO Tables
#' @description This function lists all the tables available in the ARTMO MYSQL backend. 
#' It helps to understand how much data is stored by table and to specifically address 
#' the table on interest during the concatentation of tables
#' @param con MariaDBConnection; connection to a ARTMO Database
#' @import dplyr
#' @import RMariaDB
#' @importFrom magrittr "%>%"
#' @export
db.tables<-function(con){
  
  ret<-dbGetQuery(con,"show table status") %>% 
    filter(Rows>0) %>%
    mutate(Rows=as.numeric(Rows),
           Avg_row_length=as.numeric(Avg_row_length),
           Data_length=as.numeric(Data_length)) %>% 
    select(Name,Rows,Avg_row_length,Data_length)
  
  return(ret)
  
}

#' @title Get the ARTMO Master Table
#' @description This function returns the master table. 
#' This functionality is limited to the LUT since the other apporaches do not provide a master table.
#' @param con MariaDBConnection; connection to a ARTMO Database
#' @import dplyr
#' @import RMariaDB
#' @importFrom tibble as.tibble
#' @importFrom magrittr "%>%"
#' @export
db.master<- function(con){
  
  master  <- RMariaDB::dbReadTable(con,"master") %>% 
    as.tibble %>% 
    select(-emulator)
  return(master)
  
}

#' @title Get the ARTMO Sensor Information
#' @description This function returns the sensor information by iteration. 
#' This function has not yet been tested for each of each approach.
#' @param con MariaDBConnection; connection to a ARTMO Database
#' @import dplyr
#' @import stringr
#' @import RMariaDB
#' @importFrom stats setNames
#' @importFrom magrittr "%>%"
#' @importFrom tibble as.tibble
#' @export
db.sensor<-function(con){

  x<-db.master(con)
  
  if(is.na(x$TIME_MODEL)){
    
    wavel.raw  <- x$BANDAS %>% 
      str_replace(.,"\\[","") %>% 
      str_replace(.,"\\]","") %>% 
      str_split(.,";") %>% 
      unlist
    
    wavel <-as.numeric(wavel.raw) %>% 
      as.tibble %>% 
      setNames(x$NAMESENSOR)
    
  } else {
    
    wavel.raw <- x$BANDAS %>% 
      str_split(.,",") %>% 
      unlist
    
    wavel <-as.numeric(wavel.raw) %>% 
      as.tibble %>% 
      setNames(x$NAMESENSOR)
  }
  
  return(wavel)
}
