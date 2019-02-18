

#' @title Recursively generate Directories
#' @description After the access, cleaning and join of tables they can be exported from R. 
#' The *buildpath* function is useful to create the necessary folder infrastructure on the file system based on a specified directory
#' @param dbjoin Tibble; Tibble containing the joined ARTMO Database
#' @param mydir character; Location of a Directory
#' @param foldersetup boolean; Do you want to automatically create a folder structure based on the specified directory?
#' @import purrr
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom glue glue
#' @importFrom tidyr nest
#' @export
buildpath<-function(dbjoin,dir="",foldersetup=F){
  
  dircheckup  <- function(mydir) {
    
    sp<-str_split(mydir,"/")[[1]]
    for(i in 2:length(sp)){
      mydir2<-paste(sp[1:i],collapse="/")
      if(dir.exists(mydir2)==F){dir.create(mydir2)}
    }
  }
  
  db1<-dbjoin %>% 
    mutate(Dir=pmap(.,function(...,Database,Table_Type,Metrics){
      
      gl<-glue("{dir}/{Database}/{Table_Type}/{Metrics$Model}/")
      return(as.character(gl))
      
    }))
  
  db2<-db1 %>% 
    select(-Metrics) %>% 
    unnest %>% 
    unique
  
  if(foldersetup==T) map(db1$Dir,dircheckup)
  return(db2)
  
}

#' @title Create Tidy Data Arrays from Results
#' @description This function helps the user to format the Results table based. 
#' This functions allows to reduce the information of a table on the level of statistical accuracy
#' @param jtable Tibble; Tibble containing the joined ARTMO Database
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter
#' @importFrom tidyr gather
#' @export
formatTidy<-function(jtable){
  
  tab<-jtable %>% 
    filter(!is.na(R2)) %>% 
    gather(.,key=Statistic,value=Value,ME,RMSE,RELRMSE,MAE,R,R2,NRMSE,NSE)
  
  return(tab)
  
}

#' @title Condense ARTMO Dataset
#' @description This function helps the user to format the Results table based. 
#' This functions allows to reduce the information of a table on the level of statistical accuracy
#' @param jtable Tibble; Tibble containing the joined ARTMO Database
#' @param model character; Reduction by Model (Optional)
#' @param standard boolean; Standard way of standardization (model, parameter and statistics are ignored)
#' @param parameters character;  Reduction by Parameter(s) (Optional)
#' @param statistics character;  Reduction by certain statistics (Optional)
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter select
#' @importFrom tidyr gather
#' @export
formatARTMO<-function(jtable,standard=T,model=NULL,parameters=NULL,statistics=NULL){
  
  if(standard==T){
    
    tab<-jtable %>% 
      filter(!is.na(R2)) %>% 
      gather(.,key=Statistic,value=Value,ME,RMSE,RELRMSE,MAE,R,R2,NRMSE,NSE)
    
  } else {
    
    tab<-jtable %>% 
      select(Model,
             Store.ID,
             eval(statistics),
             eval(parameters),
             Results)
    
    if(!is.null(model)) tab<-tab %>% filter(Model==model)
    if(nrow(tab)==0) stop("Check your Inputs!")
    
  }
 
  return(tab)
}

#' @title ARTMO Blob Converter
#' @description The ARTMO Data mostly lies within MYSQL Tables. MySQL has the possibility to store data both in so called "Blobs".
#' Some of the Blobs the ARTMO Software generates are stored in Binaric within the Matlab Files. In order to address these properly,
#' we created this function. It searches the different parts of the Binary File including flags for the datatypes, length and others.
#' @param input raw; Raw Binary as stored in the MYSQL ARTMO Backend
#' @import dplyr
#' @importFrom readr read_csv
#' @export
rawTrans<-function(input){
  
  ref<-suppressMessages(read_csv("data/Datatypes.csv"))
  
  #Check the Format
  format.raw<- input[1:12]
  format    <- readBin(format.raw, numeric() ,n=3,size=4)
  
  # Analyze the Content
  row   <- filter(ref,ID==format[1])
  number.raw<- input[13:length(input)]
  
  bits<- row$Bits
  n1  <- length(number.raw)/bits
  numbers<-readBin(number.raw,what="numeric",size=bits,n=n1)
  
  tst<-list()
  tst$datatype  <- format
  tst$numbers   <- numbers
  return(tst)
}



# Transforms an lm function output to text usable for ggplots
# Thanks to Jodie Burchell (http://t-redactyl.io/)
r2.equation = function(x) {
  lm_coef <- list(a = round(as.numeric(coef(x)[1]), digits = 2),
                  b = round(as.numeric(coef(x)[2]), digits = 2),
                  r2 = round(summary(x)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq))
}

