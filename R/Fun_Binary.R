# Transforms the Raw Files in Artmo

rawTrans<-function(input){
  
  ref<-suppressMessages(read_csv("data/Datatypes.csv"))
  
  #Check the Format
  format.raw<- input[1:12]
  format    <- readBin(format.raw, numeric() ,n=3,size=4)
  
  # Analyze the Content
  row   <- filter(ref,ID==format[1])
  
  number.raw<- input[13:length(input)]
  max <- row$Bits*2
  x <- seq_along(number.raw)
  d1 <- split(number.raw, ceiling(x/max))
  numbers<-map_dbl(d1,function(x) readBin(x,what="numeric",size=row$Bits))
  
  tst<-list()
  tst$datatype  <- format
  tst$numbers   <- numbers
  return(tst)
}

