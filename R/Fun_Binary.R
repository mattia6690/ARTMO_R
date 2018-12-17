# Transforms the Raw Files in Artmo

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

