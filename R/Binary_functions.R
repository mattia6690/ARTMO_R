# Transforms the Raw Files in Artmo

rawTrans<-function(input){
  
  ref<-read.table("data/Datatypes.txt",sep=" ") %>% setNames(c("ID","Bits","Name"))
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

max <- row$Bits*2
x <- seq_along(number.raw)
d1 <- split(number.raw, ceiling(x/max))
map_dbl(d1,function(x) readBin(x,what="numeric",size=row$Bits))

x <- seq(1,length(number.raw),8)
test<-split(number.raw,x)

# END/TEST --------------------------------------

test<-costs[[1]][[7]]

a<-rawTrans(input=test$resultados[[1]])
b<-rawTrans(input=test$resultados[[2]])

plot(a$numbers,b$numbers)

original<- costs[[1]][[2]]$param_user[[1]] %>% filter(param_user=="label") %>% select(param_user_val) %>% unlist(use.names = F)
modelled<- a$numbers
plot(original, modelled)

donde<-rawTrans(input=costs[[1]][[6]]$donde[[2]])
cuando<-rawTrans(input=costs[[1]][[6]]$cuanto[[2]])

number.raw %>% rawToBits() %>% .[1:4]

number.raw %>% rawToBits() %>% .[1:4] %>% paste(.,collapse = "")

# raw<-costs[[7]]$resultados[[1]]
# number.raw<- raw[13:length(raw)]
# n<- length(number.raw)/8
# number    <- readBin(number.raw, numeric() ,n=n,size=4)
# 
# 
#   bind      <- rep(1:length,bit)
#   bynumber  <- cbind.data.frame(bind,numbers) %>% as.tibble %>% group_by(bind) %>% nest
#   
#   deciph<-map(bynumber$data, function(x){
#     
#     raw1<-unlist(x,use.names = F)
#     if(what=="numeric") ret<-readBin(raw1,"numeric")
#     if(what=="character") ret<-readBin(raw1,"character")
#     
#   }) %>% unlist
#   
# }

number.raw<- input[13:length(input)]
ns<-length(number.raw)/row$Bits

if(row$Class=="numeric") {
  numbers   <- readBin(number.raw, what="double" ,n=ns,size=row$Bits,signed = row$Signed)
}
if(row$Class=="character") numbers   <- readBin(number.raw, what="character",n=length(number.raw) %/% 8)
