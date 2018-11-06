# Transforms the Raw Files in Artmo

rawTrans<-function(input){
  
  ref<-read.table("data/Datatypes.txt",sep=" ") %>% setNames(c("ID","Bits","Name"))
  
  #Check the Format
  format.raw<- input[1:12]
  format    <- readBin(format.raw, numeric() ,n=3,size=4)
  
  # Analyze the Content
  size    <- filter(ref,ID==format[1])$Bits
  
  number.raw<- input[13:length(input)]
  numbers   <- readBin(number.raw, numeric() ,n=length(number.raw) %/% 8,size=4)
  
  tst<-list()
  tst$datatype  <- format
  tst$numbers   <- numbers
  return(tst)
}



# END/TEST --------------------------------------


a<-rawTrans(costs[[7]]$resultados[[1]])


original<- costs[[2]]$param_user[[1]] %>% filter(param_user=="label") %>% select(param_user_val) %>% unlist(use.names = F)
modelled<- a$numbers
plot(original, modelled)

donde<-rawTrans(input=costs[[6]]$donde[[2]])
cuando<-rawTrans(input=costs[[6]]$cuanto[[2]])

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
