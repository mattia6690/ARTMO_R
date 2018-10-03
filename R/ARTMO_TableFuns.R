

# Read the parametrization file


ARTMOdir<-"K:/SentinelVegetationProducts/S2_LAI"
Parfile<-paste0(ARTMOdir,"/Other/test3.mat")
getA.Para<-function(file){
  
  param1<-readMat(file)[[1]][,,1]
  n.para<-param1$param.ind[[1]]
  l1.para<-param1$parametros %>% unlist %>% matrix(nrow=3,byrow=F)
  l2.para<-param1$gparametros %>% unlist %>% matrix(nrow=3,byrow=F)
  noise.para <-noise.para %>% rep(3) %>% matrix(nrow=3,byrow=T)
  r<-cbind(n.para,l1.para,l2.para,noise.para)
  return(r)

}

getA.Para((Parfile))

Cffile<-paste0(ARTMOdir,"/Other/CF_test.txt")

getA.CFtable<-function(file){
  
  rl<-readLines(file) 
  
  cls <- rl[2] %>% str_split(.,":",simplify = T) %>% .[1,2] %>% str_replace(.," ","")
  var <- rl[3] %>% str_split(.,":",simplify = T) %>% .[1,2] %>% str_replace(.," ","")
  
  stat<- rl[4:length(rl)]
  rl<-stat %>%  map(function(x)str_split(x,"\t")[[1]])
  rl2<-do.call(rbind,rl)
  colnames(rl2)<-rl2[1,]
  rl2<-rl2 %>% as.tibble %>% slice(-1)
  return(rl2)
  
}

