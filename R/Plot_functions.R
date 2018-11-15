################################'
#' 
#' Functions for Plotting the Model Statistics of ARTMO
#' 
#' 
################################'

# Explore the Outcome of a Model
explore.model.gg<-function(model,costs,stat.subset=NULL){
  
  md  <- costs %>% filter(Model==model)
  plt <- md$Statistics[[1]] %>% mutate(ID_stat=1:nrow(.))
  plt2<-plt %>% 
    gather(.,key=Statistic,value=value,me,rmse,relrmse,mae,r,r2,nrmse) %>% 
    separate(name_parameter,into=c("Name_short","Name"),sep=":") %>% 
    rename(algorithm=name_algoritmo) %>% 
    mutate(noise=as.factor(noise))
  
  if(!is.null(stat.subset)) {plt2<-plt2 %>% filter(Statistic==alg.subset)}
  
  g1<-ggplot(plt2,aes(ID_stat,value,fill=algorithm,alpha=noise))+
    geom_bar(stat="identity")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle(paste0(model,": Performance for ", plt2$Name))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  if(length(unique(plt2$Statistic))>1) {g1<-g1+ facet_wrap(Statistic~.,scales = "free")}
  return(g1)
  
}
