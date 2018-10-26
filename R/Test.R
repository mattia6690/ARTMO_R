


tab1<-get.simtable(path)
tab1.nest<-tab1 %>% unnest %>% group_by(Model) %>% nest


tab1.nest1<-tab1.nest %>% 
  mutate(GGs=map(data,function(x){
    
    g1<-ggplot(try,aes(Values))+ theme_bw()+
      geom_histogram(bins=10,color="black",fill="blue")+
      facet_grid(.~ParShort,scales = "free")
  }))

par(mfrow=c(1,2))

# GGplot from https://medium.com/optima-blog/using-polar-coordinates-for-better-visualization-1d337b6c9dec
colorb<- colorRampPalette(c("palegreen","palegreen4"))
cols  <- colorb(length(unique(tab1$Model)))
tab1$ParShort  <- factor(tab1$ParShort, levels = tab1$ParShort)
g1<-ggplot(tab1,aes(x=ParShort,y=Count,fill=Distribution))+ theme_light() +
  geom_histogram(stat="identity") +
  geom_point(aes(colour=Distribution))+
  geom_line()+
  ylab("Iteration Count")+ xlab("")+
  facet_grid(Model~.)+
  scale_color_manual(values=cols)+
  scale_fill_manual(values=cols)