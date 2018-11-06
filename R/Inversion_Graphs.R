
# Initialize --------------------------------------------------------------
source("R/Essentials.R")

# Read Data ---------------------------------------------------------------
ARTMOdir<-"K:/SentinelVegetationProducts/S2_LAI"
ARTMOdir.ss<-paste0(ARTMOdir,"/Subsets")
ARTMOdir.in<-paste0(ARTMOdir,"/ARTMO_Inversion")

iteration<-"2509_Prosail_Inversion_30000_10Bands_v1"
indir<-paste0(ARTMOdir.in,"/",iteration,"/")

gpdata.file<-paste0(ARTMOdir,"/Validation_Tables/Metrics_S2_ByBand_LAI_190918.rds")
gpsdata<- gpdata.file %>% 
  readRDS %>% 
  st_as_sf %>% 
  dplyr::select(Station,Date,OP1,meanLAI)

files.out<-list.files(indir,full.names = T) %>% 
  as.tibble %>% 
  setNames("File") %>% 
  filter(!grepl("\\.hdr",File)) %>%
  mutate(Raster=map(File,brick)) %>% 
  mutate(Statdat=map(File,function(x) str_split(basename(x),"_|\\.")[[1]])) %>% 
  mutate(Station=map_chr(Statdat, function(x) x[1])) %>% 
  mutate(Date=as_date(map_chr(Statdat, function(x) x[2]))) %>% 
  mutate(Poly=map2(Station,Date,function(x,y,gps=gpsdata) filter(gps,Station==x & Date==y)))

files<-files.out %>% 
  dplyr::select(Station,Date,Raster,Poly) %>% 
  filter(Date!="2017-05-10")

files.extract<-files %>% 
  mutate(Simulated=map2(Raster,Poly,function(x,y) raster::extract(x[[1]],as_Spatial(y)))) %>% 
  mutate(Measured=map(Poly,function(x) x$meanLAI))

files.unnest <- files.extract %>% dplyr::select(Station,Date,Simulated,Measured) %>% unnest

saveRDS(files.unnest,file = paste0(ARTMOdir.in,"/",iteration,".rds"))


eq1<-files.unnest %>% 
  group_by(Station) %>% nest %>% 
  mutate(Lm=map(data,function(l) lm(l$Simulated~l$Measured))) %>% 
  mutate(Eq=map_chr(Lm,r2.equation)) %>% 
  dplyr::select(Station,Eq)

g1<-ggplot(files.unnest,aes(Simulated,Measured,color=as.character(Date)))+ theme_bw()+
  geom_point()+
  geom_smooth(method="lm",se=F,col="brown")+
  geom_abline(intercept=0,slope=1,linetype="dotted")+
  scale_color_discrete(name = "Date")+
  facet_wrap(.~Station,ncol=2)+
  xlab("Simulated LAI")+ylab("Measured LAI")+
  ylim(c(0,10))+
  xlim(c(0,10))

g1<-g1+geom_text(data=eq1,aes(x=3,y=10,label=Eq,family="serif"),color="black",parse=T)
ggsave(g1,file=paste0(ARTMOdir,"/R_Graphs/",iteration,".png"),device = "png",width=10,height=7)

# Plot and Click ----------------------------------------------------------

ras<-files.out$Raster[[1]][[1]]
poly<-files.out$Poly[[1]]
plot(ras)

ex.p<-poly %>% st_buffer(dist = 50)
plot(ras,ext=ex.p)
plot(st_geometry(poly),add=T)



env.setup<-function(dir){
  
  dircheckup(paste0(dir,"/ARTMO_Inversion"))
  dircheckup(paste0(dir,"/ValidationTables"))
  dircheckup(paste0(dir,"/Subsets"))
  dircheckup(paste0(dir,"/Other"))
  
}

brick("K:/SentinelVegetationProducts/S2_20m_Export_NoSNAP/T32TPS/S2B_MSI_L2A_20m_20171205_N02.06_s2cV2.4_T32TPS_20171205T121008_AuxBands.tif")

