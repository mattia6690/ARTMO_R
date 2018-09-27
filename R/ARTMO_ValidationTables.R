# Script for generating the Validation Table necessary for the ARTMO LUT Inversion 

# Initialize --------------------------------------------------------------
source("R/Essentials.R")

# Input -------------------------------------------------------------------
outdir <- "K:/SentinelVegetationProducts/S2_LAI"
suffix <- "270918"

wave  <-read.csv(paste0(outdir,"/Other/Sentinel2_bands.csv"))
wave.c<- wave %>% filter(Resolution!=60) %>% dplyr::select(Central) %>% c(0,.) %>% unlist(use.names = F)

import<-readRDS(paste0(outdir,"/R_Objects/ExtractionList_",suffix,".rds"))

# ARTMO validation table ----------------------------------------------------
artmo1<-import %>% 
  as.tibble %>% 
  dplyr::select(.,Station,Date,OP1,meanLAI,contains("Band_")) %>% 
  group_by(Station,Date,OP1) %>% 
  nest

artmo2<- map(artmo1$data,function(x){x %>% unlist %>% as.numeric() %>% t %>% t})
artmo3<- do.call(cbind,artmo2)
artmo4<- cbind(wave.c,artmo3)

write.table(artmo4,file = paste0(outdir,"/Validation_Tables/Metrics_S2_Artmo_10Bands_",suffix,".csv"),col.names = FALSE,row.names = F,sep=",")

