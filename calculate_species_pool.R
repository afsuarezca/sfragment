source("C:/Users/s2993062/documents/idiv/sFragment/speciesPool/SpeciesPool.R", echo=TRUE)
source("C:/Users/s2993062/documents/idiv/sFragment/speciesPool/speciesPoolFunctions.R", echo=TRUE)

pack<-c("Matrix",
        "dismo",
        "tidyr",
        "reshape2",
        "SpadeR",
        "sp",
        "vegan",
        "parallel",
        #doParallel,
        "foreach",
        "rgeos")

sapply(pack, require, character.only = TRUE)


dataplot<-header.oa %>%
  filter(area_cat == "S")%>%
  filter(Biome == "Tropics with year-round rain")%>%
  filter(Country %in% c("Colombia","Ecuador")) %>%
  filter(!is.na(Longitude))

#unique(dataplot$Country)

colnames(dataplot)

#get species in plots#

spdf<-DT2.oa %>% 
  filter(PlotObservationID %in% dataplot$PlotObservationID) %>%
  select(PlotObservationID,Species,Original_abundance)%>%
  filter(!duplicated(cbind(PlotObservationID,Species)))

spdf<-data.frame(spdf)
spdf<-spdf[complete.cases(spdf),]


head(spdf) 

plotdat<-dataplot[c("Longitude","Latitude","PlotObservationID","Releve_area")]
plotdat<-as.data.frame(plotdat)

#convert to spatialpointdataframe

library(sp)
coordinates(plotdat) <- plotdat[1:2]
proj4string(plotdat) <- CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs")

plotdat<-plotdat[c("PlotObservationID", "Releve_area")]

model_spool<-SpeciesPool(
  spdf,
  plotdat,
  Mij = NULL,
  ncores = 1,
  rows = NULL,
  t.radius = 20000,
  t.bray = 0.2,
  t.plot.number = 10L,
  cutoff = c("iChao2", "Gompertz", "Michaelis"),
  verbose = T,
  species.list = T,
  mycrs = NULL,
  lonlat = T
)
timestamp()

plotest<- subset(model_spool,!is.na(model_spool$chao))
View(plotest)

plotdat2<-subset(plotdat,plotdat$PlotObservationID %in% plotest$RELEVE_NR)
proj4string(plotdat2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs")

plotdat2

library(sf)

st_write(sf::st_as_sf(plotdat2),"splot_points/testplots_spool.shp")
