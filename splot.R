setwd("C:/Users/s2993062/documents/idiv/sFragment")
splot<-read.csv("sPlotOpen_header.csv")

#length(unique(splot_f$PlotObservationID))
str(splot)

splot_f<-subset(splot,!is.na(splot$Forest))
splot_f<-subset(splot,splot$Forest != FALSE)
str(splot_f)
unique(splot_f$Plant_recorded)

length(unique(splot_f$GIVD_ID))
View(plyr::count(splot_f$GIVD_ID))

test<-subset(splot_f, splot_f$GIVD_ID == "AU-AU-002")

test<-subset(splot_f, splot_f$Country == "Brazil")

unique(splot_f$GIVD_ID)
plyr::count(splot_f$Country)
colnames(test)

colnames(splot_f)

test[c(1:14)]
View(test)

write.csv(test[c(1:14)],"Brazil_splot.csv")

getwd()

#sort by sampling date and releve_area


##########################################

lc<-raster("aus_for18_geotiff/aus_for18_geotiff/aus_for18.tif")
lc<-raster("MODIS/MODIS_2012_test.tif")
lc

#attributes

lc_at<-lc$STATE@data@attributes[[1]]

head(lc_at)
df<-data.frame(unique(lc_at[c("FOR_TYPE","FOR_CODE")]))
View(df)


View(lc_at)


###Sample species by stratum#######

#load all data
load("3474_76_sPlotOpen.RData")

colnames(DT2.oa)
colnames(header.oa)
colnames(CWM_CWV.oa)


#Discard plots with incomplete taxonomic identification?####
#filter out compositional outliers?####


#strata#####
library(dplyr)

df<-data.frame(size_min = c(0,150,600,1200),
           size_max = c(150,600,1200, 1e6))
df$size<-c("S","MS",
           "ML","L")

biome<-rep(unique(header.oa$Biome),each = nrow(df))

df<-cbind(df,biome)


forest<-unique(header.oa$Forest)
forest<-subset(forest,!is.na(forest))

df<-do.call("rbind", replicate(length(forest), df, simplify = FALSE))

df$forest<-rep(forest,each = nrow(df)/length(forest))

head(df)
head(header.oa)

df$nplot<-NA

for (i in 1:nrow(df)) {
    test<-header.oa %>% 
          filter(Biome == df[i,"biome"]) %>%
          filter(Releve_area >= df[i,"size_min"]) %>%
          filter(Releve_area < df[i,"size_max"]) %>%
          filter(Forest == df[i,"forest"])
    df[i,"nplot"]<-nrow(test)
    print(i)
}

View(df)

#Filter strata with forest plots####

df_for<-df %>%
        filter(nplot >0) %>%
        filter(forest == TRUE)

head(df_for)

#sample-based rarefaction and extrapolation######
library(iNEXT)

#first get plots per stratum
test<-header.oa %>% 
  filter(Biome == df_for[i,"biome"]) %>%
  filter(Releve_area >= df_for[i,"size_min"]) %>%
  filter(Releve_area < df_for[i,"size_max"]) %>%
  filter(Forest == df_for[i,"forest"])
head(test)

#get species 

spdf<-DT2.oa %>% 
      filter(PlotObservationID %in% test$PlotObservationID) %>%
      select(PlotObservationID,Species,
             Relative_cover)%>%
      filter(!duplicated(cbind(PlotObservationID,Species)))
  
head(spdf)

library(tidyverse)
#data format

dat<- spdf %>% spread(PlotObservationID,Relative_cover)


iNEXT(x, q=0, datatype="abundance", size=NULL, endpoint=NULL, knots=40, se=TRUE, conf=0.95,
      nboot=50)


####################

check<- subset(metadata.oa,metadata.oa$PlotObservationID == 56226)
View(check)

#unique plots:

splot<-header.oa[!duplicated(header.oa[c("Latitude","Longitude","Releve_area")]),]
dupplots<-header.oa[duplicated(header.oa[c("Latitude","Longitude","Releve_area")]),]

head(plots)

#length(unique(splot_f$PlotObservationID))


splot_f<-subset(splot,!is.na(splot$Forest))
splot_f<-subset(splot,splot$Forest != FALSE)
str(splot_f)
unique(splot_f$Plant_recorded)

length(unique(splot_f$GIVD_ID))
unique(splot_f$Continent)
View(plyr::count(splot_f$GIVD_ID))

str(splot_f)

test<-subset(splot_f, splot_f$GIVD_ID == "AU-AU-002")
test<-subset(splot_f, splot_f$Country == "Brazil")
unique(test$Continent)
test<-subset(splot_f, splot_f$Continent == "Africa")

unique(splot_f$GIVD_ID)
plyr::count(splot_f$Country)
plyr::count(test$Country)
colnames(test)

colnames(splot_f)

#test[c(1:14,45:47)]
View(test)

write.csv(test,"Africa_splot.csv")

getwd()


##################

plots<-st_read("./splot_points/AUS_test_prj.shp")
head(plots)
#####################
library(landscapemetrics)
library(fasterize)

metrics_units<-list()
#aplicar un loop para cada celda donde se calculan las m?tricas
for(i in 1:nrow(plots)){
    print(i)
    fplot<- plots[i,]
    fplot<-st_buffer(fplot,500)
    #generar raster cortando raster original al extent de la celda
    raster.polygon<-crop(lc,fplot,snap = "near")
    #cortar raster nuevo al borde de la celda (fasterize es m?s r?pido). rectifica que est?n como sf objects
    raster.polygon2<- fasterize(fplot,raster.polygon)
    raster.polygon<-mask(raster.polygon,raster.polygon2)
    #plot(raster.polygon)
    #calculate landscape metrics 
    #https://cran.r-project.org/web/packages/landscapemetrics/landscapemetrics.pdf
    #https://www.umass.edu/landeco/research/fragstats/documents/Metrics/Metrics%20TOC.htm
    metrics_list<-calculate_lsm(raster.polygon, what = c("lsm_c_np", 
                                                         "lsm_c_ca", 
                                                         "lsm_c_ed",
                                                         #"lsm_c_clumpy")),
                                                         "lsm_c_pafrac",
                                                         "lsm_c_pland"))
    metrics_list<-tidyr::spread(metrics_list,metric,value)
    #asignar nombres
    metrics_list$PlotObserv<-plots[i,]$PlotObserv
    metrics_list$GIVD_ID<-plots[i,]$GIVD_ID
    metrics_units[[i]]<-metrics_list
  print(i)
}

lc_metrics<-do.call(rbind,metrics_units)

##############

lc_metrics<- merge(lc_metrics,CWM_CWV.oa[1:30],by.x ="PlotObserv",by.y = "PlotObservationID",all.x = T)
lc_metrics<-subset(lc_metrics,lc_metrics$class == 3)







