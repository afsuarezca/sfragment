setwd("C:/Users/s2993062/documents/idiv/sFragment")

###Sample species by stratum#######

#load all data
load("3474_76_sPlotOpen.RData")

colnames(DT2.oa)
colnames(header.oa)
colnames(CWM_CWV.oa)

#Add size categories to all the dataset

ori_hearder.oa<-header.oa

header.oa<-subset(header.oa, !is.na(header.oa$Releve_area))

View(header.oa)

for(i in 1:nrow(header.oa)){
  if(header.oa[i,"Releve_area"] < 150){
    header.oa[i,"area_cat"]<-"S"
  }else{
    if(header.oa[i,"Releve_area"] >= 150 & header.oa[i,"Releve_area"] < 600){
      header.oa[i,"area_cat"]<-"MS"
  }else
    if(header.oa[i,"Releve_area"] >= 600 & header.oa[i,"Releve_area"] < 1200){
      header.oa[i,"area_cat"]<-"ML"
    }else{
      if(header.oa[i,"Releve_area"] >= 1200)
        header.oa[i,"area_cat"]<-"L"
    }
  }
}



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



#Filter strata with forest plots####

df_for<-df %>%
        filter(nplot >0) %>%
        filter(forest == TRUE)

df_for<-subset(df_for, df_for$biome != "Alpine")


View(df_for)

######################

dataplot<-header.oa %>%
          filter(area_cat == "S")%>%
          filter(Biome == "Tropics with year-round rain")%>%
          filter(Country %in% c("Colombia","Ecuador"))

#unique(dataplot$Country)

colnames(dataplot)

#get species in plots#

spdf<-DT2.oa %>% 
  filter(PlotObservationID %in% dataplot$PlotObservationID) %>%
  select(PlotObservationID,Species,Original_abundance)%>%
  filter(!duplicated(cbind(PlotObservationID,Species)))
  
head(spdf) 

plotdat<-dataplot[c("Longitude","Latitude","PlotObservationID","Releve_area")]

library(ggplot2)

#first get plots per stratum########
p<-list()
library(scales)

for(i in 1:nrow(df_for)){
  data<-header.oa %>% 
    filter(Biome == df_for[i,"biome"]) %>%
    filter(area_cat == df_for[i,"size"]) %>%
    #filter(Releve_area >= df_for[i,"size_min"]) %>%
    #filter(Releve_area < df_for[i,"size_max"]) %>%
    filter(Forest == df_for[i,"forest"])
  p[[i]]<-data %>%
    ggplot(aes(x=Releve_area, fill=area_cat)) +
    geom_histogram( color="#e9ecef", alpha=1, position = 'identity') +
    scale_fill_manual(values=c("#7fc97f", "#beaed4","#fdc086","#ffff99")) +
    theme_bw()+
    labs(fill="",
         x = "Plot area",
         y = "Number of plots")+
    ggtitle(df_for[i,"biome"][[1]])+
    scale_x_continuous(labels = label_scientific())
}

library(patchwork)


(p[[2]] + p[[3]]) / (p[[4]] + p[[5]])
(p[[6]] + p[[7]]) / (p[[8]] + p[[9]])
(p[[10]] + p[[11]]) / (p[[12]] + p[[13]])
(p[[14]] + p[[15]]) / (p[[16]] + p[[17]])
(p[[18]] + p[[19]]) / (p[[20]] + p[[21]])
(p[[22]] + p[[23]]) / (p[[24]] + p[[25]])
(p[[26]] + p[[27]]) / (p[[28]] + p[[29]])
(p[[30]] + p[[31]]) 

head(test)

##Filter outliers######

p<-list()

for(i in 1:nrow(df_for)){
  data<-header.oa %>% 
    filter(Biome == df_for[i,"biome"]) %>%
    filter(area_cat == df_for[i,"size"]) %>%
    #filter(Releve_area >= df_for[i,"size_min"]) %>%
    #filter(Releve_area < df_for[i,"size_max"]) %>%
    filter(Forest == df_for[i,"forest"])
  
  # Calculate mean and standard deviation
  mean_data <- mean(data$Releve_area)
  sd_data <- sd(data$Releve_area)
  
  # Define threshold (e.g., 2 times the standard deviation)
  threshold <- 2 * sd_data
  
  # Identify outliers
  data[abs(data$Releve_area - mean_data) > threshold,"outlier"]<-"yes"
  data[abs(data$Releve_area - mean_data) < threshold,"outlier"]<-"no"
  # Filter data without outliers
  data<-data %>% filter(outlier == "no")
  
  p[[i]]<-data %>%
    ggplot(aes(x=Releve_area, fill=area_cat)) +
    geom_histogram( color="#e9ecef", alpha=1, position = 'identity') +
    scale_fill_manual(values=c("#7fc97f", "#beaed4","#fdc086","#ffff99")) +
    theme_bw()+
    labs(fill="",
         x = "Plot area",
         y = "Number of plots")+
    ggtitle(df_for[i,"biome"][[1]])+
    scale_x_continuous(labels = label_scientific())
}



#get countries with more data####
unique(test$Country)
selc<-plyr::count(test$Country)
selc<-selc %>% filter(freq > 50)

test<-test %>% filter(Country %in% selc$x)

write.csv(test[1:12],"trop_smallplot_test.csv")


#get species 

testsp<-DT2.oa
testsp$presence<-1
head(testsp)

#get species in plots####

spdf<-testsp %>% 
      filter(PlotObservationID %in% test$PlotObservationID) %>%
      select(PlotObservationID,Species,
             presence)%>%
      filter(!duplicated(cbind(PlotObservationID,Species)))

  
head(spdf)

#Discard plots with incomplete taxonomic identification?


spdf<-merge(spdf,test[c("PlotObservationID","Country")],all.x = T)
head(spdf)
unique(spdf$Country)


spdf<-split(spdf,spdf$Country)

dfl<-list()
for (i in 1:length(spdf)) {
  df2<-spdf[[i]][1:3]
  dat<- df2 %>% spread(PlotObservationID,presence)
  dat<-as.data.frame(dat)
  dat[is.na(dat)]<-0
  row.names(dat)<-dat$Species
  dat<-dat[2:length(dat)]
  dfl[[i]]<-dat
}

names(dfl)

library(tidyverse)

#sample-based rarefaction and extrapolation######
library(iNEXT)

testmod_med<-iNEXT(dfl, q=0, datatype="incidence_raw",se=TRUE, conf=0.95,
      nboot=10)

ggiNEXT(testmod, type=1, se=TRUE, facet.var="None", color.var="Assemblage", grey=FALSE)


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
library(sf)
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







