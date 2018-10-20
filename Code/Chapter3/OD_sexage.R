# library(readxl)
# library(sf)
# library(ukboundaries)
# library(raster)
# library(ggplot2)
# library(dplyr)
# library(ggpubr)
################################ OD Sex ############################################
# Load and filter OD file 1 for sex
ODsex = read.csv('Datasets/OD/wu01ew_v2.csv', skip = 0)
ODsex = ODsex %>% 
  filter(Area.of.residence %in% AgeEcoOccupSex$X.1) %>% 
  rename('Residence' = Area.of.residence, 'Workplace' = Area.of.workplace, 
         'Total' = All.categories..Sex)
str(ODsex)
summary(ODsex)
sum(ODsex$Total) == sum(ODsex$Male) + sum(ODsex$Female)
sum(ODsex$Total)
ODsex$ResWork = paste0(ODsex$Residence, ODsex$Workplace)
ODsex$Residence=as.character(ODsex$Residence)
ODsex$Workplace=as.character(ODsex$Workplace)

(unique(ODsex$Workplace))
write.csv(ODsex, 'Datasets/OD/ODsex.csv', row.names=F)

############################# OD Age ###############################################
# Load and filter OD file 2 for age
OD_AGE = read.csv('Datasets/OD/wu02ew_v2.csv', skip = 0)
OD_AGE = OD_AGE %>% 
  filter(Area.of.residence %in% CommuteAgeSex$Zone)
sum(OD_AGE$All.categories..Age.16.and.over)
OD_AGE$ResWork = paste0(OD_AGE$Area.of.residence, OD_AGE$Area.of.workplace)


ODage_new = OD_AGE %>%
  select('Residence' = Area.of.residence, 'Workplace' = Area.of.workplace, 'ResWork' = ResWork,
         'Total' = All.categories..Age.16.and.over, '16-24' = X16.24, '25-34' = X25.34, 
         '35-49' = X35.49, '50-64' = X50.64, '65-74' = X65.74, 'Over75' = X75.)



ODage_new$Residence = as.character(ODage_new$Residence)
ODage_new$Workplace = as.character(ODage_new$Workplace)
ODage_new$ResWork = as.character(ODage_new$ResWork)



# Expand each OD pair to each individual
ODage_ints = ODage_new[rep(row.names(ODage_new), ODage_new$Total), ]
ODage_ints$id = seq(1:nrow(ODage_ints))
zz=unique(ODage_new$Residence)


# Do it with lists instead of dataframes
ODage_ints_new_corrected=list() 

for (i in 401:692){
  
  x = ODage_ints[ODage_ints$Residence==zz[i],]
  ODage_ints_new = list()
  yy = unique(x$ResWork)
  print(i)
  
  for (j in 1:length(yy)){
    y = x[x$ResWork==yy[j],]
    
    y1=list()
    y2=list()
    y3=list()
    y4=list()
    y5=list()
    y6=list()
    
    y_id=list()
    
    if (y$`16-24`[1]>0){
      y1 = sample_n(y, y$`16-24`[1])
      y1$`16-24`= 1
      y1$`25-34` = 0
      y1$`35-49` = 0
      y1$`50-64` = 0
      y1$`65-74` = 0
      y1$`Over75` = 0
    }
    y_id = rbind(y_id, y1)
    
    if (y$`25-34`[1]>0){
      y2 = y[!(y$id %in% y_id$id),]
      y2 = sample_n(y2, y2$`25-34`[1])
      y2$`16-24`= 0
      y2$`25-34` = 1
      y2$`35-49` = 0
      y2$`50-64` = 0
      y2$`65-74` = 0
      y2$`Over75` = 0
    }
    y_id = rbind(y_id, y2)
    
    
    if (y$`35-49`[1]>0){
      y3 = y[!(y$id %in% y_id$id),]
      y3 = sample_n(y3, y3$`35-49`[1])
      y3$`16-24`= 0
      y3$`25-34` = 0
      y3$`35-49` = 1
      y3$`50-64` = 0
      y3$`65-74` = 0
      y3$`Over75` = 0
    }
    y_id = rbind(y_id, y3)
    
    if (y$`50-64`[1]>0){
      y4 = y[!(y$id %in% y_id$id),]
      y4 = sample_n(y4, y4$`50-64`[1])
      y4$`16-24`= 0
      y4$`25-34` = 0
      y4$`35-49` = 0
      y4$`50-64` = 1
      y4$`65-74` = 0
      y4$`Over75` = 0
    }
    y_id = rbind(y_id, y4)
    
    if (y$`65-74`[1]>0){
      y5 = y[!(y$id %in% y_id$id),]
      y5 = sample_n(y5, y5$`65-74`[1])
      y5$`16-24`= 0
      y5$`25-34` = 0
      y5$`35-49` = 0
      y5$`50-64` = 0
      y5$`65-74` = 1
      y5$`Over75` = 0
    }
    y_id = rbind(y_id, y5)
    
    if (y$`Over75`[1]>0){
      y6 = y[!(y$id %in% y_id$id),]
      y6 = sample_n(y6, y6$`Over75`[1])
      y6$`16-24`= 0
      y6$`25-34` = 0
      y6$`35-49` = 0
      y6$`50-64` = 0
      y6$`65-74` = 0
      y6$`Over75` = 1
    }
    y_id = rbind(y_id, y6)
    ODage_ints_new = rbind(ODage_ints_new, y_id)
    
  }
  
  y_age=list()
  y35_44=list()
  y45_54=list()
  y55_64=list()
  y35_49=list()
  y50_64=list()
  ODage_ints_new$`35-44`=0
  ODage_ints_new$`45-54`=0
  ODage_ints_new$`55-64`=0
  ODage_new_bins = ODage_ints_new[which(ODage_ints_new$`35-49`>0 | ODage_ints_new$`50-64`>0),]
  ODage_noncommon = ODage_ints_new[!(ODage_ints_new$id %in% ODage_new_bins$id), ]
  
  
  
  y35_49 = ODage_new_bins[ODage_new_bins$`35-49`>0,]
  y35_44 = sample_n(y35_49, (AgeEcoOccupSex$`35-39`[i]+AgeEcoOccupSex$`40-44`[i]))
  y35_44$`35-44`=1
  
  y_age=rbind(y_age, y35_44)
  
  y50_64 = ODage_new_bins[ODage_new_bins$`50-64`>0,]
  y55_64 = sample_n(y50_64, (AgeEcoOccupSex$`55-59`[i]+AgeEcoOccupSex$`60-64`[i]))
  y55_64$`55-64`=1
  
  y_age=rbind(y_age, y55_64)
  
  y45_54 = ODage_new_bins[!(ODage_new_bins$id %in% y_age$id),]
  y45_54 = sample_n(y45_54, (AgeEcoOccupSex$`45-49`[i]+AgeEcoOccupSex$`50-54`[i]))
  y45_54$`45-54`=1
  y_age=rbind(y_age, y45_54)
  
  
  
  ODage_ints_new_corrected = rbind(ODage_ints_new_corrected,ODage_noncommon, y_age)
}

#  Check that each id is unique
ODage_ints_new_corrected %>% 
  count(n_distinct(id))


ODage_new_agg = ODage_ints_new_corrected %>% 
  group_by(ResWork, Workplace, Residence) %>% 
  summarise(Total=n(), `16-24` = sum(`16-24`==1), `25-34` = sum(`25-34`==1), `35-44` = sum(`35-44`==1), 
            `45-54` = sum(`45-54`==1), `55-64` = sum(`55-64`==1), `65-74` = sum(`65-74`==1), `Over75` = sum(`Over75`==1))


ODage_new_agg_zonal = ODage_new_agg %>% 
  group_by(Residence) %>% 
  summarise(Total=sum(Total), `16-24` = sum(`16-24`), `25-34` = sum(`25-34`), `35-44` = sum(`35-44`), 
            `45-54` = sum(`45-54`), `55-64` = sum(`55-64`), `65-74` = sum(`65-74`), `Over75` = sum(`Over75`))


identical(AgeEcoOccupSex$`16-24`, ODage_new_agg_zonal$`16-24`) #All true
identical(AgeEcoOccupSex$`25-34`, ODage_new_agg_zonal$`25-34`)
identical(AgeEcoOccupSex$`35-44`, ODage_new_agg_zonal$`35-44`)
identical(AgeEcoOccupSex$`45-54`, ODage_new_agg_zonal$`45-54`)
identical(AgeEcoOccupSex$`55-64`, ODage_new_agg_zonal$`55-64`)
identical(AgeEcoOccupSex$`65-74`, ODage_new_agg_zonal$`65-74`)
identical(AgeEcoOccupSex$Over75, ODage_new_agg_zonal$Over75)

str(ODage_new_agg)

ODage_new_agg = data.frame(ODage_new_agg)
str(ODage_new_agg)

# check that totals are the same
identical(ODage_new$Total, ODage_new_agg$Total) # True

write.csv(ODage_ints_new_corrected, 'Datasets/OD/ODage_ints_new_corrected.csv', row.names=F)

write.csv(ODage_new_agg, 'Datasets/OD/ODage_new_agg.csv', row.names=F)

######################## Combine OD Age-Sex #################################
ODsex = read.csv('Datasets/OD/ODsex.csv')

OD_sexage = ODsex
OD_sexage$Total_age=ODage_new_agg$Total
OD_sexage$`16-24`=ODage_new_agg$X16.24
OD_sexage$`25-34`=ODage_new_agg$X25.34
OD_sexage$`35-44`=ODage_new_agg$X35.44
OD_sexage$`45-54`=ODage_new_agg$X45.54
OD_sexage$`55-64`=ODage_new_agg$X55.64
OD_sexage$`65-74`=ODage_new_agg$X65.74
OD_sexage$Over75=ODage_new_agg$Over75

OD_sexage$diff =  OD_sexage$Total_age - OD_sexage$Total # Difference is 0
OD_sexage$abs_diff = abs(OD_sexage$diff) # Difference is 0

write.csv(OD_sexage, 'Datasets/OD/OD_SexAge.csv', row.names=F)

rm(OD_AGE)
rm(ODage)
rm(ODsex)
rm(x)
rm(y)
rm(y1)
rm(y2)
rm(y3)
rm(y4)
rm(y5)
rm(y6)
rm(y_age)
rm(y_id)
rm(y35_44)
rm(y35_49)
rm(y45_54)
rm(y50_64)
rm(y55_64)
rm(ODage_ints_new)
