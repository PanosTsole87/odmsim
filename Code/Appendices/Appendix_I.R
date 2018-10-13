library(tmap)
library(tmaptools)
library(ggplot2)
library(dplyr)
library(sf)
#tmap_mode("view")


source('Code/Chapter3/DataPreparation_Census.R')
source('Code/Chapter3/DataPreparation_Boundaries.R')
source('Code/Chapter3/HoursWorked_Occupation.R')

############### Map Initial percentages ########################################################
Initial_percentages = AgeEcoOccupSex[,c(1,41,42,43,47:51,44)]
Initial_percentages$`Over75` = AgeEcoOccupSex$Over16-AgeEcoOccupSex$`16-74`
names(Initial_percentages)[1]<-"Origin"


Initial_percentages$Male_perc = Initial_percentages$MaleOver16/Initial_percentages$Over16
Initial_percentages$Female_perc = Initial_percentages$FemaleOver16/Initial_percentages$Over16
Initial_percentages$`16-24_perc` = Initial_percentages$`16-24`/Initial_percentages$Over16
Initial_percentages$`25-34_perc` = Initial_percentages$`25-34`/Initial_percentages$Over16
Initial_percentages$`35-44_perc` = Initial_percentages$`35-44`/Initial_percentages$Over16
Initial_percentages$`45-54_perc` = Initial_percentages$`45-54`/Initial_percentages$Over16
Initial_percentages$`55-64_perc` = Initial_percentages$`55-64`/Initial_percentages$Over16
Initial_percentages$`65-74_perc` = Initial_percentages$`65-74`/Initial_percentages$Over16
Initial_percentages$`Over75_perc` = Initial_percentages$Over75/Initial_percentages$Over16
# Join with spatial data to map the percentages
Initial_spatial = left_join(Yorkshire1, Initial_percentages, by=c('AREA_ID'='Origin'))
#str(Initial_spatial)

names(Initial_spatial)[17]<-"Age.16_24_perc"
names(Initial_spatial)[18]<-"Age.25_34_perc"
names(Initial_spatial)[19]<-"Age.35_44_perc"
names(Initial_spatial)[20]<-"Age.45_54_perc"
names(Initial_spatial)[21]<-"Age.55_64_perc"
names(Initial_spatial)[22]<-"Age.65_74_perc"
names(Initial_spatial)[23]<-"Age.Over75_perc"

# Add occupation
Initial_spatial = left_join(Initial_spatial, Occupation, by=c('AREA_ID'='Code'))

Initial_spatial$`Occ1_perc` = Initial_spatial$Occ1/Initial_spatial$Over16
Initial_spatial$`Occ2_perc` = Initial_spatial$Occ2/Initial_spatial$Over16
Initial_spatial$`Occ3_perc` = Initial_spatial$Occ3/Initial_spatial$Over16
Initial_spatial$`Occ4_perc` = Initial_spatial$Occ4/Initial_spatial$Over16
Initial_spatial$`Occ5_perc` = Initial_spatial$Occ5/Initial_spatial$Over16
Initial_spatial$`Occ6_perc` = Initial_spatial$Occ6/Initial_spatial$Over16
Initial_spatial$`Occ7_perc` = Initial_spatial$Occ7/Initial_spatial$Over16
Initial_spatial$`Occ8_perc` = Initial_spatial$Occ8/Initial_spatial$Over16
Initial_spatial$`Occ9_perc` = Initial_spatial$Occ9/Initial_spatial$Over16

#################################################################################
##### Map Gender

map_Male=tm_shape(Initial_spatial)+
  tm_fill('Male_perc', palette = "Blues", title = 'Male percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
map_Male

map_Female=tm_shape(Initial_spatial)+
  tm_fill('Female_perc', palette = "Blues", title = 'Female percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
map_Female

#############################################################################
##### Map Age bands

map_16_24=tm_shape(Initial_spatial)+
  tm_fill('Age.16_24_perc', palette = "Blues", title = 'Age 16-24 percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
map_16_24

map_25_34=tm_shape(Initial_spatial)+
  tm_fill('Age.25_34_perc', palette = "Blues", title = 'Age 25-34 percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
map_25_34

map_35_44=tm_shape(Initial_spatial)+
  tm_fill('Age.35_44_perc', palette = "Blues", title = 'Age 35-44 percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
map_35_44

map_45_54=tm_shape(Initial_spatial)+
  tm_fill('Age.45_54_perc', palette = "Blues", title = 'Age 45-54 percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
map_45_54

map_55_64=tm_shape(Initial_spatial)+
  tm_fill('Age.55_64_perc', palette = "Blues", title = 'Age 55-64 percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
map_55_64

map_65_74=tm_shape(Initial_spatial)+
  tm_fill('Age.65_74_perc', palette = "Blues", title = 'Age 65-74 percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
map_65_74

map_Over75=tm_shape(Initial_spatial)+
  tm_fill('Age.Over75_perc', palette = "Blues", title = 'Age Over 75 percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
map_Over75

###################################################################################
##### Mapping Occupation
map_Occ1=tm_shape(Initial_spatial)+
  tm_fill('Occ1_perc', palette = "Blues", title = 'Occupation 1 percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
map_Occ1

map_Occ2=tm_shape(Initial_spatial)+
  tm_fill('Occ2_perc', palette = "Blues", title = 'Occupation 2 percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
map_Occ2

map_Occ3=tm_shape(Initial_spatial)+
  tm_fill('Occ3_perc', palette = "Blues", title = 'Occupation 3 percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
map_Occ3

map_Occ4=tm_shape(Initial_spatial)+
  tm_fill('Occ4_perc', palette = "Blues", title = 'Occupation 4 percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
map_Occ4

map_Occ5=tm_shape(Initial_spatial)+
  tm_fill('Occ5_perc', palette = "Blues", title = 'Occupation 5 percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
map_Occ5

map_Occ6=tm_shape(Initial_spatial)+
  tm_fill('Occ6_perc', palette = "Blues", title = 'Occupation 6 percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
map_Occ6

map_Occ7=tm_shape(Initial_spatial)+
  tm_fill('Occ7_perc', palette = "Blues", title = 'Occupation 7 percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
map_Occ7

map_Occ8=tm_shape(Initial_spatial)+
  tm_fill('Occ8_perc', palette = "Blues", title = 'Occupation 8 percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
map_Occ8

map_Occ9=tm_shape(Initial_spatial)+
  tm_fill('Occ9_perc', palette = "Blues", title = 'Occupation 9 percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
map_Occ9