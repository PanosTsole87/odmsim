# library(ggplot2)
# library(dplyr)
# library(sf)
# library(ggpubr)
# library(tmap)
# library(tmaptools)


source('Code/Chapter3/DataPreparation_Boundaries.R')


OD_SexAge_Mode = read.csv('Datasets/OD/OD_SexAge_Mode.csv')
#source('Code/Chapter3/DataPreparation.R')

################## Destinations ####################################################
#class( OD_SexAge_Mode$Workplace)
OD_SexAge_Mode$Workplace=as.character( OD_SexAge_Mode$Workplace)

Destinations_msoa = UK_boundaries[UK_boundaries$AREA_ID %in% OD_SexAge_Mode$Workplace, ]
Destinations_msoa1 = Destinations_msoa[,-5] # delete the Local Authority column
cols=colnames(Destinations_msoa1) # Save the colnames of Destinations_msoa1 df to pass it later to another df of 4 columns and bind them

Destinations_regions = UK_boundaries_regions[UK_boundaries_regions$FIRST_CODE %in% OD_SexAge_Mode$Workplace, ]
colnames(Destinations_regions)=cols # Pass the colnames of the Destinations_msoa1 df to the Destinations_regions df. Both of them have 4 columns

Destinations=rbind(Destinations_msoa1, Destinations_regions)

#class(Destinations$AREA_NAME)
Destinations$AREA_NAME=as.character(Destinations$AREA_NAME)
Destinations$name = substr(Destinations$AREA_NAME,1,nchar(Destinations$AREA_NAME)-4)


Destinations_sexage_mode = OD_SexAge_Mode %>% 
  group_by(Workplace) %>% 
  summarise(Total=sum(Total), Male = sum(Male), Female = sum(Female), `16-24` = sum(`X16.24`), `25-34` = sum(`X25.34`), 
            `35-44` = sum(`X35.44`),`45-54` = sum(`X45.54`), `55-64` = sum(`X55.64`), `65-74` = sum(`X65.74`), `Over75` = sum(Over75), 
            WorkFromHome = sum(WorkFromHome), MetroTram = sum(MetroTram), Train=sum(Train), Bus=sum(Bus), 
            Taxi=sum(Taxi), Motorcycle = sum(Motorcycle), CarDriver = sum(CarDriver), CarPassenger = sum(CarPassenger),
            Bicycle = sum(Bicycle), Pedestrian = sum(Pedestrian), Other = sum(Other))

miss_Destinations = Destinations_sexage_mode[!(Destinations_sexage_mode$Workplace %in% Destinations$AREA_ID),]
# 4 missing Destinations 
# OD0000001 Mainly work at or from home
# OD0000002 Offshore installation
# OD0000003 No fixed place
# OD0000004 Outside UK
physical_OD_SexAge_Mode = OD_SexAge_Mode[OD_SexAge_Mode$Workplace %in% Destinations$AREA_ID,]

OD_SexAge_Mode %>% 
  count(n_distinct(Workplace)) #5502 unique Destinations



OD_SexAge_Mode %>% 
  count(n_distinct(Residence)) # 692 unique Origins

Origins_sexage_mode = OD_SexAge_Mode %>% 
  group_by(Residence) %>% 
  summarise(Total=sum(Total), Male = sum(Male), Female = sum(Female), `16-24` = sum(`X16.24`), `25-34` = sum(`X25.34`), 
            `35-44` = sum(`X35.44`),`45-54` = sum(`X45.54`), `55-64` = sum(`X55.64`), `65-74` = sum(`X65.74`), `Over75` = sum(Over75), 
            WorkFromHome = sum(WorkFromHome), MetroTram = sum(MetroTram), Train=sum(Train), Bus=sum(Bus), 
            Taxi=sum(Taxi), Motorcycle = sum(Motorcycle), CarDriver = sum(CarDriver), CarPassenger = sum(CarPassenger),
            Bicycle = sum(Bicycle), Pedestrian = sum(Pedestrian), Other = sum(Other))

# Check that totals in Sociodemographics and OD datasets are the same
identical(Origins_sexage_mode$Total, AgeEcoOccupSex$Over16) # Totals are identical


Destinations_sexage_mode_endo = Destinations_sexage_mode[Destinations_sexage_mode$Workplace %in% Origins_sexage_mode$Residence,]
sum(Destinations_sexage_mode_endo$Total) # 1918301

Destinations_sexage_mode_intra = Destinations_sexage_mode[!(Destinations_sexage_mode$Workplace %in% Origins_sexage_mode$Residence),]
sum(Destinations_sexage_mode_intra$Total) # 521709

Destinations_sexage_mode_special = Destinations_sexage_mode_intra[(Destinations_sexage_mode_intra$Workplace %in% miss_Destinations$Workplace),]
sum(Destinations_sexage_mode_special$Total) # 406134

Destinations_sexage_mode_intra = Destinations_sexage_mode_intra[!(Destinations_sexage_mode_intra$Workplace %in% miss_Destinations$Workplace),]
sum(Destinations_sexage_mode_intra$Total) # 115575

total_commuting_numbers = data.frame(Destination_type = c('Inside Yorkshire', 'Rest of the UK', 'Special categories'),
                                     Commuting_trips = c(sum(Destinations_sexage_mode_endo$Total), sum(Destinations_sexage_mode_intra$Total),
                                                         sum(Destinations_sexage_mode_special$Total)   ))


# Create a new df to have the 3 destination types
total_commuting_numbers$perc = paste0(round(total_commuting_numbers$Commuting_trips/sum(total_commuting_numbers$Commuting_trips)*100, 1), '%')


# Create a pie for Destination types (Fig.3.25)
pie=ggpie(total_commuting_numbers, 'Commuting_trips', label='perc', fill='Destination_type',
          palette = (colour=c('#003399', '#006699', '#3399CC')), color='white', lab.pos = 'in', lab.font = c(6,'white'))
pie


##### Plot endo Origin and Destination totals ############################################################


Generation_spatial = left_join(Yorkshire1, Origins_sexage_mode, by=c('AREA_ID'='Residence'))
str(Generation_spatial)

Generators=tm_shape(Generation_spatial)+
  tm_fill('Total', palette = "Blues", title = 'Commuting trip generation', breaks = c(0,4000,8000,20000,80000))+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
Generators

Attraction_spatial = left_join(Yorkshire1, Destinations_sexage_mode_endo, by=c('AREA_ID'='Workplace'))
str(Attraction_spatial)


Attractors=tm_shape(Attraction_spatial)+
  tm_fill('Total', palette = "Blues", title = 'Commuting trip attraction', breaks = c(0,4000,8000,20000,80000))+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
Attractors
