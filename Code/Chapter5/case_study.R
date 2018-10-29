# library(dplyr)
# library(sf)
# library(tmap)
# library(stplanr)
# #tmap_mode("view")
# library(ggplot2)
# library(reshape2)

source('Code/Chapter3/OD_pairs.R')

##### Scenario analysis ###########################################################
ints_OD_SexAge_Mode = read.csv('Datasets/IPF/ints_OD_SexAge_Mode.csv')

#str(ints_OD_SexAge_Mode)
ints_OD_SexAge_Mode$Destination = as.character(ints_OD_SexAge_Mode$Destination)

# Check structure of ints_OD_SexAge_Mode to see that Mode is simply an integer column and not a factor
str(ints_OD_SexAge_Mode)

# Change Mode to factor in a new column
ints_OD_SexAge_Mode$Mode_factor = as.factor(ints_OD_SexAge_Mode$Mode)

# Define factor levels for the Mode column
levels(ints_OD_SexAge_Mode$Mode_factor) = c('WorkFromHome', 'MetroTram', 'Train', 'Bus', 'Taxi', 'Motorcycle', 'CarDriver', 'CarPassenger', 'Bicycle', 'Pedestrian', 'Other')

ints_OD_SexAge_Mode$Mode_char = as.character(ints_OD_SexAge_Mode$Mode_factor)

head(ints_OD_SexAge_Mode)

##### Aggregate-level Analysis #################################################################

#### Car Commuters #############################################################################

#### Car commuters (Drivers-Passengers) with Social Grade A-B commuting to Leeds ###############

car_commuters_SG_AB_Leeds = ints_OD_SexAge_Mode[(ints_OD_SexAge_Mode$Destination=='E02006875') & 
                                                  ((ints_OD_SexAge_Mode$Mode==7)|(ints_OD_SexAge_Mode$Mode==8)) & (ints_OD_SexAge_Mode$Approximated.Social.Grade==1)  ,]


agg_car_commuters_SG_AB_Leeds = car_commuters_SG_AB_Leeds %>% 
  group_by(Origin) %>% 
  summarise(Total = n(), Male = sum(Sex==1), Female = sum(Sex==2), '16-24' = sum(Age==2), '25-34' = sum(Age==3), '35-44' = sum(Age==4),'45-54' = sum(Age==5), '55-64' = sum(Age==6), '65-74' = sum(Age==7), 'Over75' = sum(Age==8), Health_1 = sum(Health==1), Health_2 = sum(Health==2), Health_3 = sum(Health==3), Health_4 = sum(Health==4), Health_5 = sum(Health==5))

agg_car_commuters_SG_AB_Leeds$Destination = 'E02006875'
agg_car_commuters_SG_AB_Leeds = agg_car_commuters_SG_AB_Leeds[agg_car_commuters_SG_AB_Leeds$Origin != 'E02006875', ] # exclude residents of Leeds city centre

agg_car_commuters_SG_AB_Leeds$OD = paste0(agg_car_commuters_SG_AB_Leeds$Origin, agg_car_commuters_SG_AB_Leeds$Destination)

desire_lines_intra_nonendo_car_Leeds = desire_lines_intra_nonendo[desire_lines_intra_nonendo$ResWork %in% agg_car_commuters_SG_AB_Leeds$OD, ]

spatial_agg_car_commuters_SG_AB_Leeds = left_join(desire_lines_intra_nonendo_car_Leeds, agg_car_commuters_SG_AB_Leeds, by=c('ResWork'='OD'))

# Map
Yorkmap = tm_shape(Yorkshire1)+
  tm_fill('LocalAuthority', legend.show = FALSE, palette = 'Blues')+
  tm_borders(alpha=.5)

od_map_SG_AB_car=tm_shape(spatial_agg_car_commuters_SG_AB_Leeds)+
  tm_lines(lwd="Total.y", scale=10, col='Total.y', style = "jenks", title.col  = 'Car commuters with Social Grade A-B')
Yorkmap+od_map_SG_AB_car

#### Car - Social grade C1 ##############################################################
car_commuters_SG_C1_Leeds = ints_OD_SexAge_Mode[(ints_OD_SexAge_Mode$Destination=='E02006875') & 
                                                  ((ints_OD_SexAge_Mode$Mode==7)|(ints_OD_SexAge_Mode$Mode==8)) & (ints_OD_SexAge_Mode$Approximated.Social.Grade==2)  ,]

agg_car_commuters_SG_C1_Leeds = car_commuters_SG_C1_Leeds %>% 
  group_by(Origin) %>% 
  summarise(Total = n(), Male = sum(Sex==1), Female = sum(Sex==2), '16-24' = sum(Age==2), '25-34' = sum(Age==3), '35-44' = sum(Age==4),'45-54' = sum(Age==5), '55-64' = sum(Age==6), '65-74' = sum(Age==7), 'Over75' = sum(Age==8), Health_1 = sum(Health==1), Health_2 = sum(Health==2), Health_3 = sum(Health==3), Health_4 = sum(Health==4), Health_5 = sum(Health==5))

agg_car_commuters_SG_C1_Leeds$Destination = 'E02006875'
agg_car_commuters_SG_C1_Leeds = agg_car_commuters_SG_C1_Leeds[agg_car_commuters_SG_C1_Leeds$Origin != 'E02006875', ] # exclude residents of Leeds city centre

agg_car_commuters_SG_C1_Leeds$OD = paste0(agg_car_commuters_SG_C1_Leeds$Origin, agg_car_commuters_SG_C1_Leeds$Destination)

desire_lines_intra_nonendo_car_Leeds = desire_lines_intra_nonendo[desire_lines_intra_nonendo$ResWork %in% agg_car_commuters_SG_C1_Leeds$OD, ]

spatial_agg_car_commuters_SG_C1_Leeds = left_join(desire_lines_intra_nonendo_car_Leeds, agg_car_commuters_SG_C1_Leeds, by=c('ResWork'='OD'))

# Map
od_map_SG_C1_car=tm_shape(spatial_agg_car_commuters_SG_C1_Leeds)+
  tm_lines(lwd="Total.y", scale=10, col='Total.y', style = "jenks", title.col  = 'Car commuters with Social Grade C1')
Yorkmap+od_map_SG_C1_car

#### Social grade C2 ###########################################################################

car_commuters_SG_C2_Leeds = ints_OD_SexAge_Mode[(ints_OD_SexAge_Mode$Destination=='E02006875') & 
                                                  ((ints_OD_SexAge_Mode$Mode==7)|(ints_OD_SexAge_Mode$Mode==8)) & (ints_OD_SexAge_Mode$Approximated.Social.Grade==3)  ,]

agg_car_commuters_SG_C2_Leeds = car_commuters_SG_C2_Leeds %>% 
  group_by(Origin) %>% 
  summarise(Total = n(), Male = sum(Sex==1), Female = sum(Sex==2), '16-24' = sum(Age==2), '25-34' = sum(Age==3), '35-44' = sum(Age==4),'45-54' = sum(Age==5), '55-64' = sum(Age==6), '65-74' = sum(Age==7), 'Over75' = sum(Age==8), Health_1 = sum(Health==1), Health_2 = sum(Health==2), Health_3 = sum(Health==3), Health_4 = sum(Health==4), Health_5 = sum(Health==5))

agg_car_commuters_SG_C2_Leeds$Destination = 'E02006875'
agg_car_commuters_SG_C2_Leeds = agg_car_commuters_SG_C2_Leeds[agg_car_commuters_SG_C2_Leeds$Origin != 'E02006875', ] # exclude residents of Leeds city centre

agg_car_commuters_SG_C2_Leeds$OD = paste0(agg_car_commuters_SG_C2_Leeds$Origin, agg_car_commuters_SG_C2_Leeds$Destination)

desire_lines_intra_nonendo_car_Leeds = desire_lines_intra_nonendo[desire_lines_intra_nonendo$ResWork %in% agg_car_commuters_SG_C2_Leeds$OD, ]

spatial_agg_car_commuters_SG_C2_Leeds = left_join(desire_lines_intra_nonendo_car_Leeds, agg_car_commuters_SG_C2_Leeds, by=c('ResWork'='OD'))

# Map
od_map_SG_C2_car=tm_shape(spatial_agg_car_commuters_SG_C2_Leeds)+
  tm_lines(lwd="Total.y", scale=10, col='Total.y', style = "jenks", title.col  = 'Car commuters with Social Grade C2')
Yorkmap+od_map_SG_C2_car

#### Car - Social grade D-E #######################################################

car_commuters_SG_DE_Leeds = ints_OD_SexAge_Mode[(ints_OD_SexAge_Mode$Destination=='E02006875') & 
                                                  ((ints_OD_SexAge_Mode$Mode==7)|(ints_OD_SexAge_Mode$Mode==8)) & (ints_OD_SexAge_Mode$Approximated.Social.Grade==4)  ,]

agg_car_commuters_SG_DE_Leeds = car_commuters_SG_DE_Leeds %>% 
  group_by(Origin) %>% 
  summarise(Total = n(), Male = sum(Sex==1), Female = sum(Sex==2), '16-24' = sum(Age==2), '25-34' = sum(Age==3), '35-44' = sum(Age==4),'45-54' = sum(Age==5), '55-64' = sum(Age==6), '65-74' = sum(Age==7), 'Over75' = sum(Age==8), Health_1 = sum(Health==1), Health_2 = sum(Health==2), Health_3 = sum(Health==3), Health_4 = sum(Health==4), Health_5 = sum(Health==5))

agg_car_commuters_SG_DE_Leeds$Destination = 'E02006875'
agg_car_commuters_SG_DE_Leeds = agg_car_commuters_SG_DE_Leeds[agg_car_commuters_SG_DE_Leeds$Origin != 'E02006875', ] # exclude residents of Leeds city centre

agg_car_commuters_SG_DE_Leeds$OD = paste0(agg_car_commuters_SG_DE_Leeds$Origin, agg_car_commuters_SG_DE_Leeds$Destination)

desire_lines_intra_nonendo_car_Leeds = desire_lines_intra_nonendo[desire_lines_intra_nonendo$ResWork %in% agg_car_commuters_SG_DE_Leeds$OD, ]

spatial_agg_car_commuters_SG_DE_Leeds = left_join(desire_lines_intra_nonendo_car_Leeds, agg_car_commuters_SG_DE_Leeds, by=c('ResWork'='OD'))

# Map
od_map_SG_DE_car=tm_shape(spatial_agg_car_commuters_SG_DE_Leeds)+
  tm_lines(lwd="Total.y", scale=10, col='Total.y', style = "jenks", title.col  = 'Car commuters with Social Grade D-E')
Yorkmap+od_map_SG_DE_car



#### Combine all affected car commuters #################################################
car_commuters_SG_Leeds_total=rbind(car_commuters_SG_AB_Leeds[car_commuters_SG_AB_Leeds$Origin != 'E02006875', ], car_commuters_SG_C1_Leeds[car_commuters_SG_C1_Leeds$Origin != 'E02006875', ], car_commuters_SG_C2_Leeds[car_commuters_SG_C2_Leeds$Origin != 'E02006875', ], car_commuters_SG_DE_Leeds[car_commuters_SG_DE_Leeds$Origin != 'E02006875', ])


#### PT Commuters ########################################################################
#### PT commuters with Social Grade A-B commuting to Leeds ###############################

PT_commuters_SG_AB_Leeds = ints_OD_SexAge_Mode[(ints_OD_SexAge_Mode$Destination=='E02006875') & 
                                                 ((ints_OD_SexAge_Mode$Mode==2)|(ints_OD_SexAge_Mode$Mode==3)|(ints_OD_SexAge_Mode$Mode==4)) & (ints_OD_SexAge_Mode$Approximated.Social.Grade==1)  ,]

agg_PT_commuters_SG_AB_Leeds = PT_commuters_SG_AB_Leeds %>% 
  group_by(Origin) %>% 
  summarise(Total = n(), Male = sum(Sex==1), Female = sum(Sex==2), '16-24' = sum(Age==2), '25-34' = sum(Age==3), '35-44' = sum(Age==4),'45-54' = sum(Age==5), '55-64' = sum(Age==6), '65-74' = sum(Age==7), 'Over75' = sum(Age==8), Health_1 = sum(Health==1), Health_2 = sum(Health==2), Health_3 = sum(Health==3), Health_4 = sum(Health==4), Health_5 = sum(Health==5))

agg_PT_commuters_SG_AB_Leeds$Destination = 'E02006875'

agg_PT_commuters_SG_AB_Leeds$OD = paste0(agg_PT_commuters_SG_AB_Leeds$Origin, agg_PT_commuters_SG_AB_Leeds$Destination)

desire_lines_intra_nonendo_PT_Leeds = desire_lines_intra_nonendo[desire_lines_intra_nonendo$ResWork %in% agg_PT_commuters_SG_AB_Leeds$OD, ]
#str(desire_lines_intra_nonendo_PT_Leeds)
desire_lines_intra_nonendo_PT_Leeds$ResWork = as.character(desire_lines_intra_nonendo_PT_Leeds$ResWork)
spatial_agg_PT_commuters_SG_AB_Leeds = left_join(desire_lines_intra_nonendo_PT_Leeds, agg_PT_commuters_SG_AB_Leeds, by=c('ResWork'='OD'))

#str(spatial_agg_PT_commuters_SG_AB_Leeds)
spatial_agg_PT_commuters_SG_AB_Leeds = st_as_sf(spatial_agg_PT_commuters_SG_AB_Leeds)

# Map
od_map_SG_AB_pt=tm_shape(spatial_agg_PT_commuters_SG_AB_Leeds)+
  tm_lines(lwd="Total.y", scale=10, col='Total.y', style = "jenks", title.col  = 'PT commuters with Social Grade A-B')
Yorkmap+od_map_SG_AB_pt

#### PT commuters with Social Grade C1 commuting to Leeds ##########################################

PT_commuters_SG_C1_Leeds = ints_OD_SexAge_Mode[(ints_OD_SexAge_Mode$Destination=='E02006875') & 
                                                 ((ints_OD_SexAge_Mode$Mode==2)|(ints_OD_SexAge_Mode$Mode==3)|(ints_OD_SexAge_Mode$Mode==4)) & (ints_OD_SexAge_Mode$Approximated.Social.Grade==2)  ,]

agg_PT_commuters_SG_C1_Leeds = PT_commuters_SG_C1_Leeds %>% 
  group_by(Origin) %>% 
  summarise(Total = n(), Male = sum(Sex==1), Female = sum(Sex==2), '16-24' = sum(Age==2), '25-34' = sum(Age==3), '35-44' = sum(Age==4),'45-54' = sum(Age==5), '55-64' = sum(Age==6), '65-74' = sum(Age==7), 'Over75' = sum(Age==8), Health_1 = sum(Health==1), Health_2 = sum(Health==2), Health_3 = sum(Health==3), Health_4 = sum(Health==4), Health_5 = sum(Health==5))

agg_PT_commuters_SG_C1_Leeds$Destination = 'E02006875'

agg_PT_commuters_SG_C1_Leeds$OD = paste0(agg_PT_commuters_SG_C1_Leeds$Origin, agg_PT_commuters_SG_C1_Leeds$Destination)

desire_lines_intra_nonendo_PT_Leeds = desire_lines_intra_nonendo[desire_lines_intra_nonendo$ResWork %in% agg_PT_commuters_SG_C1_Leeds$OD, ]
#str(desire_lines_intra_nonendo_PT_Leeds)
desire_lines_intra_nonendo_PT_Leeds$ResWork = as.character(desire_lines_intra_nonendo_PT_Leeds$ResWork)
spatial_agg_PT_commuters_SG_C1_Leeds = left_join(desire_lines_intra_nonendo_PT_Leeds, agg_PT_commuters_SG_C1_Leeds, by=c('ResWork'='OD'))

#str(spatial_agg_PT_commuters_SG_C1_Leeds)
#spatial_agg_PT_commuters_SG_AB_Leeds = st_as_sf(spatial_agg_PT_commuters_SG_AB_Leeds)

# Map
od_map_SG_C1_pt=tm_shape(spatial_agg_PT_commuters_SG_C1_Leeds)+
  tm_lines(lwd="Total.y", scale=10, col='Total.y', style = "jenks", title.col  = 'PT commuters with Social Grade C1')
Yorkmap+od_map_SG_C1_pt

#### PT commuters with Social Grade C2 commuting to Leeds ###########################################

PT_commuters_SG_C2_Leeds = ints_OD_SexAge_Mode[(ints_OD_SexAge_Mode$Destination=='E02006875') & 
                                                 ((ints_OD_SexAge_Mode$Mode==2)|(ints_OD_SexAge_Mode$Mode==3)|(ints_OD_SexAge_Mode$Mode==4)) & (ints_OD_SexAge_Mode$Approximated.Social.Grade==3)  ,]

agg_PT_commuters_SG_C2_Leeds = PT_commuters_SG_C2_Leeds %>% 
  group_by(Origin) %>% 
  summarise(Total = n(), Male = sum(Sex==1), Female = sum(Sex==2), '16-24' = sum(Age==2), '25-34' = sum(Age==3), '35-44' = sum(Age==4),'45-54' = sum(Age==5), '55-64' = sum(Age==6), '65-74' = sum(Age==7), 'Over75' = sum(Age==8), Health_1 = sum(Health==1), Health_2 = sum(Health==2), Health_3 = sum(Health==3), Health_4 = sum(Health==4), Health_5 = sum(Health==5))

agg_PT_commuters_SG_C2_Leeds$Destination = 'E02006875'

agg_PT_commuters_SG_C2_Leeds$OD = paste0(agg_PT_commuters_SG_C2_Leeds$Origin, agg_PT_commuters_SG_C2_Leeds$Destination)

desire_lines_intra_nonendo_PT_Leeds = desire_lines_intra_nonendo[desire_lines_intra_nonendo$ResWork %in% agg_PT_commuters_SG_C2_Leeds$OD, ]
#str(desire_lines_intra_nonendo_PT_Leeds)
desire_lines_intra_nonendo_PT_Leeds$ResWork = as.character(desire_lines_intra_nonendo_PT_Leeds$ResWork)
spatial_agg_PT_commuters_SG_C2_Leeds = left_join(desire_lines_intra_nonendo_PT_Leeds, agg_PT_commuters_SG_C2_Leeds, by=c('ResWork'='OD'))

#str(spatial_agg_PT_commuters_SG_C2_Leeds)
#spatial_agg_PT_commuters_SG_AB_Leeds = st_as_sf(spatial_agg_PT_commuters_SG_AB_Leeds)

# Map
od_map_SG_C2_pt=tm_shape(spatial_agg_PT_commuters_SG_C2_Leeds)+
  tm_lines(lwd="Total.y", scale=10, col='Total.y', style = "jenks", title.col  = 'PT commuters with Social Grade C2')
Yorkmap+od_map_SG_C2_pt

#### PT commuters with Social Grade D-E commuting to Leeds #######################################

PT_commuters_SG_DE_Leeds = ints_OD_SexAge_Mode[(ints_OD_SexAge_Mode$Destination=='E02006875') & 
                                                 ((ints_OD_SexAge_Mode$Mode==2)|(ints_OD_SexAge_Mode$Mode==3)|(ints_OD_SexAge_Mode$Mode==4)) & (ints_OD_SexAge_Mode$Approximated.Social.Grade==4)  ,]

agg_PT_commuters_SG_DE_Leeds = PT_commuters_SG_DE_Leeds %>% 
  group_by(Origin) %>% 
  summarise(Total = n(), Male = sum(Sex==1), Female = sum(Sex==2), '16-24' = sum(Age==2), '25-34' = sum(Age==3), '35-44' = sum(Age==4),'45-54' = sum(Age==5), '55-64' = sum(Age==6), '65-74' = sum(Age==7), 'Over75' = sum(Age==8), Health_1 = sum(Health==1), Health_2 = sum(Health==2), Health_3 = sum(Health==3), Health_4 = sum(Health==4), Health_5 = sum(Health==5))

agg_PT_commuters_SG_DE_Leeds$Destination = 'E02006875'

agg_PT_commuters_SG_DE_Leeds$OD = paste0(agg_PT_commuters_SG_DE_Leeds$Origin, agg_PT_commuters_SG_DE_Leeds$Destination)

desire_lines_intra_nonendo_PT_Leeds = desire_lines_intra_nonendo[desire_lines_intra_nonendo$ResWork %in% agg_PT_commuters_SG_DE_Leeds$OD, ]
#str(desire_lines_intra_nonendo_PT_Leeds)
desire_lines_intra_nonendo_PT_Leeds$ResWork = as.character(desire_lines_intra_nonendo_PT_Leeds$ResWork)
spatial_agg_PT_commuters_SG_DE_Leeds = left_join(desire_lines_intra_nonendo_PT_Leeds, agg_PT_commuters_SG_DE_Leeds, by=c('ResWork'='OD'))

#str(spatial_agg_PT_commuters_SG_DE_Leeds)
#spatial_agg_PT_commuters_SG_AB_Leeds = st_as_sf(spatial_agg_PT_commuters_SG_AB_Leeds)

# Map
od_map_SG_DE_pt=tm_shape(spatial_agg_PT_commuters_SG_DE_Leeds)+
  tm_lines(lwd="Total.y", scale=10, col='Total.y', style = "jenks", title.col  = 'PT commuters with Social Grade D-E')
Yorkmap+od_map_SG_DE_pt

#### Combine all affected PT commuters ######################################################

PT_commuters_SG_Leeds_total = rbind(PT_commuters_SG_AB_Leeds, PT_commuters_SG_C1_Leeds, PT_commuters_SG_C2_Leeds, PT_commuters_SG_DE_Leeds)



#### Create histogram - Figure 5.1 #################################################

car_commuters_SG_Leeds_total$affected = 1
PT_commuters_SG_Leeds_total$affected = 2
affected_population = rbind(car_commuters_SG_Leeds_total, PT_commuters_SG_Leeds_total)

affected_population2 = data.frame(Social_Grade = c('A-B', 'C1', 'C2', 'D-E'), Car_commuters = c(nrow(car_commuters_SG_AB_Leeds[car_commuters_SG_AB_Leeds$Origin != 'E02006875', ]), nrow(car_commuters_SG_C1_Leeds[car_commuters_SG_C1_Leeds$Origin != 'E02006875', ]), nrow(car_commuters_SG_C2_Leeds[car_commuters_SG_C2_Leeds$Origin != 'E02006875', ]), nrow(car_commuters_SG_DE_Leeds[car_commuters_SG_DE_Leeds$Origin != 'E02006875', ])), PT_commuters = c(nrow(PT_commuters_SG_AB_Leeds), nrow(PT_commuters_SG_C1_Leeds), nrow(PT_commuters_SG_C2_Leeds), nrow(PT_commuters_SG_DE_Leeds))) 

#str(affected_population2)
affected_population2$Social_Grade = as.character(affected_population2$Social_Grade)

# Add last line with Totals
Totals = c('Total', sum(affected_population2$Car_commuters), sum(affected_population2$PT_commuters))

affected_population22 = rbind(affected_population2, Totals)

# Change colnames
colnm = c('Approximated Social Grade', 'Private car commuters (drivers+passengers)', 'Public transport commuters (Metro-Tram, Train, Bus)')

colnames(affected_population22) = colnm


affected_population3 <- melt(affected_population2, id.vars='Social_Grade')
#head(affected_population2)

p <-ggplot(affected_population3, aes(x=Social_Grade, y=value, fill=variable))
p +geom_bar(stat = "identity", position='dodge')+
  theme(text = element_text(size = 10))


#### Affected car commuters with Social Grade A-B ###############################

# Car commuters

agg_car_commuters_SG_AB_Leeds_Origins = agg_car_commuters_SG_AB_Leeds %>% 
  group_by(Origin) %>% 
  summarize(ODTotal=sum(Total))


agg_car_commuters_SG_AB_Leeds_Origins = left_join(agg_car_commuters_SG_AB_Leeds_Origins, AgeEcoOccupSex, by=c('Origin'='X.1'))
# Drop unnecessary columns
agg_car_commuters_SG_AB_Leeds_Origins = agg_car_commuters_SG_AB_Leeds_Origins[,c(1,2,42)]
# Find percentage of affected commuters for the total zonal population
agg_car_commuters_SG_AB_Leeds_Origins$Perc = agg_car_commuters_SG_AB_Leeds_Origins$ODTotal/agg_car_commuters_SG_AB_Leeds_Origins$Over16
# Transform it to sf
spatial_agg_car_commuters_SG_AB_Leeds_Origins <- left_join(agg_car_commuters_SG_AB_Leeds_Origins, Yorkshire1, by=c('Origin'='AREA_ID'))
#str(spatial_agg_car_commuters_SG_AB_Leeds_Origins) # Still not sf because i used a df on the left and sf on the right
# Now finally transform it!
spatial_agg_car_commuters_SG_AB_Leeds_Origins = st_as_sf(spatial_agg_car_commuters_SG_AB_Leeds_Origins)

affected_car_AB = tm_shape(spatial_agg_car_commuters_SG_AB_Leeds_Origins)+
  tm_fill('Perc', palette = "Reds", title = 'Percentage of affected car commuters with Social Grade A-B')+
  tm_borders(alpha=.5)+
  tm_scale_bar()

Leeds = tm_shape(Yorkshire1[Yorkshire1$AREA_ID=='E02006875',])+
  tm_fill('AREA_ID', col = 'black', alpha = 1, title = 'Leeds city centre')+
  tm_borders(alpha=.5)+
  tm_scale_bar()
affected_car_AB+Leeds


#### Affected car commuters with Social Grade C1 #####################################

agg_car_commuters_SG_C1_Leeds_Origins = agg_car_commuters_SG_C1_Leeds %>% 
  group_by(Origin) %>% 
  summarize(ODTotal=sum(Total))

agg_car_commuters_SG_C1_Leeds_Origins = left_join(agg_car_commuters_SG_C1_Leeds_Origins, AgeEcoOccupSex, by=c('Origin'='X.1'))
# Drop unnecessary columns
agg_car_commuters_SG_C1_Leeds_Origins = agg_car_commuters_SG_C1_Leeds_Origins[,c(1,2,42)]
# Find percentage of affected commuters for the total zonal population
agg_car_commuters_SG_C1_Leeds_Origins$Perc = agg_car_commuters_SG_C1_Leeds_Origins$ODTotal/agg_car_commuters_SG_C1_Leeds_Origins$Over16
# Transform it to sf
spatial_agg_car_commuters_SG_C1_Leeds_Origins <- left_join(agg_car_commuters_SG_C1_Leeds_Origins, Yorkshire1, by=c('Origin'='AREA_ID'))
#str(spatial_agg_car_commuters_SG_C1_Leeds_Origins) # Still not sf because i used a df on the left and sf on the right
# Now finally transform it!
spatial_agg_car_commuters_SG_C1_Leeds_Origins = st_as_sf(spatial_agg_car_commuters_SG_C1_Leeds_Origins)

affected_car_C1=tm_shape(spatial_agg_car_commuters_SG_C1_Leeds_Origins)+
  tm_fill('Perc', palette = "Reds", title = 'Percentage of affected car commuters with Social Grade C1')+
  tm_borders(alpha=.5)+
  tm_scale_bar()

affected_car_C1+Leeds


#### Affected car commuters with Social Grade C2 #######################################

agg_car_commuters_SG_C2_Leeds_Origins = agg_car_commuters_SG_C2_Leeds %>% 
  group_by(Origin) %>% 
  summarize(ODTotal=sum(Total))

agg_car_commuters_SG_C2_Leeds_Origins = left_join(agg_car_commuters_SG_C2_Leeds_Origins, AgeEcoOccupSex, by=c('Origin'='X.1'))
# Drop unnecessary columns
agg_car_commuters_SG_C2_Leeds_Origins = agg_car_commuters_SG_C2_Leeds_Origins[,c(1,2,42)]
# Find percentage of affected commuters for the total zonal population
agg_car_commuters_SG_C2_Leeds_Origins$Perc = agg_car_commuters_SG_C2_Leeds_Origins$ODTotal/agg_car_commuters_SG_C2_Leeds_Origins$Over16
# Transform it to sf
spatial_agg_car_commuters_SG_C2_Leeds_Origins <- left_join(agg_car_commuters_SG_C2_Leeds_Origins, Yorkshire1, by=c('Origin'='AREA_ID'))
#str(spatial_agg_car_commuters_SG_C2_Leeds_Origins) # Still not sf because i used a df on the left and sf on the right
# Now finally transform it!
spatial_agg_car_commuters_SG_C2_Leeds_Origins = st_as_sf(spatial_agg_car_commuters_SG_C2_Leeds_Origins)

affected_car_C2=tm_shape(spatial_agg_car_commuters_SG_C2_Leeds_Origins)+
  tm_fill('Perc', palette = "Reds", title = 'Percentage of affected car commuters with Social Grade C2')+
  tm_borders(alpha=.5)+
  tm_scale_bar()

affected_car_C2+Leeds


#### Affected car commuters with Social Grade D-E ###################################

agg_car_commuters_SG_DE_Leeds_Origins = agg_car_commuters_SG_DE_Leeds %>% 
  group_by(Origin) %>% 
  summarize(ODTotal=sum(Total))

agg_car_commuters_SG_DE_Leeds_Origins = left_join(agg_car_commuters_SG_DE_Leeds_Origins, AgeEcoOccupSex, by=c('Origin'='X.1'))
# Drop unnecessary columns
agg_car_commuters_SG_DE_Leeds_Origins = agg_car_commuters_SG_DE_Leeds_Origins[,c(1,2,42)]
# Find percentage of affected commuters for the total zonal population
agg_car_commuters_SG_DE_Leeds_Origins$Perc = agg_car_commuters_SG_DE_Leeds_Origins$ODTotal/agg_car_commuters_SG_DE_Leeds_Origins$Over16
# Transform it to sf
spatial_agg_car_commuters_SG_DE_Leeds_Origins <- left_join(agg_car_commuters_SG_DE_Leeds_Origins, Yorkshire1, by=c('Origin'='AREA_ID'))
#str(spatial_agg_car_commuters_SG_DE_Leeds_Origins) # Still not sf because i used a df on the left and sf on the right
# Now finally transform it!
spatial_agg_car_commuters_SG_DE_Leeds_Origins = st_as_sf(spatial_agg_car_commuters_SG_DE_Leeds_Origins)

affected_car_DE=tm_shape(spatial_agg_car_commuters_SG_DE_Leeds_Origins)+
  tm_fill('Perc', palette = "Reds", title = 'Percentage of affected car commuters with Social Grade D-E')+
  tm_borders(alpha=.5)+
  tm_scale_bar()

affected_car_DE+Leeds


#### Affected PT commuters with Social Grade A-B #######################################

agg_PT_commuters_SG_AB_Leeds_Origins = agg_PT_commuters_SG_AB_Leeds %>% 
  group_by(Origin) %>% 
  summarize(ODTotal=sum(Total))

agg_PT_commuters_SG_AB_Leeds_Origins = left_join(agg_PT_commuters_SG_AB_Leeds_Origins, AgeEcoOccupSex, by=c('Origin'='X.1'))
# Drop unnecessary columns
agg_PT_commuters_SG_AB_Leeds_Origins = agg_PT_commuters_SG_AB_Leeds_Origins[,c(1,2,42)]
# Find percentage of affected commuters for the total zonal population
agg_PT_commuters_SG_AB_Leeds_Origins$Perc = agg_PT_commuters_SG_AB_Leeds_Origins$ODTotal/agg_PT_commuters_SG_AB_Leeds_Origins$Over16
# Transform it to sf
spatial_agg_PT_commuters_SG_AB_Leeds_Origins <- left_join(agg_PT_commuters_SG_AB_Leeds_Origins, Yorkshire1, by=c('Origin'='AREA_ID'))
#str(spatial_agg_PT_commuters_SG_AB_Leeds_Origins) # Still not sf because i used a df on the left and sf on the right
# Now finally transform it!
spatial_agg_PT_commuters_SG_AB_Leeds_Origins = st_as_sf(spatial_agg_PT_commuters_SG_AB_Leeds_Origins)

affected_PT_AB=tm_shape(spatial_agg_PT_commuters_SG_AB_Leeds_Origins)+
  tm_fill('Perc', palette = "Greens", title = 'Percentage of affected PT commuters with Social Grade A-B')+
  tm_borders(alpha=.5)+
  tm_scale_bar()

affected_PT_AB+Leeds


#### Affected PT commuters with Social Grade C1 ###############################################

agg_PT_commuters_SG_C1_Leeds_Origins = agg_PT_commuters_SG_C1_Leeds %>% 
  group_by(Origin) %>% 
  summarize(ODTotal=sum(Total))

agg_PT_commuters_SG_C1_Leeds_Origins = left_join(agg_PT_commuters_SG_C1_Leeds_Origins, AgeEcoOccupSex, by=c('Origin'='X.1'))
# Drop unnecessary columns
agg_PT_commuters_SG_C1_Leeds_Origins = agg_PT_commuters_SG_C1_Leeds_Origins[,c(1,2,42)]
# Find percentage of affected commuters for the total zonal population
agg_PT_commuters_SG_C1_Leeds_Origins$Perc = agg_PT_commuters_SG_C1_Leeds_Origins$ODTotal/agg_PT_commuters_SG_C1_Leeds_Origins$Over16
# Transform it to sf
spatial_agg_PT_commuters_SG_C1_Leeds_Origins <- left_join(agg_PT_commuters_SG_C1_Leeds_Origins, Yorkshire1, by=c('Origin'='AREA_ID'))
#str(spatial_agg_PT_commuters_SG_C1_Leeds_Origins) # Still not sf because i used a df on the left and sf on the right
# Now finally transform it!
spatial_agg_PT_commuters_SG_C1_Leeds_Origins = st_as_sf(spatial_agg_PT_commuters_SG_C1_Leeds_Origins)

affected_PT_C1=tm_shape(spatial_agg_PT_commuters_SG_C1_Leeds_Origins)+
  tm_fill('Perc', palette = "Greens", title = 'Percentage of affected PT commuters with Social Grade C1')+
  tm_borders(alpha=.5)+
  tm_scale_bar()

affected_PT_C1+Leeds


#### Affected PT commuters with Social Grade C2 ##################################################

agg_PT_commuters_SG_C2_Leeds_Origins = agg_PT_commuters_SG_C2_Leeds %>% 
  group_by(Origin) %>% 
  summarize(ODTotal=sum(Total))

agg_PT_commuters_SG_C2_Leeds_Origins = left_join(agg_PT_commuters_SG_C2_Leeds_Origins, AgeEcoOccupSex, by=c('Origin'='X.1'))
# Drop unnecessary columns
agg_PT_commuters_SG_C2_Leeds_Origins = agg_PT_commuters_SG_C2_Leeds_Origins[,c(1,2,42)]
# Find percentage of affected commuters for the total zonal population
agg_PT_commuters_SG_C2_Leeds_Origins$Perc = agg_PT_commuters_SG_C2_Leeds_Origins$ODTotal/agg_PT_commuters_SG_C2_Leeds_Origins$Over16
# Transform it to sf
spatial_agg_PT_commuters_SG_C2_Leeds_Origins <- left_join(agg_PT_commuters_SG_C2_Leeds_Origins, Yorkshire1, by=c('Origin'='AREA_ID'))
#str(spatial_agg_PT_commuters_SG_C2_Leeds_Origins) # Still not sf because i used a df on the left and sf on the right
# Now finally transform it!
spatial_agg_PT_commuters_SG_C2_Leeds_Origins = st_as_sf(spatial_agg_PT_commuters_SG_C2_Leeds_Origins)

affected_PT_C2=tm_shape(spatial_agg_PT_commuters_SG_C2_Leeds_Origins)+
  tm_fill('Perc', palette = "Greens", title = 'Percentage of affected PT commuters with Social Grade C2')+
  tm_borders(alpha=.5)+
  tm_scale_bar()

affected_PT_C2+Leeds


#### Affected PT commuters with Social Grade D-E ##############################################

agg_PT_commuters_SG_DE_Leeds_Origins = agg_PT_commuters_SG_DE_Leeds %>% 
  group_by(Origin) %>% 
  summarize(ODTotal=sum(Total))

agg_PT_commuters_SG_DE_Leeds_Origins = left_join(agg_PT_commuters_SG_DE_Leeds_Origins, AgeEcoOccupSex, by=c('Origin'='X.1'))
# Drop unnecessary columns
agg_PT_commuters_SG_DE_Leeds_Origins = agg_PT_commuters_SG_DE_Leeds_Origins[,c(1,2,42)]
# Find percentage of affected commuters for the total zonal population
agg_PT_commuters_SG_DE_Leeds_Origins$Perc = agg_PT_commuters_SG_DE_Leeds_Origins$ODTotal/agg_PT_commuters_SG_DE_Leeds_Origins$Over16
# Transform it to sf
spatial_agg_PT_commuters_SG_DE_Leeds_Origins <- left_join(agg_PT_commuters_SG_DE_Leeds_Origins, Yorkshire1, by=c('Origin'='AREA_ID'))
#str(spatial_agg_PT_commuters_SG_DE_Leeds_Origins) # Still not sf because i used a df on the left and sf on the right
# Now finally transform it!
spatial_agg_PT_commuters_SG_DE_Leeds_Origins = st_as_sf(spatial_agg_PT_commuters_SG_DE_Leeds_Origins)

affected_PT_DE=tm_shape(spatial_agg_PT_commuters_SG_DE_Leeds_Origins)+
  tm_fill('Perc', palette = "Greens", title = 'Percentage of affected PT commuters with Social Grade D-E')+
  tm_borders(alpha=.5)+
  tm_scale_bar()

affected_PT_DE+Leeds


##### Create Table 5.3 #######################################################

Social_Grade = c('A-B', 'C1', 'C2', 'D-E')

s1 = spatial_agg_car_commuters_SG_AB_Leeds_Origins[order(spatial_agg_car_commuters_SG_AB_Leeds_Origins$ODTotal, decreasing = TRUE), ]
s1 = s1[1,]
st_geometry(s1) <- NULL

s2 = spatial_agg_car_commuters_SG_C1_Leeds_Origins[order(spatial_agg_car_commuters_SG_C1_Leeds_Origins$ODTotal, decreasing = TRUE), ]
s2 = s2[1,]
st_geometry(s2) <- NULL

s3 = spatial_agg_car_commuters_SG_C2_Leeds_Origins[order(spatial_agg_car_commuters_SG_C2_Leeds_Origins$ODTotal, decreasing = TRUE), ]
s3 = s3[1,]
st_geometry(s3) <- NULL

s4 = spatial_agg_car_commuters_SG_DE_Leeds_Origins[order(spatial_agg_car_commuters_SG_DE_Leeds_Origins$ODTotal, decreasing = TRUE), ]
s4 = s4[1,]
st_geometry(s4) <- NULL

paste0(s1[, 1], ' (', s1[, 7], ')')

MSOA_zone_car = c(paste0(s1[, 1], ' (', s1[, 7], ')'), paste0(s2[, 1], ' (', s2[, 7], ')'), paste0(s3[, 1], ' (', s3[, 7], ')'), paste0(s4[, 1], ' (', s4[, 7], ')'))


s5 = spatial_agg_PT_commuters_SG_AB_Leeds_Origins[order(spatial_agg_PT_commuters_SG_AB_Leeds_Origins$ODTotal, decreasing = TRUE), ]
s5 = s5[1,]
st_geometry(s5) <- NULL

s6 = spatial_agg_PT_commuters_SG_C1_Leeds_Origins[order(spatial_agg_PT_commuters_SG_C1_Leeds_Origins$ODTotal, decreasing = TRUE), ]
s6 = s6[1,]
st_geometry(s6) <- NULL

s7 = spatial_agg_PT_commuters_SG_C2_Leeds_Origins[order(spatial_agg_PT_commuters_SG_C2_Leeds_Origins$ODTotal, decreasing = TRUE), ]
s7 = s7[1,]
st_geometry(s7) <- NULL

s8 = spatial_agg_PT_commuters_SG_DE_Leeds_Origins[order(spatial_agg_PT_commuters_SG_DE_Leeds_Origins$ODTotal, decreasing = TRUE), ]
s8 = s8[1,]
st_geometry(s8) <- NULL

paste0(s5[, 1], ' (', s5[, 7], ')')

MSOA_zone_pt = c(paste0(s5[, 1], ' (', s5[, 7], ')'), paste0(s6[, 1], ' (', s6[, 7], ')'), paste0(s7[, 1], ' (', s7[, 7], ')'), paste0(s8[, 1], ' (', s8[, 7], ')'))

Origins_affected = data.frame(cbind(Social_Grade, MSOA_zone_car, MSOA_zone_pt))


##### Create Table 5.4 #######################################################

Affected_population_category = c('Car Commuters - Social Grade A-B', 'Car Commuters - Social Grade C1', 'Car Commuters - Social Grade C2', 'Car Commuters - Social Grade D-E', 'PT Commuters - Social Grade A-B', 'PT Commuters - Social Grade C1', 'PT Commuters - Social Grade C2', 'PT Commuters - Social Grade D-E')



s11 = spatial_agg_car_commuters_SG_AB_Leeds_Origins[order(spatial_agg_car_commuters_SG_AB_Leeds_Origins$Perc, decreasing = TRUE), ]
s11 = s11[1,]
st_geometry(s11) <- NULL

s12 = spatial_agg_car_commuters_SG_C1_Leeds_Origins[order(spatial_agg_car_commuters_SG_C1_Leeds_Origins$Perc, decreasing = TRUE), ]
s12 = s12[1,]
st_geometry(s12) <- NULL

s13 = spatial_agg_car_commuters_SG_C2_Leeds_Origins[order(spatial_agg_car_commuters_SG_C2_Leeds_Origins$Perc, decreasing = TRUE), ]
s13 = s13[1,]
st_geometry(s13) <- NULL

s14 = spatial_agg_car_commuters_SG_DE_Leeds_Origins[order(spatial_agg_car_commuters_SG_DE_Leeds_Origins$Perc, decreasing = TRUE), ]
s14 = s14[1,]
st_geometry(s14) <- NULL

s15 = spatial_agg_PT_commuters_SG_AB_Leeds_Origins[order(spatial_agg_PT_commuters_SG_AB_Leeds_Origins$Perc, decreasing = TRUE), ]
s15 = s15[1,]
st_geometry(s15) <- NULL

s16 = spatial_agg_PT_commuters_SG_C1_Leeds_Origins[order(spatial_agg_PT_commuters_SG_C1_Leeds_Origins$Perc, decreasing = TRUE), ]
s16 = s16[1,]
st_geometry(s16) <- NULL

s17 = spatial_agg_PT_commuters_SG_C2_Leeds_Origins[order(spatial_agg_PT_commuters_SG_C2_Leeds_Origins$Perc, decreasing = TRUE), ]
s17 = s17[1,]
st_geometry(s17) <- NULL

s18 = spatial_agg_PT_commuters_SG_DE_Leeds_Origins[order(spatial_agg_PT_commuters_SG_DE_Leeds_Origins$Perc, decreasing = TRUE), ]
s18 = s18[1,]
st_geometry(s18) <- NULL

paste0(s11[, 1], ' (', s11[, 7], ')')

MSOA_zone = c(paste0(s11[, 1], ' (', s11[, 7], ')'), paste0(s12[, 1], ' (', s12[, 7], ')'), paste0(s13[, 1], ' (', s13[, 7], ')'), paste0(s14[, 1], ' (', s14[, 7], ')'), paste0(s15[, 1], ' (', s15[, 7], ')'), paste0(s16[, 1], ' (', s16[, 7], ')'), paste0(s17[, 1], ' (', s17[, 7], ')'), paste0(s18[, 1], ' (', s18[, 7], ')'))

z1 = round(s11[, 4]*100, 1)
z2 = round(s12[, 4]*100, 1)
z3 = round(s13[, 4]*100, 1)
z4 = round(s14[, 4]*100, 1)
z5 = round(s15[, 4]*100, 1)
z6 = round(s16[, 4]*100, 1)
z7 = round(s17[, 4]*100, 1)
z8 = round(s18[, 4]*100, 1)

Zonal_Pop_perc = c(z1, z2, z3, z4, z5, z6, z7, z8)



Origins_affected_perc = data.frame(cbind(Affected_population_category, MSOA_zone, Zonal_Pop_perc))



##### Individual-level Analysis #########################################
# 2 Randomly selected individuals
all_car_commuters_Leeds = rbind(car_commuters_SG_AB_Leeds, car_commuters_SG_C1_Leeds, car_commuters_SG_C2_Leeds, car_commuters_SG_DE_Leeds)

str(all_car_commuters_Leeds)


all_PT_commuters_Leeds = rbind(PT_commuters_SG_AB_Leeds, PT_commuters_SG_C1_Leeds, PT_commuters_SG_C2_Leeds, PT_commuters_SG_DE_Leeds)

str(all_PT_commuters_Leeds)


# Random selection of 1 individual from each dataset
set.seed(27)
random_individuals = rbind(sample_n(all_car_commuters_Leeds, 1), sample_n(all_PT_commuters_Leeds, 1))

str(random_individuals)


# Subset desire lines to include the 2 OD of the sampled inds
desire_lines_intra_nonendo_sampled_inds_casestudy = desire_lines_intra_nonendo[desire_lines_intra_nonendo$ResWork %in% random_individuals$OD, ]

spatial_random_individuals = left_join(desire_lines_intra_nonendo_sampled_inds_casestudy, random_individuals, by=c('ResWork'='OD'))


# Map the 2 sampled individuals
Yorkmap = tm_shape(Yorkshire1)+
  tm_fill('LocalAuthority', legend.show = FALSE, palette = 'Greys', alpha=0.4)+
  tm_borders(alpha=.5)

spatial_ind_sample_Origin = Yorkshire1[Yorkshire1$AREA_ID %in% random_individuals$Origin, ]
spatial_ind_sample_Origin_inds = left_join(spatial_ind_sample_Origin, random_individuals, by=c('AREA_ID'='Origin'))
origin_zones=tm_shape(spatial_ind_sample_Origin_inds)+
  tm_fill('Origin', col='green', alpha=1, legend.show = TRUE, title = 'Origin', popup.vars = c('Sex', 'Age', 'Industry', 'Approximated.Social.Grade', 'Hours.worked.per.week'))+
  tm_borders(alpha=.5)+
  tm_scale_bar()


sampled_destination_zones = Yorkshire1[Yorkshire1$AREA_ID %in% random_individuals$Destination,]
sampled_destination_zones_map = tm_shape(sampled_destination_zones)+
  tm_fill('AREA_ID', col='orange', alpha=1, title = 'Destination', legend.show = TRUE)+
  tm_borders(alpha=.5)+
  tm_scale_bar()

sampled_map=tm_shape(spatial_random_individuals)+
  tm_lines(col='Mode_char',palette=c('red', 'cyan'), scale=3, title.col  = 'Mode of travel of sampled individuals')



Yorkmap+origin_zones+sampled_destination_zones_map+sampled_map

#routes_short = line2route(spatial_random_individuals, route_fun = 'route_osrm')

