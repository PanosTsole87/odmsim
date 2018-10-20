# Load libraries
# library(dplyr)
# library(sf)
# library(tmap)
# library(tmaptools)

##### Sampled Individuals after the 1st-2nd step COMBINED #################################

source('Code/Chapter3/DataPreparation_Microdata.R')

ints_OD_df = read.csv('Datasets/IPF/ints_OD_df.csv')
ints_df = read.csv('Datasets/IPF/ints_df.csv')

# Sample of individuals
# Create a sample of inds with Origins and show their Destinations
ind_sample_Destination = ints_OD_df[ints_OD_df$Person.ID %in% ind_sample$Person.ID, ]
#str(ind_sample_Destination) # Origins and Destinations are Factors and need to change to Characters
ind_sample_Destination$Origin = as.character(ind_sample_Destination$Origin)
ind_sample_Destination$Destination = as.character(ind_sample_Destination$Destination)

# Exclude inter-regional trips
ind_sample_Destination = ind_sample_Destination[ind_sample_Destination$Destination %in% ind_sample_Destination$Origin, ]
# Exclude endozonal trips
ind_sample_Destination = ind_sample_Destination[ind_sample_Destination$Destination != ind_sample_Destination$Origin, ]

ind_sample_Destination1 = ind_sample_Destination[ind_sample_Destination$Person.ID == ind_sample$Person.ID[1],]


ind_sample_Destination2 = ind_sample_Destination[ind_sample_Destination$Person.ID == ind_sample$Person.ID[2],]


ind_sample_Destination3 = ind_sample_Destination[ind_sample_Destination$Person.ID == ind_sample$Person.ID[3],]


ind_sample_Destination4 = ind_sample_Destination[ind_sample_Destination$Person.ID == ind_sample$Person.ID[4],]


ind_sample_Destination5 = ind_sample_Destination[ind_sample_Destination$Person.ID == ind_sample$Person.ID[5],]


# Create a sample of inds and show their Origins
ind_sample_Origin = ints_df[ints_df$Person.ID %in% ind_sample$Person.ID, ]
#str(ind_sample_Origin) # Origins are Factors - Change them to characters
ind_sample_Origin$Origin = as.character(ind_sample_Origin$Origin)

# Select the first Origin for each individual
ind_sample_Origin1 = rbind(ind_sample_Origin[(ind_sample_Origin[ind_sample_Origin$Person.ID==ind_sample$Person.ID[1],] %in% ind_sample_Destination[ind_sample_Destination$Person.ID==ind_sample$Person.ID[1],]),])

ind_sample_Origin1 = ind_sample_Origin[ind_sample_Origin$Person.ID == ind_sample$Person.ID[1],]
ind_sample_Origin1 = ind_sample_Origin1[ind_sample_Origin1$Origin %in% ind_sample_Destination1$Origin, ]

ind_sample_Origin2 = ind_sample_Origin[ind_sample_Origin$Person.ID == ind_sample$Person.ID[2],]
ind_sample_Origin2 = ind_sample_Origin2[ind_sample_Origin2$Origin %in% ind_sample_Destination2$Origin, ]

ind_sample_Origin3 = ind_sample_Origin[ind_sample_Origin$Person.ID == ind_sample$Person.ID[3],]
ind_sample_Origin3 = ind_sample_Origin3[ind_sample_Origin3$Origin %in% ind_sample_Destination3$Origin, ]

ind_sample_Origin4 = ind_sample_Origin[ind_sample_Origin$Person.ID == ind_sample$Person.ID[4],]
ind_sample_Origin4 = ind_sample_Origin4[ind_sample_Origin4$Origin %in% ind_sample_Destination4$Origin, ]

ind_sample_Origin5 = ind_sample_Origin[ind_sample_Origin$Person.ID == ind_sample$Person.ID[5],]
ind_sample_Origin5 = ind_sample_Origin5[ind_sample_Origin5$Origin %in% ind_sample_Destination5$Origin, ]

ind_sample_Origin_final = rbind(sample_n(ind_sample_Origin1,1), sample_n(ind_sample_Origin2,1), sample_n(ind_sample_Origin3,1), sample_n(ind_sample_Origin4,1), sample_n(ind_sample_Origin5,1))


##### Visualize the Origins of the 5 sampled individuals #################################

# Transform it to sf
sample_origins = Yorkshire1[Yorkshire1$AREA_ID %in% ind_sample_Origin_final$Origin, ]
spatial_ind_sample_Origin = left_join(sample_origins, ind_sample_Origin_final, by=c('AREA_ID'='Origin'))

# Map the 5 sampled individuals
Yorkmap = tm_shape(Yorkshire1)+
  tm_fill('LocalAuthority', legend.show = FALSE, col='grey', alpha=0.4)+
  tm_borders(alpha=.5)

#str(spatial_ind_sample_Origin)
spatial_ind_sample_Origin$Sex = as.character(spatial_ind_sample_Origin$Sex)

sampled_map=tm_shape(spatial_ind_sample_Origin)+
  tm_fill('Sex', palette = c('blue', 'red'), alpha=1, title = 'Gender of sampled individuals', labels = c('Male', 'Female'))+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()
Yorkmap+sampled_map


##### Correct the sampled destinations #################################

ind_sample_Destination11 = ind_sample_Destination1[ind_sample_Destination1$Origin %in% ind_sample_Origin_final$Origin, ]

ind_sample_Destination22 = ind_sample_Destination2[ind_sample_Destination2$Origin %in% ind_sample_Origin_final$Origin, ]

ind_sample_Destination33 = ind_sample_Destination3[ind_sample_Destination3$Origin %in% ind_sample_Origin_final$Origin, ]

ind_sample_Destination44 = ind_sample_Destination4[ind_sample_Destination4$Origin %in% ind_sample_Origin_final$Origin, ]

ind_sample_Destination55 = ind_sample_Destination5[ind_sample_Destination5$Origin %in% ind_sample_Origin_final$Origin, ]

ind_sample_Destination_final = rbind(sample_n(ind_sample_Destination11,1), sample_n(ind_sample_Destination22,1), sample_n(ind_sample_Destination33,1), sample_n(ind_sample_Destination44,1), sample_n(ind_sample_Destination55,1))

ind_sample_Destination_final$OD = paste0(ind_sample_Destination_final$Origin, ind_sample_Destination_final$Destination)

##### Visualize the ODs of the 5 sampled individuals ##########################################
# Transform it to sf
sample_desire_lines = desire_lines_intra_nonendo[desire_lines_intra_nonendo$ResWork %in% ind_sample_Destination_final$OD, ]
spatial_ind_sample_destination = left_join(sample_desire_lines, ind_sample_Destination_final, by=c('ResWork'='OD'))

# Map the 5 sampled individuals
Yorkmap = tm_shape(Yorkshire1)+
  tm_fill('LocalAuthority', legend.show = FALSE, palette = 'Greys', alpha=0.4)+
  tm_borders(alpha=.5)

origin_zones=tm_shape(spatial_ind_sample_Origin)+
  tm_fill('Residence', col='green', alpha=1, legend.show = TRUE, title = 'Origin')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()

sampled_destination_zones = Yorkshire1[Yorkshire1$AREA_ID %in% spatial_ind_sample_destination$Workplace,]
sampled_destination_zones_map = tm_shape(sampled_destination_zones)+
  tm_fill('AREA_ID', col='orange', alpha=1, title = 'Destination', legend.show = TRUE)+
  tm_borders(alpha=.5)+
  tm_scale_bar()

#str(spatial_ind_sample_destination)
spatial_ind_sample_destination$Sex = as.character(spatial_ind_sample_destination$Sex)

sampled_map=tm_shape(spatial_ind_sample_destination)+
  tm_lines(col="Sex", palette=c('blue', 'red'), scale=3, title.col  = 'Gender of sampled individuals', labels = c('Male', 'Female'))

Yorkmap+origin_zones+sampled_destination_zones_map+sampled_map

# Failed routing attempt
#routes_short = line2route(spatial_ind_sample_destination, route_fun = 'route_osrm')

# routes_short_map=tm_shape(routes_short)+
#   tm_lines(scale=5, col='Sex', alpha=1, title.col  = 'Gender of sampled individuals', labels = c('Male', 'Female'))


##### Sampled Individuals after 3rd step ################################################

# Sampled individuals
ints_OD_SexAge_Mode = read.csv('Datasets/IPF/ints_OD_SexAge_Mode.csv')
#str(ints_OD_SexAge_Mode)
ints_OD_SexAge_Mode$Mode = as.factor(ints_OD_SexAge_Mode$Mode)

levels(ints_OD_SexAge_Mode$Mode) = c('WorkFromHome', 'MetroTram', 'Train', 'Bus', 'Taxi', 'Motorcycle', 'CarDriver', 'CarPassenger', 'Bicycle', 'Pedestrian', 'Other')

ind_sample_Mode = ints_OD_SexAge_Mode[ints_OD_SexAge_Mode$Person.ID %in% ind_sample_Destination_final$Person.ID, ]

ind_sample_Mode = ind_sample_Mode[ind_sample_Mode$OD %in% ind_sample_Destination_final$OD, ]


# Transform it to sf
spatial_ind_sample_Mode = spatial_ind_sample_destination

spatial_ind_sample_Mode$Mode = ind_sample_Mode$Mode



# Map the 5 sampled individuals
Yorkmap = tm_shape(Yorkshire1)+
  tm_fill('LocalAuthority', legend.show = FALSE, palette = 'Greys', alpha=0.4)+
  tm_borders(alpha=.5)

origin_zones=tm_shape(spatial_ind_sample_Origin)+
  tm_fill('Residence', col='green', alpha=1, legend.show = TRUE, title = 'Origin')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()


sampled_destination_zones = Yorkshire1[Yorkshire1$AREA_ID %in% spatial_ind_sample_destination$Workplace,]
sampled_destination_zones_map = tm_shape(sampled_destination_zones)+
  tm_fill('AREA_ID', col='orange', alpha=1, title = 'Destination', legend.show = TRUE)+
  tm_borders(alpha=.5)+
  tm_scale_bar()



spatial_ind_sample_Mode$Mode_char = as.character(spatial_ind_sample_Mode$Mode)

sampled_ind_map=tm_shape(spatial_ind_sample_Mode)+
  tm_lines(col='Mode_char', palette=c('red', 'blue', 'cyan'), scale=3, title.col  = 'Mode of travel of sampled individuals')



Yorkmap+origin_zones+sampled_destination_zones_map+sampled_ind_map
