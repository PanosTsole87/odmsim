library(dplyr)
library(stplanr)
library(sf)
library(tmap)
library(rgdal)
#tmap_mode("view")

#source('Code/Chapter3/DataPreparation_Boundaries.R')
source('Code/Chapter3/Origins_Destinations.R')

# Create Yorkshire boundaries and zone centroids
boundaries = st_read('Datasets/Boundaries/England_msoa_2011shp/england_msoa_2011.shp')
Yorkshire = dplyr::filter(boundaries, grepl('Barnsley|Bradford|Calderdale|Craven|Doncaster|Hambleton|Harrogate|Kingston upon Hull|Kirklees|Leeds|North East Lincolnshire|North Lincolnshire|Richmondshire|Rotherham|Ryedale|Scarborough|Selby|Sheffield|Wakefield|York', name))
York_cent = st_centroid(Yorkshire)

Destinations = st_transform(Destinations, 27700) # Reproject to BNG
UK_cent = st_centroid(Destinations)

# Intraregional trips
physical_OD_SexAge_Mode_intra=physical_OD_SexAge_Mode[physical_OD_SexAge_Mode$Workplace %in% physical_OD_SexAge_Mode$Residence,]
 
## Interregional trips
physical_OD_SexAge_Mode_inter=physical_OD_SexAge_Mode[!(physical_OD_SexAge_Mode$Workplace %in% physical_OD_SexAge_Mode$Residence),]

# Create desire lines
desire_lines_intra = od2line(flow = physical_OD_SexAge_Mode_intra, zones = York_cent) # Intraregional trips
desire_lines_intra$distance = as.numeric(st_length(desire_lines_intra))
desire_lines_intra_nonendo = desire_lines_intra[desire_lines_intra$distance!=0,] # exclude endozonal trips

desire_lines_inter = od2line(flow = physical_OD_SexAge_Mode_inter, zones = UK_cent) # Interregional trips

desire_lines_intra_nonendo_dest = desire_lines_intra_nonendo[order(desire_lines_intra_nonendo$Workplace),]




################################################################################
# OD maps
# Create Yorkshire map
Yorkmap = tm_shape(Yorkshire1)+
  tm_fill('LocalAuthority', legend.show = FALSE, palette = 'Blues')+
  tm_borders(alpha=.5)
#Yorkmap

# OD with Leeds city centre as Destination
leeds=desire_lines_intra_nonendo[desire_lines_intra_nonendo$Workplace=='E02006875',]


od_map_leeds=tm_shape(leeds)+
  tm_lines(lwd="Total", scale=10, col='Total', style = "jenks")
Yorkmap+od_map_leeds


# OD with Hull city centre as Destination
hull=desire_lines_intra_nonendo[desire_lines_intra_nonendo$Workplace=='E02002680',]

od_map_hull=tm_shape(hull)+
  tm_lines(lwd="Total", scale=10, col='Total', style = "jenks")
Yorkmap+od_map_hull


# OD with York city centre as Destination
york=desire_lines_intra_nonendo[desire_lines_intra_nonendo$Workplace=='E02002784',]

od_map_york=tm_shape(york)+
  tm_lines(lwd="Total", scale=10, col='Total', style = "jenks")
Yorkmap+od_map_york

# OD with Bradford city centre as Destination
bradford=desire_lines_intra_nonendo[desire_lines_intra_nonendo$Workplace=='E02002221',]

od_map_bradford=tm_shape(bradford)+
  tm_lines(lwd="Total", scale=10, col='Total', style = "jenks")
Yorkmap+od_map_bradford


# Per mode
# Car
od_map_leeds_car=tm_shape(leeds)+
  tm_lines(lwd="CarDriver", scale=10, col='CarDriver', style = "jenks")
Yorkmap+od_map_leeds_car

# Create histogram
cardriver_leeds = leeds[leeds$CarDriver!=0, c(23,29)]
cardriver_leeds = rep(cardriver_leeds$distance, cardriver_leeds$CarDriver)
cardriver_leeds = data.frame(cardriver_leeds)
#colnames(cardriver_leeds) = 'Car'

carpassenger_leeds = leeds[leeds$CarPassenger!=0, c(24,29)]
carpassenger_leeds = rep(carpassenger_leeds$distance, carpassenger_leeds$CarPassenger)
carpassenger_leeds = data.frame(carpassenger_leeds)
colnames(carpassenger_leeds) = 'cardriver_leeds'

cardriver_leeds = rbind(cardriver_leeds, carpassenger_leeds)
#class(car_leeds)
#rm(cardriver_leeds, carpassenger_leeds)

p=ggplot(cardriver_leeds, aes(cardriver_leeds, fill=..count..))
p+geom_histogram(color = "black", breaks=seq(0, 120000, by =2500))+
  scale_fill_gradient("Count", low = "green", high = "cyan")+
  geom_vline(aes(xintercept=median(cardriver_leeds, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)+
  labs(x = "Distance (m)", y = 'Car commuting trips')



# Bus
od_map_leeds_bus=tm_shape(leeds)+
  tm_lines(lwd="Bus", scale=10, col='Bus', style = "jenks")
Yorkmap+od_map_leeds_bus

bus_leeds = leeds[leeds$Bus!=0,]
bus_leeds = rep(bus_leeds$distance, bus_leeds$Bus)
bus_leeds = data.frame(bus_leeds)

p=ggplot(bus_leeds, aes(bus_leeds, fill=..count..))
p+geom_histogram(color = "black", breaks=seq(0, 120000, by =2500))+
  scale_fill_gradient("Count", low = "green", high = "cyan")+
  geom_vline(aes(xintercept=median(bus_leeds, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)+
  labs(x = "Distance (m)", y = 'Bus commuting trips')



# Cycling
od_map_leeds_cycling=tm_shape(leeds)+
  tm_lines(lwd="Bicycle", scale=10, col='Bicycle', style = "jenks")
Yorkmap+od_map_leeds_cycling

cycling_leeds = leeds[leeds$Bicycle!=0,]
cycling_leeds = rep(cycling_leeds$distance, cycling_leeds$Bicycle)
cycling_leeds = data.frame(cycling_leeds)

p=ggplot(cycling_leeds, aes(cycling_leeds, fill=..count..))
p+geom_histogram(color = "black", breaks=seq(0, 120000, by =2500))+
  scale_fill_gradient("Count", low = "green", high = "cyan")+
  geom_vline(aes(xintercept=median(cycling_leeds, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)+
  labs(x = "Distance (m)", y = 'Cycling commuting trips')



# Walking
od_map_leeds_pedesrian=tm_shape(leeds)+
  tm_lines(lwd="Pedestrian", scale=10, col='Pedestrian', style = "jenks")
Yorkmap+od_map_leeds_pedesrian

walking_leeds = leeds[leeds$Pedestrian!=0,]
walking_leeds = rep(walking_leeds$distance, walking_leeds$Pedestrian)
walking_leeds = data.frame(walking_leeds)

p=ggplot(walking_leeds, aes(walking_leeds, fill=..count..))
p+geom_histogram(color = "black", breaks=seq(0, 120000, by =2500))+
  scale_fill_gradient("Count", low = "green", high = "cyan")+
  geom_vline(aes(xintercept=median(walking_leeds, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)+
  labs(x = "Distance (m)", y = 'Pedestrian commuting trips')


# Occupancy rate inside the region
(sum(desire_lines_intra_nonendo$CarDriver) + sum(desire_lines_intra_nonendo$CarPassenger))/sum(desire_lines_intra_nonendo$CarDriver)
# 1.11

# Routing FAIL
# l = od2line(flow = flow, zones = cents)
# class(l)
# l = st_as_sf(l)


