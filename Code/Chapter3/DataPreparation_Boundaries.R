# library(sf)
# library(tmap)
# library(tmaptools)
source('Code/Chapter3/DataPreparation_Census.R')
######################## Load UK (England+Wales+Scotland+N.Ireland) MSOA boundaries #########################################
UK_boundaries = st_read('Datasets/Boundaries/UK_2011_Census_Boundaries__MSOA/UK_2011_Census_Boundaries__MSOA.shp')

UK_boundaries = UK_boundaries[,c(1,2,3,23)]

cs = UK_boundaries$AREA_NAME  
UK_boundaries$LocalAuthority = gsub('.{4}$', '', cs)


Yorkshire1 = UK_boundaries[UK_boundaries$AREA_ID %in% AgeEcoOccupSex$X.1,]
# Change crs to EPSG 27700 British Grid
Yorkshire1 = st_transform(Yorkshire1, 27700)
# Add centroids
York_cent = st_centroid(Yorkshire1)


#################### Load UK regions to include Scotland and N.Ireland ###########################
UK_boundaries_regions = st_read('Datasets/Boundaries/UK_2011_Census_Boundaries__Euro_Region/UK_2011_Census_Boundaries__Euro_Region.shp')

UK_boundaries_regions = UK_boundaries_regions[,c(1,3,2,33)]

