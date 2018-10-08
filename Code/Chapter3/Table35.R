library(sf)
library(ggplot2)
library(dplyr)
library(ggpubr)
###########################################################################
# Load and filter OD file 3
ODmode = read.csv('Datasets/OD/wu03ew_v2.csv', skip = 0)
OD_sexage = read.csv('Datasets/OD/OD_sexage.csv', skip = 0)

ODmode = ODmode %>% 
  filter(Area.of.residence %in% OD_sexage$Residence)
# Change column names
cols = c('Residence', 'Workplace', 'Total', 'WorkFromHome', 'MetroTram', 'Train', 'Bus', 'Taxi', 'Motorcycle',
         'CarDriver', 'CarPassenger', 'Bicycle', 'Pedestrian', 'Other')
colnames(ODmode) = cols

ODmode$WorkFromHome_Perc = sum(ODmode$WorkFromHome)/sum(ODmode$Total)
ODmode$MetroTram_Perc = sum(ODmode$MetroTram)/sum(ODmode$Total)
ODmode$Train_Perc = sum(ODmode$Train)/sum(ODmode$Total)
ODmode$Bus_Perc = sum(ODmode$Bus)/sum(ODmode$Total)
ODmode$Taxi_Perc = sum(ODmode$Taxi)/sum(ODmode$Total)
ODmode$Motorcycle_Perc = sum(ODmode$Motorcycle)/sum(ODmode$Total)
ODmode$CarDriver_Perc = sum(ODmode$CarDriver)/sum(ODmode$Total)
ODmode$CarPassenger_Perc = sum(ODmode$CarPassenger)/sum(ODmode$Total)
ODmode$Bicycle_Perc = sum(ODmode$Bicycle)/sum(ODmode$Total)
ODmode$Pedestrian_Perc = sum(ODmode$Pedestrian)/sum(ODmode$Total)
ODmode$Other_Perc = sum(ODmode$Other)/sum(ODmode$Total)
# Total Percentage should be 1
ODmode$Total_Perc = rowSums(ODmode[, 15:25])

# # Keep only the first row that contains with Percentages
mode_perc = ODmode[1, 15:25] 

# Transposed df
t_mode_perc = t(mode_perc)
t_mode_perc = data.frame(t_mode_perc)
t_mode_perc = tibble::rownames_to_column(t_mode_perc)
colnm = c('Method of travel', 'Percentage')
colnames(t_mode_perc) = colnm
t_mode_perc$Percentage = paste0(round(t_mode_perc$Percentage*100, 1), '%')
