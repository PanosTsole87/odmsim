# library(readxl)
# library(sf)
# library(ggplot2)
# library(dplyr)
# library(ggpubr)
###########################################################################
# Load and filter OD file 3
ODmode = read.csv('Datasets/OD/wu03ew_v2.csv', skip = 0)
ODmode = ODmode %>% 
  filter(Area.of.residence %in% OD_sexage$Residence)
# Change column names
cols = c('Residence', 'Workplace', 'Total', 'WorkFromHome', 'MetroTram', 'Train', 'Bus', 'Taxi', 'Motorcycle',
         'CarDriver', 'CarPassenger', 'Bicycle', 'Pedestrian', 'Other')
colnames(ODmode) = cols
ODmode$ResWork = paste0(ODmode$Residence, ODmode$Workplace)
ODmode$Residence = as.character(ODmode$Residence)
ODmode$Workplace = as.character(ODmode$Workplace)
ODmode$ResWork = as.character(ODmode$ResWork)

ODmode_initial_workplace_agg = ODmode %>% 
  group_by( Workplace) %>% 
  summarise(Total = sum(Total), WorkFromHome = sum(WorkFromHome), MetroTram = sum(MetroTram), Train=sum(Train), 
            Bus=sum(Bus), Taxi=sum(Taxi), Motorcycle = sum(Motorcycle), CarDriver = sum(CarDriver), CarPassenger = sum(CarPassenger),
            Bicycle = sum(Bicycle), Pedestrian = sum(Pedestrian), Other = sum(Other))




# Check the Residences of ODmode and OD_SexAge
identical(ODmode$Residence, OD_sexage$Residence) # They are not the same

# Total Commuters are not the same
sum(ODmode$Total) # 2083123
sum(OD_sexage$Total) # 2440010

# Difference of rows
nrow(OD_sexage) - nrow(ODmode) # 25503

ODmode %>% 
  count(n_distinct(Residence)) # ODmode contain 603 origins 
ODmode %>% 
  count(n_distinct(Workplace)) # ODmode contain 5244 destinations (It also contains commuting trips out of the region)
sum(ODmode$Total)              # and total population of 2083123 

OD_sexage %>% 
  count(n_distinct(Residence)) # OD (Age+Sex) contain 692 origins (correct number)
OD_sexage %>% 
  count(n_distinct(Workplace)) # OD (Age+Sex) contain 5502 destinations (So it only contains commuting trips inside the region)
sum(OD_sexage$Total)        # and total population of 2440010 

# Difference of commuters between ODmode and OD_SexAge
sum(OD_sexage$Total) - sum(ODmode$Total) # 356887 commuters

# Find the ResWork not present in ODmode
diffResWork = setdiff(OD_sexage$ResWork, ODmode$ResWork) # there are 25503 ResWork not present in ODmode


diffAgeSex_Mode = OD_sexage %>% 
  filter(ResWork %in% diffResWork) # 25503

sum(diffAgeSex_Mode$Total) # 356887

############################################################################################
# First correct the common rows
OD_sexage_common = OD_sexage[OD_sexage$ResWork %in% ODmode$ResWork,] # 179380 common OD pairs

ODmode$Total_sexage = OD_sexage_common$Total
ODmode$diff =  ODmode$Total - ODmode$Total_sexage # Difference in Totals is 0
ODmode$abs_diff = abs(ODmode$diff) # Difference in Totals is 0
# No need to correct the common rows



############################################################################################
# Then correct the missing OD pairs
OD_sexage = read.csv('Datasets/OD/OD_SexAge.csv')

OD_sexage_noncommon = OD_sexage[!(OD_sexage$ResWork %in% ODmode$ResWork),] 

# Create the missing OD pairs for ODmode
miss_ODmode = OD_sexage_noncommon %>% 
  select(Residence, Workplace, ResWork, Total)

OD_AGE_additional = OD_AGE %>% 
  filter(ResWork %in% diffResWork)
# Find the percentages for each Mode

miss_ODmode$WorkFromHome_Perc = sum(ODmode$WorkFromHome)/sum(ODmode$Total)
miss_ODmode$MetroTram_Perc = sum(ODmode$MetroTram)/sum(ODmode$Total)
miss_ODmode$Train_Perc = sum(ODmode$Train)/sum(ODmode$Total)
miss_ODmode$Bus_Perc = sum(ODmode$Bus)/sum(ODmode$Total)
miss_ODmode$Taxi_Perc = sum(ODmode$Taxi)/sum(ODmode$Total)
miss_ODmode$Motorcycle_Perc = sum(ODmode$Motorcycle)/sum(ODmode$Total)
miss_ODmode$CarDriver_Perc = sum(ODmode$CarDriver)/sum(ODmode$Total)
miss_ODmode$CarPassenger_Perc = sum(ODmode$CarPassenger)/sum(ODmode$Total)
miss_ODmode$Bicycle_Perc = sum(ODmode$Bicycle)/sum(ODmode$Total)
miss_ODmode$Pedestrian_Perc = sum(ODmode$Pedestrian)/sum(ODmode$Total)
miss_ODmode$Other_Perc = sum(ODmode$Other)/sum(ODmode$Total)
# Total Percentage should be 1
miss_ODmode$Total_Perc = rowSums(miss_ODmode[, 5:15])





# # Keep only the first row that contains with Percentages
mode_perc = miss_ODmode[1, 5:15] 

t_mode_perc = t(mode_perc)
t_mode_perc = data.frame(t_mode_perc)


  # Impute Modes for missing OD pairs
miss_ODmode_ints = miss_ODmode[rep(row.names(miss_ODmode), miss_ODmode$Total), ]
miss_ODmode_ints$id = seq(1:nrow(miss_ODmode_ints))
zzz=unique(miss_ODmode_ints$Residence)

miss_ODmode_ints_new=list()


for (i in 1:length(zzz)){
  print(i)
  x = miss_ODmode_ints[miss_ODmode_ints$Residence==zzz[i],]
  
  
  y1=list()
  y2=list()
  y3=list()
  y4=list()
  y5=list()
  y6=list()
  y7=list()
  y8=list()
  y9=list()
  y10=list()
  y11=list()
  y_id=list()
  
  
  y1 = sample_n(x, nrow(x)*mode_perc$WorkFromHome[1])
  y1$WorkFromHome= 1
  y1$MetroTram = 0
  y1$Train = 0
  y1$Bus = 0
  y1$Taxi = 0
  y1$Motorcycle = 0
  y1$CarDriver = 0
  y1$CarPassenger = 0
  y1$Bicycle = 0
  y1$Pedestrian = 0
  y1$Other = 0
  
  
  y_id = rbind(y_id, y1)
  
  y2 = x[!(x$id %in% y_id$id),]
  y2 = sample_n(y2, nrow(x)*mode_perc$MetroTram[1])
  y2$WorkFromHome= 0
  y2$MetroTram = 1
  y2$Train = 0
  y2$Bus = 0
  y2$Taxi = 0
  y2$Motorcycle = 0
  y2$CarDriver = 0
  y2$CarPassenger = 0
  y2$Bicycle = 0
  y2$Pedestrian = 0
  y2$Other = 0
  
  y_id = rbind(y_id, y2)
  
  y3 = x[!(x$id %in% y_id$id),]
  y3 = sample_n(y3, nrow(x)*mode_perc$Train[1])
  y3$WorkFromHome= 0
  y3$MetroTram = 0
  y3$Train = 1
  y3$Bus = 0
  y3$Taxi = 0
  y3$Motorcycle = 0
  y3$CarDriver = 0
  y3$CarPassenger = 0
  y3$Bicycle = 0
  y3$Pedestrian = 0
  y3$Other = 0
  
  y_id = rbind(y_id, y3)
  
  y4 = x[!(x$id %in% y_id$id),]
  y4 = sample_n(y4, nrow(x)*mode_perc$Bus[1])
  y4$WorkFromHome= 0
  y4$MetroTram = 0
  y4$Train = 0
  y4$Bus = 1
  y4$Taxi = 0
  y4$Motorcycle = 0
  y4$CarDriver = 0
  y4$CarPassenger = 0
  y4$Bicycle = 0
  y4$Pedestrian = 0
  y4$Other = 0
  
  y_id = rbind(y_id, y4)
  
  y5 = x[!(x$id %in% y_id$id),]
  y5 = sample_n(y5, nrow(x)*mode_perc$Taxi[1])
  y5$WorkFromHome= 0
  y5$MetroTram = 0
  y5$Train = 0
  y5$Bus = 0
  y5$Taxi = 1
  y5$Motorcycle = 0
  y5$CarDriver = 0
  y5$CarPassenger = 0
  y5$Bicycle = 0
  y5$Pedestrian = 0
  y5$Other = 0
  
  y_id = rbind(y_id, y5)
  
  y6 = x[!(x$id %in% y_id$id),]
  y6 = sample_n(y6, nrow(x)*mode_perc$Motorcycle[1])
  y6$WorkFromHome= 0
  y6$MetroTram = 0
  y6$Train = 0
  y6$Bus = 0
  y6$Taxi = 0
  y6$Motorcycle = 1
  y6$CarDriver = 0
  y6$CarPassenger = 0
  y6$Bicycle = 0
  y6$Pedestrian = 0
  y6$Other = 0
  
  y_id = rbind(y_id, y6)
  
  y7 = x[!(x$id %in% y_id$id),]
  y7 = sample_n(y7, nrow(x)*mode_perc$CarDriver[1])
  y7$WorkFromHome= 0
  y7$MetroTram = 0
  y7$Train = 0
  y7$Bus = 0
  y7$Taxi = 0
  y7$Motorcycle = 0
  y7$CarDriver = 1
  y7$CarPassenger = 0
  y7$Bicycle = 0
  y7$Pedestrian = 0
  y7$Other = 0
  
  y_id = rbind(y_id, y7)
  
  y8 = x[!(x$id %in% y_id$id),]
  y8 = sample_n(y8, nrow(x)*mode_perc$CarPassenger[1])
  y8$WorkFromHome= 0
  y8$MetroTram = 0
  y8$Train = 0
  y8$Bus = 0
  y8$Taxi = 0
  y8$Motorcycle = 0
  y8$CarDriver = 0
  y8$CarPassenger = 1
  y8$Bicycle = 0
  y8$Pedestrian = 0
  y8$Other = 0
  
  y_id = rbind(y_id, y8)
  
  y9 = x[!(x$id %in% y_id$id),]
  y9 = sample_n(y9, nrow(x)*mode_perc$Bicycle[1])
  y9$WorkFromHome= 0
  y9$MetroTram = 0
  y9$Train = 0
  y9$Bus = 0
  y9$Taxi = 0
  y9$Motorcycle = 0
  y9$CarDriver = 0
  y9$CarPassenger = 0
  y9$Bicycle = 1
  y9$Pedestrian = 0
  y9$Other = 0
  
  y_id = rbind(y_id, y9)
  
  y10 = x[!(x$id %in% y_id$id),]
  y10 = sample_n(y10, nrow(x)*mode_perc$Pedestrian[1])
  y10$WorkFromHome= 0
  y10$MetroTram = 0
  y10$Train = 0
  y10$Bus = 0
  y10$Taxi = 0
  y10$Motorcycle = 0
  y10$CarDriver = 0
  y10$CarPassenger = 0
  y10$Bicycle = 0
  y10$Pedestrian = 1
  y10$Other = 0
  
  y_id = rbind(y_id, y10)
  
  y11 = x[!(x$id %in% y_id$id),]
  #y11 = sample_n(y11, y11$Other[1])
  y11$WorkFromHome= 0
  y11$MetroTram = 0
  y11$Train = 0
  y11$Bus = 0
  y11$Taxi = 0
  y11$Motorcycle = 0
  y11$CarDriver = 0
  y11$CarPassenger = 0
  y11$Bicycle = 0
  y11$Pedestrian = 0
  y11$Other = 1
  
  y_id = rbind(y_id, y11)
  miss_ODmode_ints_new = rbind(miss_ODmode_ints_new, y_id)
}



# Check that ids are unique
miss_ODmode_ints_new %>% 
  count(n_distinct(id))

# All zones
write.csv(OD_mode_ints_new, 'Datasets/OD/OD_mode_ints_new.csv', row.names=F )



# Aggregate based on ResWork
miss_ODmode_new_agg = miss_ODmode_ints_new %>% 
  group_by(ResWork, Workplace, Residence) %>% 
  summarise(Total = n(), WorkFromHome = sum(WorkFromHome), MetroTram = sum(MetroTram), Train=sum(Train), 
            Bus=sum(Bus), Taxi=sum(Taxi), Motorcycle = sum(Motorcycle), CarDriver = sum(CarDriver), CarPassenger = sum(CarPassenger),
            Bicycle = sum(Bicycle), Pedestrian = sum(Pedestrian), Other = sum(Other))

# Aggregate based on Workplace
miss_ODmode_new_agg_workplace = miss_ODmode_new_agg %>% 
  group_by(Workplace) %>% 
  summarise(Total = sum(Total), WorkFromHome = sum(WorkFromHome), MetroTram = sum(MetroTram), Train=sum(Train), 
            Bus=sum(Bus), Taxi=sum(Taxi), Motorcycle = sum(Motorcycle), CarDriver = sum(CarDriver), CarPassenger = sum(CarPassenger),
            Bicycle = sum(Bicycle), Pedestrian = sum(Pedestrian), Other = sum(Other))



miss_ODmode_new_agg$Total_initial = miss_ODmode$Total
miss_ODmode_new_agg$diff = miss_ODmode_new_agg$Total - miss_ODmode_new_agg$Total_initial # Difference is 0
miss_ODmode_new_agg$abs_diff = abs(miss_ODmode_new_agg$diff) # Difference is 0

miss_ODmode_new_agg = miss_ODmode_new_agg[, c(3,2,1,4:15)]


# Change column order before rbind
str(ODmode) 
ODmode = ODmode[,c(1,2,15,3:14)]

# Change column types before rbind

str(miss_ODmode_new_agg)
miss_ODmode_new_agg = data.frame(miss_ODmode_new_agg)
miss_ODmode$Residence=as.character(miss_ODmode$Residence)
miss_ODmode$Workplace=as.character(miss_ODmode$Workplace)
miss_ODmode$ResWork=as.character(miss_ODmode$ResWork)

# Bind together ODmode and miss_ODmode to have all the OD pairs with the modes
ODmode_final = rbind(ODmode, miss_ODmode_new_agg)
sum(ODmode_final$Total)

# Check now if OD_SexAge is identical with ODmode_final
identical(OD_sexage$Total, ODmode_final$Total) # True

# Aggregate based on Residence (Zonal level)
ODmode_agg_final = ODmode_final %>%
  group_by(Residence) %>%
  summarise(Total = sum(Total))

# Check the Totals per Zone
identical(ODmode_agg_final$Total, AgeEcoOccupSex$Over16) # True





write.csv(ODmode_final, 'Datasets/OD/ODmode_final.csv', row.names=F )



##### Combine Sex Age Mode ##############################################################################


OD_SexAge_Mode = inner_join(OD_sexage, ODmode_final)
write.csv(OD_SexAge_Mode, 'Datasets/OD/OD_SexAge_Mode.csv', row.names=F )



