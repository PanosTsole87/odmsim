library(dplyr)
library(sf)

###### Load required datasets #################################################################
agg_ints_OD_df = read.csv('Datasets/IPF/agg_ints_OD_df.csv')

ints_OD_df = read.csv('Datasets/IPF/ints_OD_df.csv')
ints_OD_df$idd = rep(1:nrow(ints_OD_df)) # assign an idd for each individual

ODmode_final = read.csv('Datasets/OD/ODmode_final.csv')
ODmode_final$Residence = as.character(ODmode_final$Residence)
ODmode_final$Workplace = as.character(ODmode_final$Workplace)
ODmode_final$ResWork = as.character(ODmode_final$ResWork)

r = unique(ODmode_final$Residence)

##### Algorithm for Mode imputation ###############################################
# DONT RUN IT FOR ALL THE ZONES AT ONCE!!! TOO SLOW DUE TO DATAFRAMES!!!

ints_OD_SexAge_Mode = list()

ints_OD_SexAge_Mode = read.csv('Datasets/IPF/ints_OD_SexAge_Mode.csv')

ints_OD_SexAge_Mode_aux = list()

# For example the algorithm below only loops through the last 92 zones. I did separate loops per 100 zones
for (z in 601:692){
  #z=2
  print(z)
  
  ints_OD_perzone_df = ints_OD_df[ints_OD_df$Origin==r[z],] # Subset individuals for each origin
  ODmode_final_perzone = ODmode_final[ODmode_final$Residence==r[z],]
  w = ODmode_final_perzone$Workplace
  for (y in 1:length(w)){
    for (j in 5:15){
      x = ints_OD_perzone_df[ints_OD_perzone_df$Destination == w[y],] # Subset for each destination
      x = x[which(!(x$idd %in% ints_OD_SexAge_Mode_aux$idd)), ]
      n = ODmode_final_perzone[ODmode_final_perzone$Workplace == w[y], j]
      x = sample_n(x, n)
      if (nrow(x)!=0){
        x$Mode = j-4
        ints_OD_SexAge_Mode_aux = rbind(ints_OD_SexAge_Mode_aux, x)
      } 
    }
  }
}

#CommuteAgeSex = read.csv('Datasets/Sociodemographic/CommuteAgeSex.csv')
sum(CommuteAgeSex$Over16[1:692])
ints_OD_SexAge_Mode = rbind(ints_OD_SexAge_Mode, ints_OD_SexAge_Mode_aux)
ints_OD_SexAge_Mode %>% 
  count(n_distinct(idd))
ints_OD_SexAge_Mode %>% 
  count(n_distinct(Origin))
# Until zone 692


ints_OD_SexAge_Mode$OD = paste0(ints_OD_SexAge_Mode$Origin, ints_OD_SexAge_Mode$Destination)

write.csv(ints_OD_SexAge_Mode, file = 'Datasets/IPF/ints_OD_SexAge_Mode.csv', row.names = F)

ints_OD_SexAge_Mode = read.csv('Datasets/IPF/ints_OD_SexAge_Mode.csv')

# Aggregate the constructed df
agg_ints_OD_SexAge_Mode = ints_OD_SexAge_Mode %>% 
  group_by(OD, Destination, Origin) %>% 
  summarise(Total = n(), Male = sum(Sex==1), Female = sum(Sex==2), '16-24' = sum(Age==2), '25-34' = sum(Age==3), 
            '35-44' = sum(Age==4),'45-54' = sum(Age==5), '55-64' = sum(Age==6), '65-74' = sum(Age==7), 'Over75' = sum(Age==8), 
            WorkFromHome = sum(Mode==1), MetroTram = sum(Mode==2), Train = sum(Mode==3), Bus = sum(Mode==4), 
            Taxi = sum(Mode==5), Motorcycle = sum(Mode==6),CarDriver = sum(Mode==7), CarPassenger = sum(Mode==8), 
            Bicycle = sum(Mode==9), Pedestrian = sum(Mode==10), Other = sum(Mode==11), Health_1 = sum(Health==1), Health_2 = sum(Health==2), 
            Health_3 = sum(Health==3), Health_4 = sum(Health==4), Health_5 = sum(Health==5), SG_AB = sum(Approximated.Social.Grade==1),
            SG_C1 = sum(Approximated.Social.Grade==2), SG_C2 = sum(Approximated.Social.Grade==3), SG_DE = sum(Approximated.Social.Grade==4))

write.csv(agg_ints_OD_SexAge_Mode, file = 'Datasets/IPF/agg_ints_OD_SexAge_Mode.csv', row.names = F)

agg_ints_OD_SexAge_Mode = read.csv('Datasets/IPF/agg_ints_OD_SexAge_Mode.csv')

#### Check if initial and constructed ODmode are the same ###########################################

identical(agg_ints_OD_SexAge_Mode$Total, ODmode_final$Total) # True
identical(agg_ints_OD_SexAge_Mode$WorkFromHome, ODmode_final$WorkFromHome) # True
identical(agg_ints_OD_SexAge_Mode$MetroTram, ODmode_final$MetroTram) # True

identical(agg_ints_OD_SexAge_Mode$Train, ODmode_final$Train) # True
identical(agg_ints_OD_SexAge_Mode$Bus, ODmode_final$Bus) # True
identical(agg_ints_OD_SexAge_Mode$Taxi, ODmode_final$Taxi) # True
identical(agg_ints_OD_SexAge_Mode$Motorcycle, ODmode_final$Motorcycle) # True
identical(agg_ints_OD_SexAge_Mode$CarDriver, ODmode_final$CarDriver) # True
identical(agg_ints_OD_SexAge_Mode$CarPassenger, ODmode_final$CarPassenger)
identical(agg_ints_OD_SexAge_Mode$Bicycle, ODmode_final$Bicycle) # True
identical(agg_ints_OD_SexAge_Mode$Pedestrian, ODmode_final$Pedestrian) # True
identical(agg_ints_OD_SexAge_Mode$Other, ODmode_final$Other) # True

identical(agg_ints_OD_SexAge_Mode$Male,ints_OD_agg1$Male) # True
identical(agg_ints_OD_SexAge_Mode$Female,ints_OD_agg1$Female) # True
identical(agg_ints_OD_SexAge_Mode$`16-24`,ints_OD_agg1$X16.24) # True
identical(agg_ints_OD_SexAge_Mode$`25-34`,ints_OD_agg1$X25.34) # True
identical(agg_ints_OD_SexAge_Mode$`35-44`,ints_OD_agg1$X35.44) # True
identical(agg_ints_OD_SexAge_Mode$`45-54`,ints_OD_agg1$X45.54) # True
identical(agg_ints_OD_SexAge_Mode$`55-64`,ints_OD_agg1$X55.64) # True
identical(agg_ints_OD_SexAge_Mode$`65-74`,ints_OD_agg1$X65.74) # True
identical(agg_ints_OD_SexAge_Mode$Over75,ints_OD_agg1$Over75) # True


#### Check fit statistics if they are the same as after the IPF_OD step ###############################

OD_SexAge_Mode = read.csv('Datasets/OD/OD_SexAge_Mode.csv')

identical(agg_ints_OD_SexAge_Mode$Male, OD_SexAge_Mode$Male)
agg_ints_OD_SexAge_Mode$Initial_Male = OD_SexAge_Mode$Male
agg_ints_OD_SexAge_Mode$Initial_Female = OD_SexAge_Mode$Female
agg_ints_OD_SexAge_Mode$Initial_16_24 = OD_SexAge_Mode$X16.24
agg_ints_OD_SexAge_Mode$Initial_25_34 = OD_SexAge_Mode$X25.34
agg_ints_OD_SexAge_Mode$Initial_35_44 = OD_SexAge_Mode$X35.44
agg_ints_OD_SexAge_Mode$Initial_45_54 = OD_SexAge_Mode$X45.54
agg_ints_OD_SexAge_Mode$Initial_55_64 = OD_SexAge_Mode$X55.64
agg_ints_OD_SexAge_Mode$Initial_65_74 = OD_SexAge_Mode$X65.74
agg_ints_OD_SexAge_Mode$Initial_Over75 = OD_SexAge_Mode$Over75

cor(as.numeric(ints_OD_agg1$initial_Total), as.numeric(ints_OD_agg1$Total))
tae(as.numeric(ints_OD_agg1$initial_Total), as.numeric(ints_OD_agg1$Total))/sum(ints_OD_agg1$Total)

cor(as.numeric(agg_ints_OD_SexAge_Mode$Initial_Male), as.numeric(agg_ints_OD_SexAge_Mode$Male))
tae(as.numeric(agg_ints_OD_SexAge_Mode$Initial_Male), as.numeric(agg_ints_OD_SexAge_Mode$Male))/sum(agg_ints_OD_SexAge_Mode$Male)

cor(as.numeric(agg_ints_OD_SexAge_Mode$Initial_Female), as.numeric(agg_ints_OD_SexAge_Mode$Female))
tae(as.numeric(agg_ints_OD_SexAge_Mode$Initial_Female), as.numeric(agg_ints_OD_SexAge_Mode$Female))/sum(agg_ints_OD_SexAge_Mode$Female)

cor(as.numeric(agg_ints_OD_SexAge_Mode$Initial_16_24), as.numeric(agg_ints_OD_SexAge_Mode$`16-24`))
tae(as.numeric(agg_ints_OD_SexAge_Mode$Initial_16_24), as.numeric(agg_ints_OD_SexAge_Mode$`16-24`))/sum(agg_ints_OD_SexAge_Mode$`16-24`)

cor(as.numeric(agg_ints_OD_SexAge_Mode$Initial_25_34), as.numeric(agg_ints_OD_SexAge_Mode$`25-34`))
tae(as.numeric(agg_ints_OD_SexAge_Mode$Initial_25_34), as.numeric(agg_ints_OD_SexAge_Mode$`25-34`))/sum(agg_ints_OD_SexAge_Mode$`25-34`)

cor(as.numeric(agg_ints_OD_SexAge_Mode$Initial_35_44), as.numeric(agg_ints_OD_SexAge_Mode$`35-44`))
tae(as.numeric(agg_ints_OD_SexAge_Mode$Initial_35_44), as.numeric(agg_ints_OD_SexAge_Mode$`35-44`))/sum(agg_ints_OD_SexAge_Mode$`35-44`)

cor(as.numeric(agg_ints_OD_SexAge_Mode$Initial_45_54), as.numeric(agg_ints_OD_SexAge_Mode$`45-54`))
tae(as.numeric(agg_ints_OD_SexAge_Mode$Initial_45_54), as.numeric(agg_ints_OD_SexAge_Mode$`45-54`))/sum(agg_ints_OD_SexAge_Mode$`45-54`)

cor(as.numeric(agg_ints_OD_SexAge_Mode$Initial_55_64), as.numeric(agg_ints_OD_SexAge_Mode$`55-64`))
tae(as.numeric(agg_ints_OD_SexAge_Mode$Initial_55_64), as.numeric(agg_ints_OD_SexAge_Mode$`55-64`))/sum(agg_ints_OD_SexAge_Mode$`55-64`)

cor(as.numeric(agg_ints_OD_SexAge_Mode$Initial_65_74), as.numeric(agg_ints_OD_SexAge_Mode$`65-74`))
tae(as.numeric(agg_ints_OD_SexAge_Mode$Initial_65_74), as.numeric(agg_ints_OD_SexAge_Mode$`65-74`))/sum(agg_ints_OD_SexAge_Mode$`65-74`)

cor(as.numeric(agg_ints_OD_SexAge_Mode$Initial_Over75), as.numeric(agg_ints_OD_SexAge_Mode$Over75))
tae(as.numeric(agg_ints_OD_SexAge_Mode$Initial_Over75), as.numeric(agg_ints_OD_SexAge_Mode$Over75))/sum(agg_ints_OD_SexAge_Mode$Over75)

write.csv(agg_ints_OD_SexAge_Mode, 'Datasets/IPF/agg_ints_OD_SexAge_Mode.csv', row.names=F )

agg_ints_OD_SexAge_Mode = read.csv('Datasets/IPF/agg_ints_OD_SexAge_Mode.csv')

agg_ints_OD_SexAge_Mode_intra = agg_ints_OD_SexAge_Mode[agg_ints_OD_SexAge_Mode$Destination %in% AgeEcoOccupSex$X.1,]
agg_ints_OD_SexAge_Mode_intra$Destination=as.character(agg_ints_OD_SexAge_Mode_intra$Destination)
agg_ints_OD_SexAge_Mode_intra$Origin=as.character(agg_ints_OD_SexAge_Mode_intra$Origin)

agg_ints_OD_SexAge_Mode_intra_nonendo = agg_ints_OD_SexAge_Mode_intra[agg_ints_OD_SexAge_Mode_intra$Destination!=agg_ints_OD_SexAge_Mode_intra$Origin,]

spatial_agg_ints_OD_SexAge_Mode_intra_nonendo=left_join(desire_lines_intra_nonendo, agg_ints_OD_SexAge_Mode_intra_nonendo, by=c('ResWork'='OD'))

Yorkmap = tm_shape(Yorkshire1)+
  tm_fill('LocalAuthority', legend.show = FALSE, palette = 'Blues')+
  tm_borders(alpha=.5)
Yorkmap
od_map_car=tm_shape(spatial_agg_ints_OD_SexAge_Mode_intra_nonendo[spatial_agg_ints_OD_SexAge_Mode_intra_nonendo$Workplace=='E02006875',])+
  tm_lines(lwd="CarDriver.y", scale=10, col='CarDriver.y', style = "jenks")
Yorkmap+od_map_car

od_map_bicycle=tm_shape(spatial_agg_ints_OD_SexAge_Mode_intra_nonendo[spatial_agg_ints_OD_SexAge_Mode_intra_nonendo$Workplace=='E02006875',])+
  tm_lines(lwd="Bicycle.y", scale=10, col='Bicycle.y', style = "jenks")
Yorkmap+od_map_bicycle

od_map_pedestrian=tm_shape(spatial_agg_ints_OD_SexAge_Mode_intra_nonendo[spatial_agg_ints_OD_SexAge_Mode_intra_nonendo$Workplace=='E02006875',])+
  tm_lines(lwd="Pedestrian.y", scale=10, col='Pedestrian.y', style = "jenks")
Yorkmap+od_map_pedestrian

od_map_health_1=tm_shape(spatial_agg_ints_OD_SexAge_Mode_intra_nonendo[spatial_agg_ints_OD_SexAge_Mode_intra_nonendo$Workplace=='E02006875',])+
  tm_lines(lwd="Health_1", scale=10, col='Health_1', style = "jenks")
Yorkmap+od_map_health_1

od_map_SG=tm_shape(spatial_agg_ints_OD_SexAge_Mode_intra_nonendo[spatial_agg_ints_OD_SexAge_Mode_intra_nonendo$Workplace=='E02006875',])+
  tm_lines(lwd="SG_AB", scale=10, col='SG_AB', style = "jenks")
Yorkmap+od_map_SG

od_map_SG=tm_shape(spatial_agg_ints_OD_SexAge_Mode_intra_nonendo[spatial_agg_ints_OD_SexAge_Mode_intra_nonendo$Workplace=='E02006875',])+
  tm_lines(lwd="SG_C1", scale=10, col='SG_C1', style = "jenks")
Yorkmap+od_map_SG

od_map_SG=tm_shape(spatial_agg_ints_OD_SexAge_Mode_intra_nonendo[spatial_agg_ints_OD_SexAge_Mode_intra_nonendo$Workplace=='E02006875',])+
  tm_lines(lwd="SG_C2", scale=10, col='SG_C2', style = "jenks")
Yorkmap+od_map_SG

od_map_SG=tm_shape(spatial_agg_ints_OD_SexAge_Mode_intra_nonendo[spatial_agg_ints_OD_SexAge_Mode_intra_nonendo$Workplace=='E02006875',])+
  tm_lines(lwd="SG_DE", scale=10, col='SG_DE', style = "jenks")
Yorkmap+od_map_SG


