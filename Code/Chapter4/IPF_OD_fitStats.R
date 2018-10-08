# Load libraries
library(dplyr)
library(tmap)
library(tmaptools)
source('functions.R')
library(stplanr)

ints_OD_agg1 = read.csv('Datasets/IPF/ints_OD_agg1.csv')

##### Fit statistics #########################################################


OD_c1=round(cor(as.numeric(ints_OD_agg1$initial_Total), as.numeric(ints_OD_agg1$Total)), 3)
OD_re1=round(tae(as.numeric(ints_OD_agg1$initial_Total), as.numeric(ints_OD_agg1$Total))/sum(ints_OD_agg1$Total), 3)

OD_c2=round(cor(as.numeric(ints_OD_agg1$Initial_Male), as.numeric(ints_OD_agg1$Male)), 3)
OD_re2=round(tae(as.numeric(ints_OD_agg1$Initial_Male), as.numeric(ints_OD_agg1$Male))/sum(ints_OD_agg1$Male), 3)

OD_c3=round(cor(as.numeric(ints_OD_agg1$Initial_Female), as.numeric(ints_OD_agg1$Female)), 3)
OD_re3=round(tae(as.numeric(ints_OD_agg1$Initial_Female), as.numeric(ints_OD_agg1$Female))/sum(ints_OD_agg1$Female), 3)

OD_c4=round(cor(as.numeric(ints_OD_agg1$Initial_16_24), as.numeric(ints_OD_agg1$X16.24)), 3)
OD_re4=round(tae(as.numeric(ints_OD_agg1$Initial_16_24), as.numeric(ints_OD_agg1$X16.24))/sum(ints_OD_agg1$X16.24), 3)

OD_c5=round(cor(as.numeric(ints_OD_agg1$Initial_25_34), as.numeric(ints_OD_agg1$X25.34)), 3)
OD_re5=round(tae(as.numeric(ints_OD_agg1$Initial_25_34), as.numeric(ints_OD_agg1$X25.34))/sum(ints_OD_agg1$X25.34), 3)

OD_c6=round(cor(as.numeric(ints_OD_agg1$Initial_35_44), as.numeric(ints_OD_agg1$X35.44)), 3)
OD_re6=round(tae(as.numeric(ints_OD_agg1$Initial_35_44), as.numeric(ints_OD_agg1$X35.44))/sum(ints_OD_agg1$X35.44), 3)

OD_c7=round(cor(as.numeric(ints_OD_agg1$Initial_45_54), as.numeric(ints_OD_agg1$X45.54)), 3)
OD_re7=round(tae(as.numeric(ints_OD_agg1$Initial_45_54), as.numeric(ints_OD_agg1$X45.54))/sum(ints_OD_agg1$X45.54), 3)

OD_c8=round(cor(as.numeric(ints_OD_agg1$Initial_55_64), as.numeric(ints_OD_agg1$X55.64)), 3)
OD_re8=round(tae(as.numeric(ints_OD_agg1$Initial_55_64), as.numeric(ints_OD_agg1$X55.64))/sum(ints_OD_agg1$X55.64), 3)

OD_c9=round(cor(as.numeric(ints_OD_agg1$Initial_65_74), as.numeric(ints_OD_agg1$X65.74)), 3)
OD_re9=round(tae(as.numeric(ints_OD_agg1$Initial_65_74), as.numeric(ints_OD_agg1$X65.74))/sum(ints_OD_agg1$X65.74), 3)

OD_c10=round(cor(as.numeric(ints_OD_agg1$Initial_Over75), as.numeric(ints_OD_agg1$Over75)), 3)
OD_re10=round(tae(as.numeric(ints_OD_agg1$Initial_Over75), as.numeric(ints_OD_agg1$Over75))/sum(ints_OD_agg1$Over75), 3)


# Create df with fit stats 
OD_Variable = c('OD zonal totals', 'Gender_Male', 'Gender_Female', 'Age16-24', 'Age25-34', 'Age35-44', 'Age45-54', 'Age55-64', 'Age65-74', 'AgeOver75')

OD_Pearson_correlation = c(OD_c1, OD_c2, OD_c3, OD_c4, OD_c5, OD_c6, OD_c7, OD_c8, OD_c9, OD_c10)

OD_Relative_Error = c(OD_re1, OD_re2, OD_re3, OD_re4, OD_re5, OD_re6, OD_re7, OD_re8, OD_re9, OD_re10)

OD_fitStats_df = cbind(OD_Variable, OD_Pearson_correlation, OD_Relative_Error) 
OD_fitStats_df = data.frame(OD_fitStats_df)


##### Correlation plots ####################################################

# Plot for Totals
ggplot(ints_OD_agg1, aes(x=Total, y=initial_Total)) + geom_point(aes(size=Total), color="blue", alpha = 0.2 )+
  labs(x="Real OD totals", y = "Simulated OD totals")+theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)

# Plots for Gender
ggplot(ints_OD_agg1, aes(x=Male, y=Initial_Male)) + geom_point(aes(size=Male), color="blue", alpha = 0.2 )+
  labs(x="Real Male OD totals", y = "Simulated Male OD totals")+theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)

ggplot(ints_OD_agg1, aes(x=Female, y=Initial_Female)) + geom_point(aes(size=Female), color="blue", alpha = 0.2 )+
  labs(x="Real Female OD totals", y = "Simulated Female OD totals")+theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)

# Plots for Age
p=ggplot(ints_OD_agg1, aes(x=`16-24`, y=Initial_16_24)) + geom_point(aes(size=`16-24`), color="blue", alpha = 0.2 )+
  labs(x="Real 16-24 OD totals", y = "Simulated 16-24 OD totals")+theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "16-24 \nyears old")

p=ggplot(ints_OD_agg1, aes(x=`25-34`, y=Initial_25_34)) + geom_point(aes(size=`25-34`), color="blue", alpha = 0.2 )+
  labs(x="Real 25-34 OD totals", y = "Simulated 25-34 OD totals")+theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "25-34 \nyears old")

ggplot(ints_OD_agg1, aes(x=`35-44`, y=Initial_35_44)) + geom_point(aes(size=`35-44`), color="blue", alpha = 0.2 )+
  labs(x="Real 35-44 OD totals", y = "Simulated 35-44 OD totals")+theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "35-44 \nyears old")

p=ggplot(ints_OD_agg1, aes(x=`45-54`, y=Initial_45_54)) + geom_point(aes(size=`45-54`), color="blue", alpha = 0.2 )+
  labs(x="Real 45-54 OD totals", y = "Simulated 45-54 OD totals")+theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "45-54 \nyears old")

p=ggplot(ints_OD_agg1, aes(x=`55-64`, y=Initial_55_64)) + geom_point(aes(size=`55-64`), color="blue", alpha = 0.2 )+
  labs(x="Real 55-64 OD totals", y = "Simulated 55-64 OD totals")+theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "55-64 \nyears old")

p=ggplot(ints_OD_agg1, aes(x=`65-74`, y=Initial_65_74)) + geom_point(aes(size=`65-74`), color="blue", alpha = 0.2 )+
  labs(x="Real 65-74 OD totals", y = "Simulated 65-74 OD totals")+theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "65-74 \nyears old")

p=ggplot(ints_OD_agg1, aes(x=`Over75`, y=Initial_Over75)) + geom_point(aes(size=`Over75`), color="blue", alpha = 0.2 )+
  labs(x="Real Over 75 OD totals", y = "Simulated Over 75 OD totals")+theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "Over 75 \nyears old")



##### Maps ###############################################################################

ints_OD_agg1_intra_nonendo = ints_OD_agg1[ints_OD_agg1$OD %in% desire_lines_intra_nonendo$ResWork,]
spatial_ints_OD_agg1_intra_nonendo = left_join(desire_lines_intra_nonendo, ints_OD_agg1_intra_nonendo, 
                                               by=c('ResWork'='OD'))
spatial_ints_OD_agg1_intra_nonendo = spatial_ints_OD_agg1_intra_nonendo[,-c(3:5,7:25)]
spatial_ints_OD_agg1_intra_nonendo = spatial_ints_OD_agg1_intra_nonendo[,-4]



Destinations_OD_sexage = ints_OD_agg1 %>% 
  group_by(Destination) %>% 
  summarise(Total=sum(Total), Male = sum(Male), Female = sum(Female), `16-24` = sum(X16.24), `25-34` = sum(X25.34), 
            `35-44` = sum(X35.44),`45-54` = sum(X45.54), `55-64` = sum(X55.64), `65-74` = sum(X65.74), `Over75` = sum(Over75), 
            Health_1 = sum(Health_1), Health_2 = sum(Health_2), Health_3 = sum(Health_3), Health_4 = sum(Health_4), Health_5 = sum(Health_5),
            HW_1 = sum(HW_1), HW_2 = sum(HW_2), HW_3 = sum(HW_3), HW_4 = sum(HW_4), 
            SG_AB = sum(SG_AB), SG_C1 = sum(SG_C1), SG_C2 = sum(SG_C2), SG_DE = sum(SG_DE))

Destinations_OD_sexage = Destinations_OD_sexage[Destinations_OD_sexage$Destination %in% AgeEcoOccupSex$X.1,]

Destination_OD_sexage_spatial = left_join(Yorkshire1, Destinations_OD_sexage, by=c('AREA_ID'='Destination'))

Destination_OD_sexage_spatial$Health_1_perc = Destination_OD_sexage_spatial$Health_1/Destination_OD_sexage_spatial$Total
Destination_OD_sexage_spatial$Health_2_perc = Destination_OD_sexage_spatial$Health_2/Destination_OD_sexage_spatial$Total
Destination_OD_sexage_spatial$Health_3_perc = Destination_OD_sexage_spatial$Health_3/Destination_OD_sexage_spatial$Total
Destination_OD_sexage_spatial$Health_4_perc = Destination_OD_sexage_spatial$Health_4/Destination_OD_sexage_spatial$Total
Destination_OD_sexage_spatial$Health_5_perc = Destination_OD_sexage_spatial$Health_5/Destination_OD_sexage_spatial$Total

Destination_OD_sexage_spatial$SG_AB_perc = Destination_OD_sexage_spatial$SG_AB/Destination_OD_sexage_spatial$Total
Destination_OD_sexage_spatial$SG_C1_perc = Destination_OD_sexage_spatial$SG_C1/Destination_OD_sexage_spatial$Total
Destination_OD_sexage_spatial$SG_C2_perc = Destination_OD_sexage_spatial$SG_C2/Destination_OD_sexage_spatial$Total
Destination_OD_sexage_spatial$SG_DE_perc = Destination_OD_sexage_spatial$SG_DE/Destination_OD_sexage_spatial$Total

######## Attracted Social Grade mapping
tm_shape(Destination_OD_sexage_spatial)+
  tm_fill('SG_AB_perc', palette = "Blues", title = 'Percentage of attracted individuals with Social Grade A-B')+
  tm_borders(alpha=.5)+
  tm_scale_bar()

tm_shape(Destination_OD_sexage_spatial)+
  tm_fill('SG_C1_perc', palette = "Blues", title = 'Percentage of attracted individuals with Social Grade C1')+
  tm_borders(alpha=.5)+
  tm_scale_bar()

tm_shape(Destination_OD_sexage_spatial)+
  tm_fill('SG_C2_perc', palette = "Blues", title = 'Percentage of attracted individuals with Social Grade C2')+
  tm_borders(alpha=.5)+
  tm_scale_bar()

tm_shape(Destination_OD_sexage_spatial)+
  tm_fill('SG_DE_perc', palette = "Blues", title = 'Percentage of attracted individuals with Social Grade D-E')+
  tm_borders(alpha=.5)+
  tm_scale_bar()

######## Attracted Health mapping - NOT PRESENTED IN THE REPORT ###################

tm_shape(Destination_OD_sexage_spatial)+
  tm_fill('Health_1_perc', palette = "Blues", title = 'Percentage of individuals attracted with Health lvl1')+
  tm_borders(alpha=.5)+
  tm_scale_bar()

tm_shape(Destination_OD_sexage_spatial)+
  tm_fill('Health_2_perc', palette = "Blues", title = 'Percentage of individuals attracted with Health lvl2')+
  tm_borders(alpha=.5)+
  tm_scale_bar()

tm_shape(Destination_OD_sexage_spatial)+
  tm_fill('Health_3_perc', palette = "Blues", title = 'Percentage of individuals attracted with Health lvl3')+
  tm_borders(alpha=.5)+
  tm_scale_bar()

tm_shape(Destination_OD_sexage_spatial)+
  tm_fill('Health_4_perc', palette = "Blues", title = 'Percentage of individuals attracted with Health lvl4')+
  tm_borders(alpha=.5)+
  tm_scale_bar()

tm_shape(Destination_OD_sexage_spatial)+
  tm_fill('Health_5_perc', palette = "Blues", title = 'Percentage of individuals attracted with Health lvl5')+
  tm_borders(alpha=.5)+
  tm_scale_bar()