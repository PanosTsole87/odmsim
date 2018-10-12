# Load libraries and functions
library(dplyr)
library(ggplot2)
source('Code/Chapter4/functions.R') #load the tae function

ints_df = read.csv('Datasets/IPF/ints_df.csv')
ints_agg = read.csv('Datasets/IPF/ints_agg.csv')
cons = read.csv('Datasets/Sociodemographic/cons.csv')

nrow(ints_df)
nrow(Yorkshire)
u = unique(ints_df$zone)
length(u)
summary(ints_df$Hours.worked.per.week)

source('Code/Chapter3/DataPreparation_Census.R') 
source('Code/Chapter3/HoursWorked_Occupation.R') 


ints_agg$Origin = AgeEcoOccupSex$X.1
ints_agg$initial_Total = AgeEcoOccupSex$Over16
ints_agg$Initial_Male = AgeEcoOccupSex$MaleOver16
ints_agg$Initial_Female = AgeEcoOccupSex$FemaleOver16

ints_agg$`Initial_16_24` = AgeEcoOccupSex$`16-24`
ints_agg$`Initial_25_34` = AgeEcoOccupSex$`25-34`
ints_agg$`Initial_35_44` = AgeEcoOccupSex$`35-44`
ints_agg$`Initial_45_54` = AgeEcoOccupSex$`45-54`
ints_agg$`Initial_55_64` = AgeEcoOccupSex$`55-64`
ints_agg$`Initial_65_74` = AgeEcoOccupSex$`65-74`
ints_agg$`Initial_Over75` = AgeEcoOccupSex$Over75

ints_agg$Health_1_perc = ints_agg$Health_1/ints_agg$Total
ints_agg$Health_2_perc = ints_agg$Health_2/ints_agg$Total
ints_agg$Health_3_perc = ints_agg$Health_3/ints_agg$Total
ints_agg$Health_4_perc = ints_agg$Health_4/ints_agg$Total
ints_agg$Health_5_perc = ints_agg$Health_5/ints_agg$Total

ints_agg$SG_AB_perc = ints_agg$SG_AB/ints_agg$Total
ints_agg$SG_C1_perc = ints_agg$SG_C1/ints_agg$Total
ints_agg$SG_C2_perc = ints_agg$SG_C2/ints_agg$Total
ints_agg$SG_DE_perc = ints_agg$SG_DE/ints_agg$Total

ints_agg$Initial_Occ1 = cons$Occ1
ints_agg$Initial_Occ2 = cons$Occ2
ints_agg$Initial_Occ3 = cons$Occ3
ints_agg$Initial_Occ4 = cons$Occ4
ints_agg$Initial_Occ5 = cons$Occ5
ints_agg$Initial_Occ6 = cons$Occ6
ints_agg$Initial_Occ7 = cons$Occ7
ints_agg$Initial_Occ8 = cons$Occ8
ints_agg$Initial_Occ9 = cons$Occ9

##### Internal Validation #####
# Zonal totals plot
ggplot(ints_agg, aes(x=Total, y=initial_Total)) + geom_point(aes(size=Total), color="blue", alpha = 0.2 )+
  labs(x="Real zonal totals", y = "Simulated zonal totals")+theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)

# Zonal totals Fit Statistics
c1=round(cor(as.numeric(ints_agg$initial_Total), as.numeric(ints_agg$Total)), 3)
re1=round(tae(as.numeric(ints_agg$initial_Total), as.numeric(ints_agg$Total))/sum(ints_agg$Total), 3)

# Gender Fit Statistics
c2=round(cor(as.numeric(ints_agg$Initial_Male), as.numeric(ints_agg$Male)), 3)
re2=round(tae(as.numeric(ints_agg$Initial_Male), as.numeric(ints_agg$Male))/sum(ints_agg$Male), 3)

c3=round(cor(as.numeric(ints_agg$Initial_Female), as.numeric(ints_agg$Female)), 3)
re3=round(tae(as.numeric(ints_agg$Initial_Female), as.numeric(ints_agg$Female))/sum(ints_agg$Female), 3)

# Age Fit Statistics
c4=round(cor(as.numeric(ints_agg$Initial_16_24), as.numeric(ints_agg$X16_24)), 3)
re4=round(tae(as.numeric(ints_agg$Initial_16_24), as.numeric(ints_agg$X16_24))/sum(ints_agg$X16_24), 3)

c5=round(cor(as.numeric(ints_agg$Initial_25_34), as.numeric(ints_agg$X25.34)), 3)
re5=round(tae(as.numeric(ints_agg$Initial_25_34), as.numeric(ints_agg$X25.34))/sum(ints_agg$X25.34), 3)

c6=round(cor(as.numeric(ints_agg$Initial_35_44), as.numeric(ints_agg$X35_44)), 3)
re6=round(tae(as.numeric(ints_agg$Initial_35_44), as.numeric(ints_agg$X35_44))/sum(ints_agg$X35_44), 3)

c7=round(cor(as.numeric(ints_agg$Initial_45_54), as.numeric(ints_agg$X45.54)), 3)
re7=round(tae(as.numeric(ints_agg$Initial_45_54), as.numeric(ints_agg$X45.54))/sum(ints_agg$X45.54), 3)

c8=round(cor(as.numeric(ints_agg$Initial_55_64), as.numeric(ints_agg$X55.64)), 3)
re8=round(tae(as.numeric(ints_agg$Initial_55_64), as.numeric(ints_agg$X55.64))/sum(ints_agg$X55.64), 3)

c9=round(cor(as.numeric(ints_agg$Initial_65_74), as.numeric(ints_agg$X65.74)), 3)
re9=round(tae(as.numeric(ints_agg$Initial_65_74), as.numeric(ints_agg$X65.74))/sum(ints_agg$X65.74), 3)

c10=round(cor(as.numeric(ints_agg$Initial_Over75), as.numeric(ints_agg$Over75)), 3)
re10=round(tae(as.numeric(ints_agg$Initial_Over75), as.numeric(ints_agg$Over75))/sum(ints_agg$Over75), 3)

sum(ints_agg$Initial_Over75) # Small total number

# Occupation Fit Statistics
c11=round(cor(as.numeric(ints_agg$Initial_Occ1), as.numeric(ints_agg$Occ1)), 3)
re11=round(tae(as.numeric(ints_agg$Initial_Occ1), as.numeric(ints_agg$Occ1))/sum(ints_agg$Occ1), 3)

c12=round(cor(as.numeric(ints_agg$Initial_Occ2), as.numeric(ints_agg$Occ2)), 3)
re12=round(tae(as.numeric(ints_agg$Initial_Occ2), as.numeric(ints_agg$Occ2))/sum(ints_agg$Occ2), 3)

c13=round(cor(as.numeric(ints_agg$Initial_Occ3), as.numeric(ints_agg$Occ3)), 3)
re13=round(tae(as.numeric(ints_agg$Initial_Occ3), as.numeric(ints_agg$Occ3))/sum(ints_agg$Occ3), 3)

c14=round(cor(as.numeric(ints_agg$Initial_Occ4), as.numeric(ints_agg$Occ4)), 3)
re14=round(tae(as.numeric(ints_agg$Initial_Occ4), as.numeric(ints_agg$Occ4))/sum(ints_agg$Occ4), 3)

c15=round(cor(as.numeric(ints_agg$Initial_Occ5), as.numeric(ints_agg$Occ5)), 3)
re15=round(tae(as.numeric(ints_agg$Initial_Occ5), as.numeric(ints_agg$Occ5))/sum(ints_agg$Occ5), 3)

c16=round(cor(as.numeric(ints_agg$Initial_Occ6), as.numeric(ints_agg$Occ6)), 3)
re16=round(tae(as.numeric(ints_agg$Initial_Occ6), as.numeric(ints_agg$Occ6))/sum(ints_agg$Occ6), 3)

c17=round(cor(as.numeric(ints_agg$Initial_Occ7), as.numeric(ints_agg$Occ7)), 3)
re17=round(tae(as.numeric(ints_agg$Initial_Occ7), as.numeric(ints_agg$Occ7))/sum(ints_agg$Occ7), 3)

c18=round(cor(as.numeric(ints_agg$Initial_Occ8), as.numeric(ints_agg$Occ8)), 3)
re18=round(tae(as.numeric(ints_agg$Initial_Occ8), as.numeric(ints_agg$Occ8))/sum(ints_agg$Occ8), 3)

c19=round(cor(as.numeric(ints_agg$Initial_Occ9), as.numeric(ints_agg$Occ9)), 3)
re19=round(tae(as.numeric(ints_agg$Initial_Occ9), as.numeric(ints_agg$Occ9))/sum(ints_agg$Occ9), 3)

# Create df with fit stats
Variable = c('Zone totals', 'Gender_Male', 'Gender_Female', 'Age16-24', 'Age25-34', 'Age35-44', 'Age45-54', 'Age55-64', 'Age65-74', 'AgeOver75', 'Occ1', 'Occ2', 'Occ3', 'Occ4', 'Occ5', 'Occ6', 'Occ7', 'Occ8', 'Occ9')

Pearson_correlation = c(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19)

Relative_Error = c(re1, re2, re3, re4, re5, re6, re7, re8, re9, re10, re11, re12, re13, re14, re15, re16, re17, re18, re19)

IPF_fitStats_df = cbind(Variable, Pearson_correlation, Relative_Error) 
IPF_fitStats_df = data.frame(IPF_fitStats_df)

# Gender plots
ggplot(ints_agg, aes(x=Male, y=Initial_Male)) + geom_point(aes(size=Male), color="blue", alpha = 0.2 )+
  labs(x="Real Male totals", y = "Simulated Male totals")+theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)

ggplot(ints_agg, aes(x=Female, y=Initial_Female)) + geom_point(aes(size=Female), color="blue", alpha = 0.2 )+
  labs(x="Real Female totals", y = "Simulated Female totals")+theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)


# Age plots
p=ggplot(ints_agg, aes(x=`X16_24`, y=Initial_16_24)) + geom_point(aes(size=`X16_24`), color="blue", alpha = 0.2 )+
  labs(x="Real 16-24 totals", y = "Simulated 16-24 totals")+theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "16-24 \nyears old")

p=ggplot(ints_agg, aes(x=`X25.34`, y=Initial_25_34)) + geom_point(aes(size=`X25.34`), color="blue", alpha = 0.2 )+
  labs(x="Real 25-34 totals", y = "Simulated 25-34 totals")+theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "25-34 \nyears old")

p=ggplot(ints_agg, aes(x=`X35_44`, y=Initial_35_44)) + geom_point(aes(size=`X35_44`), color="blue", alpha = 0.2 )+
  labs(x="Real 35-44 totals", y = "Simulated 35-44 totals")+theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "35-44 \nyears old")

p=ggplot(ints_agg, aes(x=`X45.54`, y=Initial_45_54)) + geom_point(aes(size=`X45.54`), color="blue", alpha = 0.2 )+
  labs(x="Real 45-54 totals", y = "Simulated 45-54 totals")+theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "45-54 \nyears old")

p=ggplot(ints_agg, aes(x=`X55.64`, y=Initial_55_64)) + geom_point(aes(size=`X55.64`), color="blue", alpha = 0.2 )+
  labs(x="Real 55-64 totals", y = "Simulated 55-64 totals")+theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "55-64 \nyears old")

p=ggplot(ints_agg, aes(x=`X65.74`, y=Initial_65_74)) + geom_point(aes(size=`X65.74`), color="blue", alpha = 0.2 )+
  labs(x="Real 65-74 totals", y = "Simulated 65-74 totals")+theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "65-74 \nyears old")

p=ggplot(ints_agg, aes(x=`Over75`, y=Initial_Over75)) + geom_point(aes(size=`Over75`), color="blue", alpha = 0.2 )+
  labs(x="Real Over 75 totals", y = "Simulated Over 75 totals")+
  theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "Over 75 \nyears old")

# Occupation plots
p=ggplot(ints_agg, aes(x=`Occ1`, y=Initial_Occ1)) + geom_point(aes(size=`Occ1`), color="blue", alpha = 0.2 )+
  labs(x="Real Occupation 1 totals", y = "Simulated Occupation 1 totals")+
  theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "Occupation 1")

p=ggplot(ints_agg, aes(x=`Occ2`, y=Initial_Occ2)) + geom_point(aes(size=`Occ2`), color="blue", alpha = 0.2 )+
  labs(x="Real Occupation 2 totals", y = "Simulated Occupation 2 totals")+
  theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "Occupation 2")

p=ggplot(ints_agg, aes(x=`Occ3`, y=Initial_Occ3)) + geom_point(aes(size=`Occ3`), color="blue", alpha = 0.2 )+
  labs(x="Real Occupation 3 totals", y = "Simulated Occupation 3 totals")+
  theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "Occupation 3")

p=ggplot(ints_agg, aes(x=`Occ4`, y=Initial_Occ4)) + geom_point(aes(size=`Occ4`), color="blue", alpha = 0.2 )+
  labs(x="Real Occupation 4 totals", y = "Simulated Occupation 4 totals")+
  theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "Occupation 4")

p=ggplot(ints_agg, aes(x=`Occ5`, y=Initial_Occ5)) + geom_point(aes(size=`Occ5`), color="blue", alpha = 0.2 )+
  labs(x="Real Occupation 5 totals", y = "Simulated Occupation 5 totals")+
  theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "Occupation 5")

p=ggplot(ints_agg, aes(x=`Occ6`, y=Initial_Occ6)) + geom_point(aes(size=`Occ6`), color="blue", alpha = 0.2 )+
  labs(x="Real Occupation 6 totals", y = "Simulated Occupation 6 totals")+
  theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "Occupation 6")

p=ggplot(ints_agg, aes(x=`Occ7`, y=Initial_Occ7)) + geom_point(aes(size=`Occ7`), color="blue", alpha = 0.2 )+
  labs(x="Real Occupation 7 totals", y = "Simulated Occupation 7 totals")+
  theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "Occupation 7")

p=ggplot(ints_agg, aes(x=`Occ8`, y=Initial_Occ8)) + geom_point(aes(size=`Occ8`), color="blue", alpha = 0.2 )+
  labs(x="Real Occupation 8 totals", y = "Simulated Occupation 8 totals")+
  theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "Occupation 8")

p=ggplot(ints_agg, aes(x=`Occ9`, y=Initial_Occ9)) + geom_point(aes(size=`Occ9`), color="blue", alpha = 0.2 )+
  labs(x="Real Occupation 9 totals", y = "Simulated Occupation 9 totals")+
  theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "Occupation 9")





##### External Validation #####

ints_agg$Initial_HW1 = HoursWorked$HW1
ints_agg$Initial_HW2 = HoursWorked$HW2
ints_agg$Initial_HW3 = HoursWorked$HW3
ints_agg$Initial_HW4 = HoursWorked$HW4

ext_c1 = round(cor(as.numeric(ints_agg$Initial_HW1), as.numeric(ints_agg$HW_1)), 3)
ext_re1=round(tae(as.numeric(ints_agg$Initial_HW1), as.numeric(ints_agg$HW_1))/sum(ints_agg$HW_1), 3)
sum(ints_agg$HW_1)
sum(ints_agg$Initial_HW1)

ext_c2=round(cor(as.numeric(ints_agg$Initial_HW2), as.numeric(ints_agg$HW_2)), 3)
ext_re2=round(tae(as.numeric(ints_agg$Initial_HW2), as.numeric(ints_agg$HW_2))/sum(ints_agg$HW_2), 3)
sum(ints_agg$HW_2)
sum(ints_agg$Initial_HW2)

ext_c3=round(cor(as.numeric(ints_agg$Initial_HW3), as.numeric(ints_agg$HW_3)), 3)
ext_re3=round(tae(as.numeric(ints_agg$Initial_HW3), as.numeric(ints_agg$HW_3))/sum(ints_agg$HW_3), 3)
sum(ints_agg$HW_3)
sum(ints_agg$Initial_HW3)

ext_c4=round(cor(as.numeric(ints_agg$Initial_HW4), as.numeric(ints_agg$HW_4)), 3)
ext_re4=round(tae(as.numeric(ints_agg$Initial_HW4), as.numeric(ints_agg$HW_4))/sum(ints_agg$HW_4),3)
sum(ints_agg$HW_4)
sum(ints_agg$Initial_HW4)


# Create df with fit stats for External Validation
ext_Variable = c('HoursWorked_1', 'HoursWorked_2', 'HoursWorked_3', 'HoursWorked_4')

ext_Pearson_correlation = c(ext_c1, ext_c2, ext_c3, ext_c4)

ext_Relative_Error = c(ext_re1, ext_re2, ext_re3, ext_re4)

ext_fitStats_df = cbind(ext_Variable, ext_Pearson_correlation, ext_Relative_Error) 
ext_fitStats_df = data.frame(ext_fitStats_df)

colnames(ext_fitStats_df) = c('Variable', 'Pearson_correlation', 'Relative_Error') # Change colnames


p=ggplot(ints_agg, aes(x=`HW_1`, y=Initial_HW1)) + geom_point(aes(size=`HW_1`), color="blue", alpha = 0.2 )+
  labs(x="Real Hours worked per week 1 totals", y = "Simulated Hours worked per week 1 totals")+
  theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "Hours worked \nper week 1")

p=ggplot(ints_agg, aes(x=`HW_2`, y=Initial_HW2)) + geom_point(aes(size=`HW_2`), color="blue", alpha = 0.2 )+
  labs(x="Real Hours worked per week 2 totals", y = "Simulated Hours worked per week 2 totals")+
  theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "Hours worked \nper week 2")

p=ggplot(ints_agg, aes(x=`HW_3`, y=Initial_HW3)) + geom_point(aes(size=`HW_3`), color="blue", alpha = 0.2 )+
  labs(x="Real Hours worked per week 3 totals", y = "Simulated Hours worked per week 3 totals")+
  theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "Hours worked \nper week 3")

p=ggplot(ints_agg, aes(x=`HW_4`, y=Initial_HW4)) + geom_point(aes(size=`HW_4`), color="blue", alpha = 0.2 )+
  labs(x="Real Hours worked per week 4 totals", y = "Simulated Hours worked per week 4 totals")+
  theme(text = element_text(size=15))+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed", size=1.0)
p + labs(size= "Hours worked \nper week 4")


# Maps of simulated Social Grade levels
ints_agg$Area = AgeEcoOccupSex$X.1 # new column with Zone codes
Simulated_Yorkshire = left_join(Yorkshire1, ints_agg, by=c('AREA_ID'='Area')) # Join sf with df
#str(Simulated_Yorkshire) # Class sf

tm_shape(Simulated_Yorkshire)+
  tm_fill('SG_AB_perc', palette = "Blues", title = 'Social Grade A-B percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()

tm_shape(Simulated_Yorkshire)+
  tm_fill('SG_C1_perc', palette = "Blues", title = 'Social Grade C1 percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()

tm_shape(Simulated_Yorkshire)+
  tm_fill('SG_C2_perc', palette = "Blues", title = 'Social Grade C2 percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()

tm_shape(Simulated_Yorkshire)+
  tm_fill('SG_DE_perc', palette = "Blues", title = 'Social Grade D-E percentage')+
  tm_borders(alpha=.5)+
  tm_compass(position = c('left', 'top'))+
  tm_scale_bar()



