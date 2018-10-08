# Load libraries
library(ipfp)
library(dplyr)
source('functions.R')

##### Prepare the data for IPF ###################################################

YorkMicro_employed = read.csv('Datasets/Microdata/YorkMicro_employed.csv')

ind = read.csv('Datasets/Microdata/YorkMicro_employed.csv')
ind$Sex = as.factor(ind$Sex)
ind$Age = as.factor(ind$Age)
ind$Occupation = as.factor(ind$Occupation)

# Select required columns for individuals
ind = ind %>% 
  select(Sex, Age, Occupation)

con_age_sex = read.csv('Datasets/Sociodemographic/CommuteAgeSex.csv')


con_age_sex = select(con_age_sex, 12:25)
sum(con_age_sex)

write.csv(con_age_sex, file = 'Datasets/Sociodemographic/con_age_sex.csv', row.names = F)
con_age_sex = read.csv('Datasets/Sociodemographic/con_age_sex.csv')

con_occup = read.csv('Datasets/Sociodemographic/con_occup.csv')


plot(colSums(con_age_sex))
range(con_age_sex) # range of values - there are zeros
con_age_sex[con_age_sex==0] = 0.0000000001 # replace 0s with a small value

mean(rowSums(con_age_sex)) # average number of individuals in each zone

range(con_occup) # range of values - there are no zeros
mean(rowSums(con_occup)) # average number of individuals in each zone

cons = cbind(con_age_sex, con_occup)
write.csv(cons, 'Datasets/Sociodemographic/cons.csv', row.names = FALSE)
# create new age/sex variable
AS <- paste0(ind$Sex, ind$Age)
unique(AS)

# matrix for constraint 1 - age/sex
cat_AS <- model.matrix(~AS-1)
cat_occup = model.matrix(~ind$Occupation-1)
m = cbind(cat_AS, cat_occup)
head(cons)
head(m)
dim(cons)
dim(m)

colnames(m) <- names(cons)
head(m)
summary(rowSums(m))

ind_cat <- data.frame(m)
str(ind_cat)

n_ind_cat = nrow(ind_cat)
x0 = rep(1, n_ind_cat)
ind_catt = t(ind_cat)
cons = apply(cons, 2, as.numeric)
str(cons)

###### Run IPF ---Weight assignment (real numbers) #############################################

weights = apply(cons, 1, FUN = function(x)
  ipfp(x, ind_catt, x0, maxit = 20, verbose = T))
range(weights)
write.csv(weights, file = 'Datasets/IPF/weights.csv', row.names = F)
weights = read.csv('Datasets/IPF/weights.csv')
range(weights)
which(weights==max(weights), arr.ind = T)

# Calculate individuals per zone
ind_agg = t(apply(weights, 2, function(x) colSums(x*ind_cat)))
colnames(ind_agg) = colnames(cons)
write.csv(ind_agg, file = 'Datasets/IPF/ind_agg.csv', row.names = F)
ind_agg = read.csv('Datasets/IPF/ind_agg.csv')
ind_agg[3,]
cons[3,]
cor(as.numeric(cons), as.numeric(ind_agg))
max(abs(ind_agg-cons))
which(abs(ind_agg-cons)==max(abs(ind_agg-cons)), arr.ind = T)

plot(ind_agg, cons)
# Complete Fit Statistics are presented in 'Code/Chapter3/IPF_fitStats.R'

###### Integerisation with trs algorithm #######################################################

set.seed(0)
ints = apply(weights, 2, int_trs)
indices = NULL
ints = for(i in 1:ncol(ints)){
  indices = c(indices, int_expand_vector(ints[, i]))
}

ints_df = data.frame(id = indices)
zone = rep(1:nrow(cons), AgeEcoOccupSex$Over16)
ints_df$zone = zone


##### Final Step: Spatial Microdata ###########################################################

# Join individual microdata with expanded interegised individuals 
YorkMicro_employed$id = rep(1:nrow(YorkMicro_employed))
ints_df = inner_join(ints_df, YorkMicro_employed)
ints_df[ints_df$zone==2,]

head(ints_df)
class(ints_df$Age)


tae(cons, ind_agg)
tae(cons, ind_agg)/sum(cons)
ind_agg = data.frame(ind_agg)

# Numeric Hours Worked
ints_df$HW_numeric = 0
ints_df$HW_numeric[ints_df$Hours.worked.per.week == 1]=7.5
ints_df$HW_numeric[ints_df$Hours.worked.per.week == 2]=(16+30)/2
ints_df$HW_numeric[ints_df$Hours.worked.per.week == 3]=(31+48)/2
ints_df$HW_numeric[ints_df$Hours.worked.per.week == 4]=60

# Numeric Age
ints_df$Age_numeric = 0
ints_df$Age_numeric[ints_df$Age == 2]=(16+24)/2
ints_df$Age_numeric[ints_df$Age == 3]=(25+34)/2
ints_df$Age_numeric[ints_df$Age == 4]=(35+44)/2
ints_df$Age_numeric[ints_df$Age == 5]=(45+54)/2
ints_df$Age_numeric[ints_df$Age == 6]=(55+64)/2
ints_df$Age_numeric[ints_df$Age == 7]=(65+74)/2
ints_df$Age_numeric[ints_df$Age == 8]=80


# Aggregate integerised individuals based on their Origin zone (Residence place)
ints_agg = group_by(ints_df, zone) %>% 
  summarise(Total=n(), Male=sum(Sex==1), Female=sum(Sex==2), '16_24' = sum(Age==2), '25-34' = sum(Age==3), '35_44' = sum(Age==4),
            '45-54' = sum(Age==5), '55-64' = sum(Age==6), '65-74' = sum(Age==7), 'Over75' = sum(Age==8), 'Occ1' = sum(Occupation==1), 
            'Occ2' = sum(Occupation==2), 'Occ3' = sum(Occupation==3), 'Occ4' = sum(Occupation==4), 'Occ5' = sum(Occupation==5), 
            'Occ6' = sum(Occupation==6), 'Occ7' = sum(Occupation==7), 'Occ8' = sum(Occupation==8), 'Occ9' = sum(Occupation==9), 
            avWork = mean(HW_numeric), avAge = mean(Age_numeric), 
            Health_1 = sum(Health==1), Health_2 = sum(Health==2), Health_3 = sum(Health==3), Health_4 = sum(Health==4), Health_5 = sum(Health==5), 
            HW_1 = sum(Hours.worked.per.week==1), HW_2 = sum(Hours.worked.per.week==2), HW_3 = sum(Hours.worked.per.week==3), HW_4 = sum(Hours.worked.per.week==4), 
            SG_AB = sum(Approximated.Social.Grade==1), SG_C1 = sum(Approximated.Social.Grade==2), SG_C2 = sum(Approximated.Social.Grade==3), SG_DE = sum(Approximated.Social.Grade==4))

write.csv(ints_agg, file = 'Datasets/IPF/ints_agg.csv', row.names = F)

# Set the Origins for ints_df
ints_df$Origin = rep(AgeEcoOccupSex$X.1,ints_agg$Total)
# Save ints_df
write.csv(ints_df, file = 'Datasets/IPF/ints_df.csv', row.names = F)

#ints_df = read.csv('Datasets/IPF/ints_df.csv')
#ints_agg = read.csv('Datasets/IPF/ints_agg.csv')


##### Create Percentages #################################################################

head = ind_agg[353,]
percentages = ind_agg
percentages$Male16.24 = ind_agg$Male16.24/(ind_agg$Male16.24+ind_agg$Male25.34+ind_agg$Male35.44+ind_agg$Male45.54+ind_agg$Male55.64+ind_agg$Male65.74+ind_agg$Female16.24+ind_agg$Female25.34+ind_agg$Female35.44+ind_agg$Female45.54+ind_agg$Female55.64+ind_agg$Female65.74)
percentages$Male25.34 = percentages$Male16.24+ind_agg$Male25.34/AgeEcoOccupSex$`16-74`
percentages$Male35.44 = percentages$Male25.34+ind_agg$Male35.44/AgeEcoOccupSex$`16-74`
percentages$Male45.54 = percentages$Male35.44+ind_agg$Male45.54/AgeEcoOccupSex$`16-74`
percentages$Male55.64 = percentages$Male45.54+ind_agg$Male55.64/AgeEcoOccupSex$`16-74`
percentages$Male65.74 = percentages$Male55.64+ind_agg$Male65.74/AgeEcoOccupSex$`16-74`

percentages$Female16.24 = percentages$Male65.74+ind_agg$Female16.24/AgeEcoOccupSex$`16-74`
percentages$Female25.34 = percentages$Female16.24+ind_agg$Female25.34/AgeEcoOccupSex$`16-74`
percentages$Female35.44 = percentages$Female25.34+ind_agg$Female35.44/AgeEcoOccupSex$`16-74`
percentages$Female45.54 = percentages$Female35.44+ind_agg$Female45.54/AgeEcoOccupSex$`16-74`
percentages$Female55.64 = percentages$Female45.54+ind_agg$Female55.64/AgeEcoOccupSex$`16-74`
percentages$Female65.74 = percentages$Female55.64+ind_agg$Female65.74/AgeEcoOccupSex$`16-74`

# See integerised weight matrix
weights = read.csv('Datasets/IPF/weights.csv')
set.seed(0)
weights_int = apply(weights, 2, int_trs)
write.csv(weights_int, file = 'Datasets/IPF/weights_int.csv', row.names = FALSE)





