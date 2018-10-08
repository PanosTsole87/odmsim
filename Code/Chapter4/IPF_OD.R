library(ipfp)
library(dplyr)
source('Code/Chapter4/functions.R')
library(stplanr)




###################################################################################
# Automated IPF

ints_df = read.csv('Datasets/IPF/ints_df.csv')
ints_df$Sex = as.factor(ints_df$Sex)
ints_df$Age = as.factor(ints_df$Age)


ints_agg = read.csv('Datasets/IPF/ints_agg.csv')


OD_sexage = read.csv('Datasets/OD/OD_SexAge.csv')
#str(OD_sexage) # Change Factors to Characters
OD_sexage$Residence = as.character(OD_sexage$Residence)
OD_sexage$Workplace = as.character(OD_sexage$Workplace)
OD_sexage$ResWork = as.character(OD_sexage$ResWork)
OD_sexage = OD_sexage[,c(1,2,6,3,4,5,7,8,9,10,11,12,13,14,15)]

cons_IPF = OD_sexage[, -c(1:4, 7, 15)]
cons_IPF[cons_IPF==0] = 0.0000000001 # replace 0s with a small value
sum(cons_IPF)/2 # total commuters 2428074
cons_IPF$Residence = OD_sexage$Residence # Add again Residence

# Origins = unique(OD_sexage$Residence)
# ints_df$Origin = rep(Origins, ints_agg$n) # Add Origins (Residence) to ints_df


ints_df_flat = ints_df[,8:9]
str(ints_df_flat)
ints_df_flat$Sex = as.factor(ints_df_flat$Sex)
ints_df_flat$Age = as.factor(ints_df_flat$Age)
str(ints_df_flat)

cat_sex = model.matrix(~ ints_df_flat$Sex -1)
cat_age = model.matrix(~ ints_df_flat$Age -1)
m1 = cbind(cat_sex, cat_age)

head(cons_IPF)
head(m1)
colnames(m1) <- names(cons_IPF)[1:9]
head(m1)
summary(rowSums(m1))

m1 <- data.frame(m1)
m1$Origin = ints_df$Origin

ind_agg0 = list()
ints_OD_df = list()
agg_ints_OD_df = list()
cor0 = vector()
corr = vector()
tae0 = vector()


for (z in 1:length(Origins)){
  print(z)
  ints_perzone_df = ints_df[ints_df$Origin == Origins[z],]
  ints_perzone_df$id = rep(1:nrow(ints_perzone_df))
  OD_SexAge_perzone = OD_sexage[OD_sexage$Residence == Origins[z],]
  cons = cons_IPF[cons_IPF$Residence == Origins[z], -10] 
  ind_cat = m1[m1$Origin == Origins[z], -10]
  n_ind_cat = nrow(ind_cat)
  x0 = rep(1, n_ind_cat)
  ind_catt = t(ind_cat)
  cons = apply(cons, 2, as.numeric)
  
  # Run IPF ---Weight assignment (real numbers)
  weights = apply(cons, 1, FUN = function(x)
    ipfp(x, ind_catt, x0, maxit = 20, verbose = F))
  # Calculate individuals per zone
  ind_agg_perzone = t(apply(weights, 2, function(x) colSums(x*ind_cat)))
  colnames(ind_agg_perzone) = colnames(cons)
  sum(ind_agg_perzone)/2 # 4041
  c=cor(as.numeric(cons), as.numeric(ind_agg_perzone)) #correlation=1
  cor0 = c(cor0, c)
  
  t = tae(cons, ind_agg_perzone)
  tae0 = c(tae0, t)
  
  ind_agg0 = rbind(ind_agg0, ind_agg_perzone)
  
  # TRS
  ints = apply(weights, 2, int_trs)
  indices = NULL
  ints = for(i in 1:ncol(ints)){
    indices = c(indices, int_expand_vector(ints[, i]))
  }
  
  ints_OD_perzone_df = data.frame(id = indices)
  ints_OD_perzone_df$Destination = rep(OD_SexAge_perzone$Workplace, OD_SexAge_perzone$Total_age)
  
  # Join individual microdata with expanded interegised individuals 
  ints_OD_perzone_df = inner_join(ints_OD_perzone_df, ints_perzone_df, by='id')
  ints_OD_df = rbind(ints_OD_df, ints_OD_perzone_df)
  # Aggregate microdata
  agg_ints_OD_perzone_df = ints_OD_perzone_df %>% 
    group_by(Destination) %>% 
    summarise(Total = n(), Male = sum(Sex==1), Female = sum(Sex==2), 
              '16_24' = sum(Age==2), '25-34' = sum(Age==3), '35_44' = sum(Age==4),
              '45-54' = sum(Age==5), '55-64' = sum(Age==6), '65-74' = sum(Age==7), 'Over75' = sum(Age==8))
  
  agg_ints_OD_perzone_df$Origin = Origins[z]
  agg_ints_OD_df = rbind(agg_ints_OD_df, agg_ints_OD_perzone_df)
  
  # Cor after TRS
  x1 = agg_ints_OD_perzone_df[, -c(1:2, 12)]
  x1=data.matrix(x1)
  cc = cor(as.numeric(x1), as.numeric(cons))
  corr = c(corr, cc)
  
  # TAE after TRS
  # tt = tae(cons, x1)
  # taee = c(tae, tt)
}

# Check for missing OD pairs
OD_total_initial = cons_IPF
OD_total_initial$Total = OD_total_initial$Male + OD_total_initial$Female 
sum(OD_total_initial$Total)
OD_total_initial$Workplace = OD_sexage$Workplace
OD_total_initial$ResWork = paste0(OD_total_initial$Residence, OD_total_initial$Workplace)

agg_ints_OD_df$ResWork = paste0(agg_ints_OD_df$Origin, agg_ints_OD_df$Destination)

# Aggregate OD pair df to each Origin (692 rows)
agg_OD_df = agg_ints_OD_df %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Total), Male=sum(Male), Female = sum(Female), Age16_24=sum(`16_24`), Age25_34 =sum(`25-34`),
            Age35_44=sum(`35_44`), Age45_54=sum(`45-54`), Age55_64=sum(`55-64`), Age65_74=sum(`65-74`), AgeOver75=sum(`Over75`))

agg_OD_initial = OD_total_initial %>% 
  group_by(Residence) %>% 
  summarise(Total = sum(Total), Male=sum(Male), Female = sum(Female), Age16_24=sum(X16.24), Age25_34 =sum(X25.34),
            Age35_44=sum(X35.44), Age45_54=sum(X45.54), Age55_64=sum(X55.64), Age65_74=sum(X65.74), AgeOver75=sum(Over75))


# Save integerised individuals - OD pairs df
ints_OD_df$OD = paste0(ints_OD_df$Origin, ints_OD_df$Destination)

write.csv(ints_OD_df, file = 'Datasets/IPF/ints_OD_df.csv', row.names = F)


ints_OD_df = read.csv('Datasets/IPF/ints_OD_df.csv')

# Save new aggregated OD pairs df
write.csv(agg_ints_OD_df, file = 'Datasets/IPF/agg_ints_OD_df.csv', row.names = F)
agg_ints_OD_df = read.csv('Datasets/IPF/agg_ints_OD_df.csv')


ints_OD_agg1 = ints_OD_df %>% 
  group_by(OD) %>% 
  summarise(Total=n(), Male=sum(Sex==1), Female=sum(Sex==2), '16-24' = sum(Age==2), '25-34' = sum(Age==3), '35-44' = sum(Age==4),
            '45-54' = sum(Age==5), '55-64' = sum(Age==6), '65-74' = sum(Age==7), 'Over75' = sum(Age==8), 'Occ1' = sum(Occupation==1), 
            'Occ2' = sum(Occupation==2), 'Occ3' = sum(Occupation==3), 'Occ4' = sum(Occupation==4), 'Occ5' = sum(Occupation==5), 
            'Occ6' = sum(Occupation==6), 'Occ7' = sum(Occupation==7), 'Occ8' = sum(Occupation==8), 'Occ9' = sum(Occupation==9), 
            avWork = mean(HW_numeric), avAge = mean(Age_numeric), 
            Health_1 = sum(Health==1), Health_2 = sum(Health==2), Health_3 = sum(Health==3), Health_4 = sum(Health==4), Health_5 = sum(Health==5), 
            HW_1 = sum(Hours.worked.per.week==1), HW_2 = sum(Hours.worked.per.week==2), HW_3 = sum(Hours.worked.per.week==3), HW_4 = sum(Hours.worked.per.week==4), 
            SG_AB = sum(Approximated.Social.Grade==1), SG_C1 = sum(Approximated.Social.Grade==2), SG_C2 = sum(Approximated.Social.Grade==3), SG_DE = sum(Approximated.Social.Grade==4))

ints_OD_agg1$Destination = OD_SexAge_Mode$Workplace


ints_OD_agg1$initial_Total = OD_sexage$Total
ints_OD_agg1$Initial_Male = OD_sexage$Male
ints_OD_agg1$Initial_Female = OD_sexage$Female

ints_OD_agg1$`Initial_16_24` = OD_sexage$X16.24
ints_OD_agg1$`Initial_25_34` = OD_sexage$X25.34
ints_OD_agg1$`Initial_35_44` = OD_sexage$X35.44
ints_OD_agg1$`Initial_45_54` = OD_sexage$X45.54
ints_OD_agg1$`Initial_55_64` = OD_sexage$X55.64
ints_OD_agg1$`Initial_65_74` = OD_sexage$X65.74
ints_OD_agg1$`Initial_Over75` = OD_sexage$Over75

ints_OD_agg1$Health_1_perc = ints_OD_agg1$Health_1/ints_OD_agg1$Total
ints_OD_agg1$Health_2_perc = ints_OD_agg1$Health_2/ints_OD_agg1$Total
ints_OD_agg1$Health_3_perc = ints_OD_agg1$Health_3/ints_OD_agg1$Total
ints_OD_agg1$Health_4_perc = ints_OD_agg1$Health_4/ints_OD_agg1$Total
ints_OD_agg1$Health_5_perc = ints_OD_agg1$Health_5/ints_OD_agg1$Total

ints_OD_agg1$SG_AB_perc = ints_OD_agg1$SG_AB/ints_OD_agg1$Total
ints_OD_agg1$SG_C1_perc = ints_OD_agg1$SG_C1/ints_OD_agg1$Total
ints_OD_agg1$SG_C2_perc = ints_OD_agg1$SG_C2/ints_OD_agg1$Total
ints_OD_agg1$SG_DE_perc = ints_OD_agg1$SG_DE/ints_OD_agg1$Total

write.csv(ints_OD_agg1, file = 'Datasets/IPF/ints_OD_agg1.csv', row.names = F)

ints_OD_agg1 = read.csv('Datasets/IPF/ints_OD_agg1.csv')



