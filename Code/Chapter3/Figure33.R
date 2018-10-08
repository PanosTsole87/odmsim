# Load libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)

YorkMicro_employed = read.csv('Datasets/Microdata/YorkMicro_employed.csv')

# Select only Gender-Age
YorkMicro_employed_sexage = YorkMicro_employed[,6:7]
#str(YorkMicro_employed_sexage)

################# Transformation to plot Gender-Age distribution in the Microdata ###############################

YorkMicro_employed_sexage2 = data.frame(Age = c('Age.16_24', 'Age.25_34', 'Age.35_44', 'Age.45_54', 'Age.55_64', 'Age.65_74', 'Age.75over'), 
                                        Female = c(sum(YorkMicro_employed_sexage$Sex==2 & YorkMicro_employed_sexage$Age==2), 
                                                   sum(YorkMicro_employed_sexage$Sex==2 & YorkMicro_employed_sexage$Age==3),
                                                   sum(YorkMicro_employed_sexage$Sex==2 & YorkMicro_employed_sexage$Age==4),
                                                   sum(YorkMicro_employed_sexage$Sex==2 & YorkMicro_employed_sexage$Age==5),
                                                   sum(YorkMicro_employed_sexage$Sex==2 & YorkMicro_employed_sexage$Age==6),
                                                   sum(YorkMicro_employed_sexage$Sex==2 & YorkMicro_employed_sexage$Age==7),
                                                   sum(YorkMicro_employed_sexage$Sex==2 & YorkMicro_employed_sexage$Age==8)),
                                        Male = c(sum(YorkMicro_employed_sexage$Sex==1 & YorkMicro_employed_sexage$Age==2),
                                                 sum(YorkMicro_employed_sexage$Sex==1 & YorkMicro_employed_sexage$Age==3),
                                                 sum(YorkMicro_employed_sexage$Sex==1 & YorkMicro_employed_sexage$Age==4),
                                                 sum(YorkMicro_employed_sexage$Sex==1 & YorkMicro_employed_sexage$Age==5),
                                                 sum(YorkMicro_employed_sexage$Sex==1 & YorkMicro_employed_sexage$Age==6),
                                                 sum(YorkMicro_employed_sexage$Sex==1 & YorkMicro_employed_sexage$Age==7),
                                                 sum(YorkMicro_employed_sexage$Sex==1 & YorkMicro_employed_sexage$Age==8)))


# Reshape df
YorkMicro_employed_sexage3 <- melt(YorkMicro_employed_sexage2, id.vars='Age')
#head(YorkMicro_employed_sexage3)

p1 <-ggplot(YorkMicro_employed_sexage3, aes(x=Age, y=value, fill=variable))+
  geom_bar(stat = "identity", position='dodge')+
  theme(text = element_text(size = 40), plot.title = element_text(hjust = 0.5, size = 50))+
  ggtitle("Microdata")



############### Sociodemographic datasets Gender-Age ################################################
source('Code/Chapter3/DataPreparation_Census.R')
# Employed Population Over16 including Over75
Employed_population = AgeEcoOccupSex[,c(52:61, 45:46)]
Employed_population = Employed_population[, c(1:5,11,6:10,12)]
Employed_population$Male75over = AgeEcoOccupSex$MaleOver16 - AgeEcoOccupSex$`Male16-74`
Employed_population$Female75over = AgeEcoOccupSex$FemaleOver16 - AgeEcoOccupSex$`Female16-74`
Employed_population = Employed_population[, c(1:6,13,7:12,14)]
sum(Employed_population)
Employed_population$zone = AgeEcoOccupSex$X.1

Commuting_sexage_totals_ALL = data.frame(Age = c('Age.16_24', 'Age.25_34', 'Age.35_44', 'Age.45_54', 'Age.55_64', 'Age.65_74', 'Age.75Over'), 
                                         Female = c(sum(Employed_population$`Female16-24`), sum(Employed_population$`Female25-34`), sum(Employed_population$`Female35-44`),
                                                    sum(Employed_population$`Female45-54`), sum(Employed_population$`Female55-64`), sum(Employed_population$`Female65-74`), sum(Employed_population$Female75over)),
                                         Male = c(sum(Employed_population$`Male16-24`), sum(Employed_population$`Male25-34`), sum(Employed_population$`Male35-44`),
                                                  sum(Employed_population$`Male45-54`), sum(Employed_population$`Male55-64`), sum(Employed_population$`Male65-74`), sum(Employed_population$Male75over)))

# Reshape df
Commuting_sexage_totals_ALL2 <- melt(Commuting_sexage_totals_ALL, id.vars='Age')
head(Commuting_sexage_totals_ALL2)

p2 <-ggplot(Commuting_sexage_totals_ALL2, aes(x=Age, y=value, fill=variable))+
  geom_bar(stat = "identity", position='dodge')+
  theme(text = element_text(size = 40), plot.title = element_text(hjust = 0.5, size = 50))+
  ggtitle("Aggregated dataset")

# Show both plots side by side
grid.arrange(p1, p2, ncol = 2)
