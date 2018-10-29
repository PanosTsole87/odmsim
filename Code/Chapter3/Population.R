# library(reshape2)

AgeEcoOccupSex = read.csv('Datasets/Sociodemographic/AgeEcoOccupSex.csv')
############### Sociodemographic datasets Gender-Age ################################################
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


Commuting_sexage_totals_ALL2 <- melt(Commuting_sexage_totals_ALL, id.vars='Age')
head(Commuting_sexage_totals_ALL2)

p <-ggplot(Commuting_sexage_totals_ALL2, aes(x=Age, y=value, fill=variable))
p +geom_bar(stat = "identity", position='dodge')+
  theme(text = element_text(size = 20))

# Total population for all age bands
Total_population_all_ages_sex = read.csv('Datasets/Sociodemographic/Data_SEX_UNIT_ALL.csv', skip = 1)

Total_population = Total_population_all_ages_sex[, 1:2]

Total_population$`Male0-15` = Total_population_all_ages_sex$Age...Age.under.1...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.1...Sex...Males...Unit...Persons+
  Total_population_all_ages_sex$Age...Age.2...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.3...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.4...Sex...Males...Unit...Persons+
  Total_population_all_ages_sex$Age...Age.5...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.6...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.7...Sex...Males...Unit...Persons+
  Total_population_all_ages_sex$Age...Age.8...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.9...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.10...Sex...Males...Unit...Persons+
  Total_population_all_ages_sex$Age...Age.11...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.12...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.13...Sex...Males...Unit...Persons+
  Total_population_all_ages_sex$Age...Age.14...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.15...Sex...Males...Unit...Persons

Total_population$`Male16-24` = Total_population_all_ages_sex$Age...Age.16...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.17...Sex...Males...Unit...Persons+
  Total_population_all_ages_sex$Age...Age.18...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.19...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.20...Sex...Males...Unit...Persons+
  Total_population_all_ages_sex$Age...Age.21...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.22...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.23...Sex...Males...Unit...Persons+
  Total_population_all_ages_sex$Age...Age.24...Sex...Males...Unit...Persons

Total_population$`Male25-34` = Total_population_all_ages_sex$Age...Age.25.to.29...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.30.to.34...Sex...Males...Unit...Persons

Total_population$`Male35-44` = Total_population_all_ages_sex$Age...Age.35.to.39...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.40.to.44...Sex...Males...Unit...Persons

Total_population$`Male45-54` = Total_population_all_ages_sex$Age...Age.45.to.49...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.50.to.54...Sex...Males...Unit...Persons

Total_population$`Male55-64` = Total_population_all_ages_sex$Age...Age.55.to.59...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.60.to.64...Sex...Males...Unit...Persons

Total_population$`Male65-74` = Total_population_all_ages_sex$Age...Age.65.to.69...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.70.to.74...Sex...Males...Unit...Persons

Total_population$`Male75over` = Total_population_all_ages_sex$Age...Age.75.to.79...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.80.to.84...Sex...Males...Unit...Persons+Total_population_all_ages_sex$Age...Age.85.and.over...Sex...Males...Unit...Persons


Total_population$`Female0-15` = Total_population_all_ages_sex$Age...Age.under.1...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.1...Sex...Females...Unit...Persons+
  Total_population_all_ages_sex$Age...Age.2...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.3...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.4...Sex...Females...Unit...Persons+
  Total_population_all_ages_sex$Age...Age.5...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.6...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.7...Sex...Females...Unit...Persons+
  Total_population_all_ages_sex$Age...Age.8...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.9...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.10...Sex...Females...Unit...Persons+
  Total_population_all_ages_sex$Age...Age.11...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.12...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.13...Sex...Females...Unit...Persons+
  Total_population_all_ages_sex$Age...Age.14...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.15...Sex...Females...Unit...Persons

Total_population$`Female16-24` = Total_population_all_ages_sex$Age...Age.16...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.17...Sex...Females...Unit...Persons+
  Total_population_all_ages_sex$Age...Age.18...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.19...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.20...Sex...Females...Unit...Persons+
  Total_population_all_ages_sex$Age...Age.21...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.22...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.23...Sex...Females...Unit...Persons+
  Total_population_all_ages_sex$Age...Age.24...Sex...Females...Unit...Persons

Total_population$`Female25-34` = Total_population_all_ages_sex$Age...Age.25.to.29...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.30.to.34...Sex...Females...Unit...Persons

Total_population$`Female35-44` = Total_population_all_ages_sex$Age...Age.35.to.39...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.40.to.44...Sex...Females...Unit...Persons

Total_population$`Female45-54` = Total_population_all_ages_sex$Age...Age.45.to.49...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.50.to.54...Sex...Females...Unit...Persons

Total_population$`Female55-64` = Total_population_all_ages_sex$Age...Age.55.to.59...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.60.to.64...Sex...Females...Unit...Persons

Total_population$`Female65-74` = Total_population_all_ages_sex$Age...Age.65.to.69...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.70.to.74...Sex...Females...Unit...Persons

Total_population$`Female75over` = Total_population_all_ages_sex$Age...Age.75.to.79...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.80.to.84...Sex...Females...Unit...Persons+Total_population_all_ages_sex$Age...Age.85.and.over...Sex...Females...Unit...Persons

Total_population = Total_population[, -(1:2)]
sum(Total_population)
sum(Total_population_all_ages_sex$Sex...Males...Unit...Persons)+sum(Total_population_all_ages_sex$Sex...Females...Unit...Persons)
sum(Total_population_all_ages_sex$Age...Total..Age...Sex...Males...Unit...Persons)+sum(Total_population_all_ages_sex$Age...Total..Age...Sex...Females...Unit...Persons)
Total_population$zone = Total_population_all_ages_sex$X.1

write.csv(Total_population, file = 'Datasets/Sociodemographic/Total_population.csv', row.names = F)

# Find the non-employed population
Remaining_population = Total_population
Remaining_population$`Male16-24` = Total_population$`Male16-24`-Employed_population$`Male16-24`
Remaining_population$`Male25-34` = Total_population$`Male25-34`-Employed_population$`Male25-34`
Remaining_population$`Male35-44` = Total_population$`Male35-44`-Employed_population$`Male35-44`
Remaining_population$`Male45-54` = Total_population$`Male45-54`-Employed_population$`Male45-54`
Remaining_population$`Male55-64` = Total_population$`Male55-64`-Employed_population$`Male55-64`
Remaining_population$`Male65-74` = Total_population$`Male65-74`-Employed_population$`Male65-74`
Remaining_population$Male75over = Total_population$Male75over-Employed_population$Male75over

Remaining_population$`Female16-24` = Total_population$`Female16-24`-Employed_population$`Female16-24`
Remaining_population$`Female25-34` = Total_population$`Female25-34`-Employed_population$`Female25-34`
Remaining_population$`Female35-44` = Total_population$`Female35-44`-Employed_population$`Female35-44`
Remaining_population$`Female45-54` = Total_population$`Female45-54`-Employed_population$`Female45-54`
Remaining_population$`Female55-64` = Total_population$`Female55-64`-Employed_population$`Female55-64`
Remaining_population$`Female65-74` = Total_population$`Female65-74`-Employed_population$`Female65-74`
Remaining_population$Female75over = Total_population$Female75over-Employed_population$Female75over

# Load General Health of the Population
General_health = read.csv('Datasets/Sociodemographic/Health.csv', skip = 1)
General_health = General_health[,6:10]
sum(General_health)

