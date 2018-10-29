# library(dplyr)
AgeEcoOccupSex = read.csv('Datasets/Sociodemographic/Data_AGE_ECOACT_OCCUP_SEX_UNIT.csv', skip = 1)
names(AgeEcoOccupSex)
AgeEcoOccupSex = AgeEcoOccupSex %>% 
  dplyr::select(-X, -X.2, -X.3, -X.4, -X.5,
         -Age...Age.16.and.over...Economic.activity...Economically.active..including.full.time.students...In.employment...Occupation...Total..Occupation...Sex...Total..Sex...Unit...Persons,
         -Age...Age.16.and.over...Economic.activity...Economically.active..including.full.time.students...In.employment...Occupation...Total..Occupation...Sex...Males...Unit...Persons,
         -Age...Age.16.and.over...Economic.activity...Economically.active..including.full.time.students...In.employment...Occupation...Total..Occupation...Sex...Females...Unit...Persons,
         '16-74' = 'Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Total..Sex...Unit...Persons',
         'Male16-74' = 'Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Males...Unit...Persons',
         'Female16-74' = 'Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Females...Unit...Persons',
         'Over16' = 'Age...Age.16.and.over...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Total..Sex...Unit...Persons',
         'MaleOver16' = 'Age...Age.16.and.over...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Males...Unit...Persons',
         'FemaleOver16' = 'Age...Age.16.and.over...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Females...Unit...Persons',
         '16-19' = 'Age...Age.16.to.19...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Total..Sex...Unit...Persons',
         '20-21' = 'Age...Age.20.to.21...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Total..Sex...Unit...Persons',
         '22-24' = 'Age...Age.22.to.24...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Total..Sex...Unit...Persons',
         '25-29' = 'Age...Age.25.to.29...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Total..Sex...Unit...Persons',
         '30-34' = 'Age...Age.30.to.34...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Total..Sex...Unit...Persons',
         '35-39' = 'Age...Age.35.to.39...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Total..Sex...Unit...Persons',
         '40-44' = 'Age...Age.40.to.44...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Total..Sex...Unit...Persons',
         '45-49' = 'Age...Age.45.to.49...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Total..Sex...Unit...Persons',
         '50-54' = 'Age...Age.50.to.54...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Total..Sex...Unit...Persons',
         '55-59' = 'Age...Age.55.to.59...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Total..Sex...Unit...Persons',
         '60-64' = 'Age...Age.60.to.64...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Total..Sex...Unit...Persons',
         'Over65' = 'Age...Age.65.and.over...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Total..Sex...Unit...Persons',
         'Male16-19' = 'Age...Age.16.to.19...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Males...Unit...Persons',
         'Male20-21' = 'Age...Age.20.to.21...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Males...Unit...Persons',
         'Male22-24' = 'Age...Age.22.to.24...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Males...Unit...Persons',
         'Male25-29' = 'Age...Age.25.to.29...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Males...Unit...Persons',
         'Male30-34' = 'Age...Age.30.to.34...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Males...Unit...Persons',
         'Male35-39' = 'Age...Age.35.to.39...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Males...Unit...Persons',
         'Male40-44' = 'Age...Age.40.to.44...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Males...Unit...Persons',
         'Male45-49' = 'Age...Age.45.to.49...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Males...Unit...Persons',
         'Male50-54' = 'Age...Age.50.to.54...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Males...Unit...Persons',
         'Male55-59' = 'Age...Age.55.to.59...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Males...Unit...Persons',
         'Male60-64' = 'Age...Age.60.to.64...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Males...Unit...Persons',
         'MaleOver65' = 'Age...Age.65.and.over...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Males...Unit...Persons',
         'Female16-19' = 'Age...Age.16.to.19...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Females...Unit...Persons',
         'Female20-21' = 'Age...Age.20.to.21...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Females...Unit...Persons',
         'Female22-24' = 'Age...Age.22.to.24...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Females...Unit...Persons',
         'Female25-29' = 'Age...Age.25.to.29...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Females...Unit...Persons',
         'Female30-34' = 'Age...Age.30.to.34...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Females...Unit...Persons',
         'Female35-39' = 'Age...Age.35.to.39...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Females...Unit...Persons',
         'Female40-44' = 'Age...Age.40.to.44...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Females...Unit...Persons',
         'Female45-49' = 'Age...Age.45.to.49...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Females...Unit...Persons',
         'Female50-54' = 'Age...Age.50.to.54...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Females...Unit...Persons',
         'Female55-59' = 'Age...Age.55.to.59...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Females...Unit...Persons',
         'Female60-64' = 'Age...Age.60.to.64...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Females...Unit...Persons',
         'FemaleOver65' = 'Age...Age.65.and.over...Economic.activity...In.employment.the.week.before.the.census...Occupation...Total..Occupation...Sex...Females...Unit...Persons')
AgeEcoOccupSex$`65-74` = AgeEcoOccupSex$Over65-(AgeEcoOccupSex$Over16 - AgeEcoOccupSex$`16-74`)
AgeEcoOccupSex$`Male65-74` = AgeEcoOccupSex$MaleOver65 - (AgeEcoOccupSex$MaleOver16 - AgeEcoOccupSex$`Male16-74`)
AgeEcoOccupSex$`Female65-74` = AgeEcoOccupSex$FemaleOver65 - (AgeEcoOccupSex$FemaleOver16 - AgeEcoOccupSex$`Female16-74`)

# Totals
AgeEcoOccupSex$`16-24` = AgeEcoOccupSex$`16-19` + AgeEcoOccupSex$`20-21` + AgeEcoOccupSex$`22-24`
AgeEcoOccupSex$`25-34` = AgeEcoOccupSex$`25-29` + AgeEcoOccupSex$`30-34`
AgeEcoOccupSex$`35-44` = AgeEcoOccupSex$`35-39` + AgeEcoOccupSex$`40-44`
AgeEcoOccupSex$`45-54` = AgeEcoOccupSex$`45-49` + AgeEcoOccupSex$`50-54`
AgeEcoOccupSex$`55-64` = AgeEcoOccupSex$`55-59` + AgeEcoOccupSex$`60-64`

# Males
AgeEcoOccupSex$`Male16-24` = AgeEcoOccupSex$`Male16-19` + AgeEcoOccupSex$`Male20-21` + AgeEcoOccupSex$`Male22-24`
AgeEcoOccupSex$`Male25-34` = AgeEcoOccupSex$`Male25-29` + AgeEcoOccupSex$`Male30-34`
AgeEcoOccupSex$`Male35-44` = AgeEcoOccupSex$`Male35-39` + AgeEcoOccupSex$`Male40-44`
AgeEcoOccupSex$`Male45-54` = AgeEcoOccupSex$`Male45-49` + AgeEcoOccupSex$`Male50-54`
AgeEcoOccupSex$`Male55-64` = AgeEcoOccupSex$`Male55-59` + AgeEcoOccupSex$`Male60-64`

# Females
AgeEcoOccupSex$`Female16-24` = AgeEcoOccupSex$`Female16-19` + AgeEcoOccupSex$`Female20-21` + AgeEcoOccupSex$`Female22-24`
AgeEcoOccupSex$`Female25-34` = AgeEcoOccupSex$`Female25-29` + AgeEcoOccupSex$`Female30-34`
AgeEcoOccupSex$`Female35-44` = AgeEcoOccupSex$`Female35-39` + AgeEcoOccupSex$`Female40-44`
AgeEcoOccupSex$`Female45-54` = AgeEcoOccupSex$`Female45-49` + AgeEcoOccupSex$`Female50-54`
AgeEcoOccupSex$`Female55-64` = AgeEcoOccupSex$`Female55-59` + AgeEcoOccupSex$`Female60-64`

AgeEcoOccupSex$Male_over75 = AgeEcoOccupSex$MaleOver16-AgeEcoOccupSex$`Male16-74`
AgeEcoOccupSex$Female_over75 = AgeEcoOccupSex$FemaleOver16-AgeEcoOccupSex$`Female16-74`
AgeEcoOccupSex$Over75 = AgeEcoOccupSex$Male_over75 + AgeEcoOccupSex$Female_over75

#write.csv(AgeEcoOccupSex, file = 'Datasets/Sociodemographic/AgeEcoOccupSex.csv', row.names = F)

# Select only the necessary columns
CommuteAgeSex = AgeEcoOccupSex %>% 
  dplyr::select('Zone' = X.1, 'Over16', 'MaleOver16', 'FemaleOver16',
         `16-24`, `25-34`, `35-44`, `45-54`, `55-64`, `65-74`, 'Over75', `Male16-24`, `Male25-34`, `Male35-44`,
         `Male45-54`, `Male55-64`, `Male65-74`, 'Male_over75', `Female16-24`, `Female25-34`, `Female35-44`,
         `Female45-54`, `Female55-64`, `Female65-74`, 'Female_over75')

sum(CommuteAgeSex$Over16) #Check the sum

write.csv(CommuteAgeSex, file = 'Datasets/Sociodemographic/CommuteAgeSex.csv', row.names = F)
#rm(AgeEcoOccupSex)
