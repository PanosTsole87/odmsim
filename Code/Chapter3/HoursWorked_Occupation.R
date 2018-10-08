Age_Eco_Hours_Occ = read.csv('Datasets/Sociodemographic/Data_AGE_ECOACT_HRSWRK_OCCUP_UNIT.csv', skip = 1)
sum(Age_Eco_Hours_Occ[6:41])

HoursWorked = read.csv('Datasets/Sociodemographic/HoursWorked.csv', skip = 1)
HoursWorked = HoursWorked[,c(2:3, 6:9)]
x=c('Code', 'Name', 'HW1', 'HW2', 'HW3', 'HW4')
colnames(HoursWorked) = x

sum(HoursWorked[3:6])


Occupation = read.csv('Datasets/Sociodemographic/Occupation.csv', skip = 1)
Occupation = Occupation[,c(2:3, 10:18)]

x=c('Code', 'Name', 'Occ1', 'Occ2', 'Occ3', 'Occ4', 'Occ5', 'Occ6', 'Occ7', 'Occ8', 'Occ9')
colnames(Occupation) = x
sum(Occupation[3:11])
con_occup = Occupation[3:11]

write.csv(con_occup, file = 'Datasets/Sociodemographic/con_occup.csv', row.names = F)
