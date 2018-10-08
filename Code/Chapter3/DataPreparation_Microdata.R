# Load the necessary libraries
library(dplyr)
library(ggplot2)
library(reshape2)

################## Microdata loading and cleaning ############################################################
#Read the microdata csv and skip the first row
microdata = read.csv('Datasets/Microdata/2011CensusMicrodataTeachingFile.csv', skip = 1)
# Select only Yorkshire
sel1= microdata %>% 
  filter(Region == 'E12000003')
table(sel1$Age)

# Select only employed individuals
YorkMicro_employed=sel1[sel1$Hours.worked.per.week!=-9,]


# Check that there are no -9 values
table(YorkMicro_employed$Age)

table(YorkMicro_employed$Economic.Activity)
table(YorkMicro_employed$Occupation)
table(YorkMicro_employed$Industry)
table(YorkMicro_employed$Hours.worked.per.week)
table(YorkMicro_employed$Approximated.Social.Grade) # 168 individuals with -9
table(YorkMicro_employed$Health)

################## Correct SocialGrade = -9 for employed population ################################
social_grade = YorkMicro_employed[YorkMicro_employed$Approximated.Social.Grade!=-9,]
no_social_grade = YorkMicro_employed[YorkMicro_employed$Approximated.Social.Grade==-9,]

no_social_grade_new = list()
no_social_grade_1 = list()
no_social_grade_2 = list()
no_social_grade_3 = list()
no_social_grade_4 = list()
# Social Grade 1
no_social_grade_1 = sample_n(no_social_grade, 
                             (nrow(social_grade[social_grade$Approximated.Social.Grade==1, ])/nrow(social_grade))*nrow(no_social_grade))
no_social_grade_1$Approximated.Social.Grade=1

no_social_grade_new = rbind(no_social_grade_new, no_social_grade_1)
# Social Grade 2
no_social_grade_2 = no_social_grade[!(no_social_grade$Person.ID %in% no_social_grade_new$Person.ID),]
no_social_grade_2 = sample_n(no_social_grade_2, 
                             (nrow(social_grade[social_grade$Approximated.Social.Grade==2, ])/nrow(social_grade))*nrow(no_social_grade))
no_social_grade_2$Approximated.Social.Grade=2

no_social_grade_new = rbind(no_social_grade_new, no_social_grade_2)
# Social Grade 3
no_social_grade_3 = no_social_grade[!(no_social_grade$Person.ID %in% no_social_grade_new$Person.ID),]
no_social_grade_3 = sample_n(no_social_grade_3, 
                             (nrow(social_grade[social_grade$Approximated.Social.Grade==3, ])/nrow(social_grade))*nrow(no_social_grade))
no_social_grade_3$Approximated.Social.Grade=3

no_social_grade_new = rbind(no_social_grade_new, no_social_grade_3)
# Social Grade 4
no_social_grade_4 = no_social_grade[!(no_social_grade$Person.ID %in% no_social_grade_new$Person.ID),]

no_social_grade_4$Approximated.Social.Grade=4

no_social_grade_new = rbind(no_social_grade_new, no_social_grade_4)

no_social_grade_new = as.data.frame(no_social_grade_new)

# Bind both employed df with and without social grade
YorkMicro_employed = rbind(social_grade, no_social_grade_new)
#str(no_social_grade_new)

# Create a new column that signifies those individuals as employed ones
YorkMicro_employed$Employed = 1
YorkMicro_employed1 = YorkMicro_employed[,3:18]
# Save the df
write.csv(YorkMicro_employed, file = 'Datasets/Microdata/YorkMicro_employed.csv', row.names = F)

YorkMicro_employed = read.csv('Datasets/Microdata/YorkMicro_employed.csv')

# Remove non-necessary df
rm(microdata)
rm(no_social_grade)
rm(no_social_grade_1)
rm(no_social_grade_2)
rm(no_social_grade_3)
rm(no_social_grade_4)
rm(social_grade)



# Select first 5 individuals
ind_sample = head(YorkMicro_employed[order(YorkMicro_employed$Person.ID),], 5)

################# Remaining individuals #########################################

YorkMicro_remaining = sel1[!(sel1$Person.ID %in%  YorkMicro_employed$Person.ID),]
YorkMicro_remaining = YorkMicro_remaining[YorkMicro_remaining$Hours.worked.per.week==-9,]
table(YorkMicro_remaining$Economic.Activity)
table(YorkMicro_remaining$Occupation)
table(YorkMicro_remaining$Industry)
table(YorkMicro_remaining$Hours.worked.per.week)
table(YorkMicro_remaining$Approximated.Social.Grade)
table(YorkMicro_remaining$Health)
table(YorkMicro_remaining$Family.Composition)
table(YorkMicro_remaining$Marital.Status)
table(YorkMicro_remaining$Country.of.Birth)
table(YorkMicro_remaining$Ethnic.Group)
table(YorkMicro_remaining$Religion)
table(YorkMicro_remaining$Student)

YorkMicro_remaining$Employed = 0
write.csv(YorkMicro_remaining, file = 'Datasets/Microdata/YorkMicro_remaining.csv', row.names = F)
YorkMicro_remaining = YorkMicro_remaining[YorkMicro_remaining$Religion!=-9,]

Total_Micro = rbind(YorkMicro_employed, YorkMicro_remaining)
# Sort individuals by Person ID
Total_Micro = Total_Micro[order(Total_Micro$Person.ID),]


table(Total_Micro$Economic.Activity)
table(Total_Micro$Occupation)
table(Total_Micro$Industry)
table(Total_Micro$Hours.worked.per.week)
table(Total_Micro$Approximated.Social.Grade)
table(Total_Micro$Health)
table(Total_Micro$Family.Composition)
table(Total_Micro$Population.Base)
table(Total_Micro$Marital.Status)
table(Total_Micro$Country.of.Birth)
table(Total_Micro$Ethnic.Group)
table(Total_Micro$Religion)
table(Total_Micro$Student)

write.csv(Total_Micro, file = 'Datasets/Microdata/Total_Micro.csv', row.names = F)

#Total_Micro = read.csv('Datasets/Microdata/Total_Micro.csv')