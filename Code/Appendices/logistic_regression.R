# Logistic Regression

source('Code/Chapter3/DataPreparation_Microdata.R')

# Importing the dataset
dataset = read.csv('Datasets/Microdata/Total_Micro.csv')
dataset = dataset[dataset$Age!=1,]

str(dataset)

################## Correct SocialGrade = -9 ################################
social_grade = dataset[dataset$Approximated.Social.Grade!=-9,]
no_social_grade = dataset[dataset$Approximated.Social.Grade==-9,]

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
dataset = rbind(social_grade, no_social_grade_new)
str(no_social_grade_new)


################## Correct FamilyComposition = -9  ################################
family_composition = dataset[dataset$Family.Composition!=-9,]
no_family_composition = dataset[dataset$Family.Composition==-9,]

no_family_composition_new = list()
no_family_composition_1 = list()
no_family_composition_2 = list()
no_family_composition_3 = list()
no_family_composition_4 = list()
no_family_composition_5 = list()
no_family_composition_6 = list()

# Family Composition 1
no_family_composition_1 = sample_n(no_family_composition, 
                             (nrow(family_composition[family_composition$Family.Composition==1, ])/nrow(family_composition))*nrow(no_family_composition))
no_family_composition_1$Family.Composition=1

no_family_composition_new = rbind(no_family_composition_new, no_family_composition_1)

# Family Composition 2
no_family_composition_2 = no_family_composition[!(no_family_composition$Person.ID %in% no_family_composition_new$Person.ID),]
no_family_composition_2 = sample_n(no_family_composition_2, 
                             (nrow(family_composition[family_composition$Family.Composition==2, ])/nrow(family_composition))*nrow(no_family_composition))
no_family_composition_2$Family.Composition=2

no_family_composition_new = rbind(no_family_composition_new, no_family_composition_2)

# Family Composition 3
no_family_composition_3 = no_family_composition[!(no_family_composition$Person.ID %in% no_family_composition_new$Person.ID),]
no_family_composition_3 = sample_n(no_family_composition_3, 
                             (nrow(family_composition[family_composition$Family.Composition==3, ])/nrow(family_composition))*nrow(no_family_composition))
no_family_composition_3$Family.Composition=3

no_family_composition_new = rbind(no_family_composition_new, no_family_composition_3)

# Family Composition 4
no_family_composition_4 = no_family_composition[!(no_family_composition$Person.ID %in% no_family_composition_new$Person.ID),]
no_family_composition_4 = sample_n(no_family_composition_4, 
                                   (nrow(family_composition[family_composition$Family.Composition==4, ])/nrow(family_composition))*nrow(no_family_composition))

no_family_composition_4$Family.Composition=4

no_family_composition_new = rbind(no_family_composition_new, no_family_composition_4)

# Family Composition 5
no_family_composition_5 = no_family_composition[!(no_family_composition$Person.ID %in% no_family_composition_new$Person.ID),]
no_family_composition_5 = sample_n(no_family_composition_5, 
                                   (nrow(family_composition[family_composition$Family.Composition==5, ])/nrow(family_composition))*nrow(no_family_composition))

no_family_composition_5$Family.Composition=5

no_family_composition_new = rbind(no_family_composition_new, no_family_composition_5)

# Family Composition 6
no_family_composition_6 = no_family_composition[!(no_family_composition$Person.ID %in% no_family_composition_new$Person.ID),]

no_family_composition_6$Family.Composition=6

no_family_composition_new = rbind(no_family_composition_new, no_family_composition_6)

no_family_composition_new = as.data.frame(no_family_composition_new)

# Bind both employed df with and without social grade
dataset = rbind(family_composition, no_family_composition_new)

dataset = dataset[, c(4:13, 18:19)]


table(dataset$Residence.Type)
table(dataset$Family.Composition)
table(dataset$Population.Base)
table(dataset$Sex)
table(dataset$Age)
table(dataset$Marital.Status)
table(dataset$Student)
table(dataset$Country.of.Birth)
table(dataset$Health)
table(dataset$Ethnic.Group)
table(dataset$Religion)
table(dataset$Economic.Activity)
table(dataset$Occupation)
table(dataset$Industry)
table(dataset$Hours.worked.per.week)
table(dataset$Approximated.Social.Grade)
table(dataset$Employed)

# Encoding the target feature as factor
#dataset$Residence.Type = factor(dataset$Residence.Type, levels = c(1, 2))
dataset$Family.Composition = factor(dataset$Family.Composition, levels = c(1, 2, 3, 4, 5, 6))
dataset$Population.Base = factor(dataset$Population.Base, levels = c(1, 3))
dataset$Sex = factor(dataset$Sex, levels = c(1, 2))
dataset$Age = factor(dataset$Age, levels = c(2, 3, 4, 5, 6, 7, 8))
dataset$Marital.Status = factor(dataset$Marital.Status, levels = c(1, 2, 3, 4, 5))
dataset$Student = factor(dataset$Student, levels = c(1, 2))
dataset$Country.of.Birth = factor(dataset$Country.of.Birth, levels = c(1, 2))
dataset$Health = factor(dataset$Health, levels = c(1, 2, 3, 4, 5))
dataset$Ethnic.Group = factor(dataset$Ethnic.Group, levels = c(1, 2, 3, 4, 5))
dataset$Religion = factor(dataset$Religion, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9))
# dataset$Economic.Activity = factor(dataset$Economic.Activity, levels = c(-9, 1, 2, 3, 4, 5, 6, 7, 8, 9))
# dataset$Occupation = factor(dataset$Occupation, levels = c(-9, 1, 2, 3, 4, 5, 6, 7, 8, 9))
# dataset$Industry = factor(dataset$Industry, levels = c(-9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
# dataset$Hours.worked.per.week = factor(dataset$Hours.worked.per.week, levels = c(-9, 1, 2, 3, 4))
dataset$Approximated.Social.Grade = factor(dataset$Approximated.Social.Grade, levels = c( 1, 2, 3, 4))
dataset$Employed = factor(dataset$Employed, levels = c(0, 1))

#contrasts(dataset$Residence.Type)
contrasts(dataset$Population.Base)
contrasts(dataset$Family.Composition)
contrasts(dataset$Sex)
contrasts(dataset$Age)
contrasts(dataset$Marital.Status)
contrasts(dataset$Student)
contrasts(dataset$Approximated.Social.Grade)
contrasts(dataset$Employed)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
# library(caTools)
# #set.seed(123)
# split = sample.split(dataset$Employed, SplitRatio = 0.75)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# # Feature Scaling
# training_set[-3] = scale(training_set[-3])
# test_set[-3] = scale(test_set[-3])

#### Fitting Logistic Regression to the Training set ##########################
classifier = glm(formula = Employed ~ .,
                 family = binomial(link = 'logit'),
                 data = dataset)
summary(classifier)
# Run Anova
anova(classifier, test="Chisq")

#### Find R2 ################################################################
#install.packages('pscl')
library(pscl)
R_sq = pR2(classifier)

Mc_Fadden_R_squared = c(round(R_sq[4], 3), '-', '-')

#### Create Table for Results ###############################################

Coefficient =  round(summary(classifier)$coefficients[,1], 3)
Standard_error = round(summary(classifier)$coefficients[,2], 3)
P_value = round(summary(classifier)$coefficients[,4], 3)

for (i in 1:length(P_value)){
  
  if (P_value[i]<=0.01){
    P_value[i] = paste0(P_value[i], '***')
    
  } else if (P_value[i]>0.01 && P_value[i]<=0.05){
    
    P_value[i] = paste0(P_value[i], '**')
    
  } else if (P_value[i]>0.05 && P_value[i]<=0.1) {
    
    (P_value[i] = paste0(P_value[i], '*'))
    
  }
}



Results_table = cbind(Coefficient, Standard_error, P_value)
Results_table = rbind(Results_table, Mc_Fadden_R_squared)
Results_table = data.frame(Results_table)
