# Author : Nasser Albalawi 
# Date: 
# Description: 
#Download and Import the dataset
 
#download.file("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/v2/fatal-police-shootings-data.csv",
              #"v2_fatal-police-shootings-data.csv")

#Import the dataset
shootings_v2 <- read.csv("v2_fatal-police-shootings-data.csv")

#Inspect the dimension and column names
dim(shootings_v2)
names(shootings_v2)

#=================================Data discretization of Age=========================================
library(dplyr)
#select some variables to work with
shootings_select <- select(shootings_v2, 2,3,4,5,13,14,15,17)
shootings_select <- filter(shootings_select, !is.na(age))

#Binning of age group

#Age groups without labels
age_groups <- cut(shootings_select$age, 
     breaks = c(0, 25, 50, 75, 100))

#Age groups with labels
age_groups_2 <- cut(shootings_select$age, 
                  breaks = c(0, 25, 50, 75, 100), 
                  labels=c("young", "grown", "mature", "old"))
table(age_groups_2)
barplot(table(age_groups_2), col = c("blue","green","orange","red"), main = "Age distribution")

#Combine age_groups into one data frame
age_label_df <- tibble(age_group=age_groups, label= age_groups_2)

#The age classification will be needed later for association analysis


#=================================Test of Association==========================================
#Here, I will be using the chi-squared test to test the association between the following
#1. Gender & Mental illness
#2. Gender & Mental illness among Native Americans
#3. Age & Gender

#1. Gender & Mental illness
#SUbset for needed variables and remove non-typical entry
shootings_select_1 <- shootings_v2%>%
   filter(gender=="male" | gender=="female")

#Create contigency table for gender and mental health
gender_mental_health <- table(shootings_select_1$gender, shootings_select_1$was_mental_illness_related)

#Null hypothesis (H0): Gender and mental health status are independent.
#Alternative hypothesis (H1): Gender and mental health status are dependent

chisq.test(gender_mental_health)

#p-value: ~0

#Result: Gender is statistically significantly associated with mental illness


#2. Gender & Mental illness among Native Americans
#SUbset for needed variables
shootings_select_2 <- shootings_v2%>%
   filter(gender=="male" | gender=="female")%>%
   filter(race=="N")

#Create contigency table for gender and mental health
gender_mental_health_2 <- table(shootings_select_2$gender, shootings_select_2$was_mental_illness_related)

#Null hypothesis (H0): Gender and mental health status are independent.
#Alternative hypothesis (H1): Gender and mental health status are dependent

chisq.test(gender_mental_health_2)

#p-value: 0.03597

#Result: Gender is statistically significantly associated with mental illness among Native Americans

#3. Age & Gender
#Create gender column in age dataset and remove non-typical entry
age_label_df_1 <- age_label_df

gender <- shootings_select$gender
age_label_df_1$gender <- gender

#How many non-typical gender entry
nrow(filter(age_label_df_1, gender!="male" & gender!="female"))

#Select only male and female
age_label_df_1 <- filter(age_label_df_1, gender=="male" | gender=="female")


#Remove all NAs
age_label_df_1 <- na.omit(age_label_df_1)

#Create contigency table for age and gender
age_gender <- table(age_label_df_1$label, age_label_df_1$gender)

#Null hypothesis (H0): Gender and mental health status are independent.
#Alternative hypothesis (H1): Gender and mental health status are dependent

chisq.test(age_gender)

#p-value: 0.9021

#Result: Gender is not statistically significantly associated with age

