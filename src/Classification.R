
#Download and Import the dataset

#Download the file (already downloaded previously, no need to rerun)
#download.file("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/v2/fatal-police-shootings-data.csv",
#"v2_fatal-police-shootings-data.csv")

#Import the dataset
shootings_v2 <- read.csv("v2_fatal-police-shootings-data.csv")

#Inspect the dimension and column names
dim(shootings_v2)
names(shootings_v2)

#==================================Data preprocessing=========================================
library(dplyr)
library(rpart)
library(caret)
install.packages("rpart.plot")
#Select variables
shooting_classify <- select(shootings_v2, 3,4,5,14,15,17)

#Inspect the data
glimpse(shooting_classify)

#Check unique entries in each variable
unique(shooting_classify$threat_type)
unique(shooting_classify$flee_status)
unique(shooting_classify$armed_with)
unique(shooting_classify$race)
unique(shooting_classify$was_mental_illness_related)

#Filter the data and remove non-typical entries
shooting_classify <- filter(shooting_classify, race!="")
shooting_classify <- filter(shooting_classify, threat_type!="")
shooting_classify <- filter(shooting_classify, flee_status!="")
shooting_classify <- filter(shooting_classify, armed_with!="")
shooting_classify <- filter(shooting_classify, armed_with!="unknown")
shooting_classify <- filter(shooting_classify, armed_with!="other")

#shooting_classify <- filter(shooting_classify, armed_with!="blunt_object;knife")
#shooting_classify <- filter(shooting_classify, armed_with!="other;gun")
#shooting_classify <- filter(shooting_classify, armed_with!="blunt_object;blunt_object")
#shooting_classify <- filter(shooting_classify, armed_with!="vehicle;gun")
#shooting_classify <- filter(shooting_classify, armed_with!="vehicle;gun")


#For loop to recode values in 'armed_with' variable
k <- 1
for(i in shooting_classify$armed_with){
      if(i=="blunt_object;blunt_object") shooting_classify$armed_with[k]="blunt_object"
      else if(i=="blunt_object;knife") shooting_classify$armed_with[k]="knife;blunt_object"
      else if(i=="other;gun") shooting_classify$armed_with[k]="gun"
      else if(i=="vehicle;gun") shooting_classify$armed_with[k]="gun;vehicle"
      else if(i=="vehicle;knife;other") shooting_classify$armed_with[k]="knife;vehicle"
      k = k+1
}

#Convert all variables into factor
shooting_classify <- lapply(shooting_classify, factor)
shooting_classify <- as.data.frame(shooting_classify)
glimpse(shooting_classify)

#Plots to visualize variables
shooting_classify%>%
   group_by(race)%>%
   summarise(n=n())%>%
   ggplot(aes(race, n, fill=race,label=n))+geom_bar(stat = "identity")+ theme_bw()+
   labs(y="frequency", title = "Distribution of race")
   #geom_text(position = position_stack(vjust = 0.5))#This line adds the actual frequency atop each bar
ggsave("race_dist.png", width = 5.5)

shooting_classify%>%
   ggplot(aes(race, fill=flee_status))+geom_bar(position = "dodge")+ theme_bw()+
   labs(y="frequency", title = "Race by flee status of victims")
ggsave("race_vs_flee.png")
#===================================Classification task=========================================
#To predict the Race of victims.
#Create training and test data
train_set <- createDataPartition(shooting_classify$race, p=0.7, list=FALSE)
train <- shooting_classify[train_set,]
test <- shooting_classify[-train_set,]
dim(train); dim(test) 

#Build the model
tree_model <- rpart(race ~ was_mental_illness_related+threat_type+flee_status+armed_with, data=train, method="class")
summary(tree_model)

#Visualize the model
library(rpart.plot)
prp(tree_model)

#Predict
predict_shoot_train = predict(tree_model, data = train, type = "class")

#Confusion matrix
confision_matrix <- table(train$race, predict_shoot_train)
sum(diag(confision_matrix))/sum(confision_matrix)


#Test data
Predict_shoot_test = predict(tree_model, newdata = test, type = "class")

conf_mat <- table(test$race, Predict_shoot_test)

#Accuracy on test data
sum(diag(conf_mat))/sum(conf_mat)

#============================Classification Part 2===========================================
#Our first classification task was biased towards whites (W)
#Hence we reprocess the data by excluding some rows with the "W" race

# Obtain the row index for the "W" race
index_w <- as.numeric(rownames(subset(shooting_classify, shooting_classify$race=="W")))

# randomly select 2000 values from the index
index_w_2 <- sample(index_w, 2000)

# Remove the index from the data set
shoot_index <- shooting_classify[-index_w_2, ]

# Inspect frequency of race
shoot_index%>%
   group_by(race)%>%
   summarise(n=n())


# #To predict the Race of victims.
#Create training and test data
train_set_2 <- createDataPartition(shoot_index$race, p=0.7, list=FALSE)
train_2 <- shoot_index[train_set_2,]
test_2 <- shoot_index[-train_set_2,]
dim(train_2); 
dim(test_2) 

#Build the model
tree_model2 <- rpart(race ~ was_mental_illness_related+threat_type+flee_status+armed_with, data=train_2, method="class")
summary(tree_model2)

#Visualize the model
library(rpart.plot)
prp(tree_model2) #Tree diversified

#Predict on train data
predict_shoot_train2 = predict(tree_model2, data = train_2, type = "class")

#Confusion matrix
confision_matrix_2 <- table(train_2$race, predict_shoot_train2)
sum(diag(confision_matrix_2))/sum(confision_matrix_2)


#Test data
Predict_shoot_test2 = predict(tree_model2, newdata = test_2, type = "class")

conf_mat_2 <- table(test_2$race, Predict_shoot_test2)

#Accuracy on test data
sum(diag(conf_mat_2))/sum(conf_mat_2)





