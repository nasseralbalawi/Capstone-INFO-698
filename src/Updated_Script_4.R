#********************* Download Data set ************************************#
# Download the file shooting 
download.file("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/v2/fatal-police-shootings-data.csv",
              "v2_fatal-police-shootings-data.csv")

#Download the file shooting agencies
download.file("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/v2/fatal-police-shootings-agencies.csv",
              "v2_fatal-police-shootings-agencies.csv")

#**************** Reading,inspecting and wrangling the data *******************#
#Read in the file

shootings_v2 <- read.csv("v2_fatal-police-shootings-data.csv",na.strings = c("", NA)) 
agencies_v2 <- read.csv("v2_fatal-police-shootings-agencies.csv") 


#Inspect the shooting data
str(shootings_v2)
names(shootings_v2)

#Inspect the agencies data
str(agencies_v2)
names(agencies_v2)

#******************************************************
#Divide the data into two (Before and After COVID-19) *
#Before: Jan 2015- June 2019                          *
#During: Jan 2020- June 2023                          *
#******************************************************

library(ggplot2)
library(lubridate)
library(dplyr)
library(rpart)
library(rpart.plot)
library(tibble)
library(scales)
library(arsenal)
library(mice)
library(caret)
library(tidyverse)
library(gridExtra)





#**************** Data Processing ***************************
# Extracting the months and years
x <- ymd(shootings_v2$date)
Months <- month(x, label = T, abbr = F)
Year <- year(x)
month_year <- paste(Months, Year)

# Add new columns
shootings_v2 <- add_column(shootings_v2, month_year, .after = "date")
shootings_v2 <- add_column(shootings_v2, Year, .after = "date")

# Examining missing Shooting data
filter(shootings_v2, !complete.cases(shootings_v2))
apply(shootings_v2, 2, function(x) sum(is.na(x))) %>% sort(decreasing=TRUE)

# Examining missing Agencies data
filter(agencies_v2, !complete.cases(agencies_v2))
apply(agencies_v2, 2, function(x) sum(is.na(x))) %>% sort(decreasing=TRUE)


# Extracting Data Before COVID
Before_covid <- filter(shootings_v2, between(x, ymd("2016-01-01"), ymd("2019-06-30")))
Before_covid <- add_column(Before_covid, Period="Before COVID-19")

# Extracting Data During COVID
During_covid <- filter(shootings_v2, between(x, ymd("2020-01-01"), ymd("2023-06-30")))
During_covid <- add_column(During_covid, Period="During COVID-19")

#Before and During combined in one dataframe 
merged_Before_covid_During_covid <- bind_rows(Before_covid,During_covid)

# comparing both data sets
summary(comparedf(Before_covid, During_covid))

#**************** location & total missing values *************************
 
# location of missing values Before COVID
print("Position of missing values Before COVID : ")
which(is.na(Before_covid))
#count total missing values Before COVID
print("Count of total missing values Before COVID: ")
sum(is.na(Before_covid))

# location of missing values During COVID
print("Position of missing values During COVID : ")
which(is.na(During_covid))
#count total missing values During COVID
print("Count of total missing values During COVID: ")
sum(is.na(During_covid))

#Complete observations
nrow(Before_covid)
print("Complete Observations Before COVID : ")
nrow(na.omit(Before_covid))

nrow(During_covid)
print("Complete Observations During COVID : ")
nrow(na.omit(During_covid))


#************************* Gender *********************************
# Total number of Female and Male Before COVID
print("The Total number of  female  Before COVID is : ")
sum(Before_covid$gender == "female", na.rm=TRUE) 

print("The Total number of  male  Before COVID is : ")
sum(Before_covid$gender == "male", na.rm=TRUE) 

# Total number of Female and Male During COVID
print("The Total number of  female  During COVID is : ")
sum(During_covid$gender == "female", na.rm=TRUE) 

print("The Total number of  male  During COVID is : ")
sum(During_covid$gender == "male",na.rm=TRUE) 



# merged_Before_covid_During_covid 

merged_Before_covid_During_covid %>%
  filter(!is.na(gender) & gender != "non-binary")%>%
  group_by(Period)%>%count(gender)%>%
  mutate(perc=n/sum(n),tage=perc*100)%>%
  ggplot(aes(gender,tage, fill=gender))+geom_bar(width = 0.5, stat = "identity")+
  #scale_y_discrete(breaks=4)+
  facet_wrap(~factor(Period,levels = c("Before COVID-19", "During COVID-19")))+theme_bw()+
  labs(x="Gender",y="Percent (%)",title="Gender Distribution")+
  geom_text(aes(label=percent(perc)), nudge_y = 2.5)+
  theme(legend.position = "none")
ggsave("gender_covid_3.png")
#Males represent the most affected gender both before and after COVID-19



#*********************Distribution by age******************************************
# Calculating average using mean()
#Calculate average age before and after COVID-19
#mean(Before_covid$age, na.rm = T)
#mean(During_covid$age, na.rm = T)

#Mean/median age
mean(Before_covid$age, na.rm = T)
med <- median(Before_covid$age, na.rm = T)

#How many NAs are in age variable?
nn <- NULL
n1 <- 1
for(age in Before_covid$age){
  nn[n1] <- is.na(age)
  n1 = n1 + 1
}
#sum(nn) #137 NAs, 


#Mean/median age

mean(During_covid$age, na.rm = T)
med <- median(During_covid$age, na.rm = T)

#How many NAs are in age variable?
nn <- NULL
n1 <- 1
for(age in During_covid$age){
  nn[n1] <- is.na(age)
  n1 = n1 + 1
}
#sum(nn) #389 NAs, 


#Mean/median age

mean(merged_Before_covid_During_covid$age, na.rm = T)
med <- median(merged_Before_covid_During_covid$age, na.rm = T)

#How many NAs are in age variable?
nn <- NULL
n1 <- 1
for(age in merged_Before_covid_During_covid$age){
  nn[n1] <- is.na(age)
  n1 = n1 + 1
}
#sum(nn) # 526 NAs, 



#Age Distribution
png('baseHist1.png', width = 520)
par(mfrow=c(1,2))
hist(Before_covid$age, col="red",xlab = "Age",main = "Age distribution Before COVID-19")
hist(During_covid$age, col="red",xlab = "Age",main = "Age distribution During COVID-19")
dev.off()

#********************* Age Discretization ***************************************

#Discretization
#Before COVID
b4age <- filter(Before_covid, !is.na(age))
#Age groups with labels
b4age <- cut(b4age$age, 
            breaks = c(0, 25, 50, 75, 100), 
            labels=c("young", "grown", "mature", "old"))

#=====After COVID
afterage <- filter(During_covid, !is.na(age))
#Age groups with labels
afterage <- cut(afterage$age, 
             breaks = c(0, 25, 50, 75, 100), 
             labels=c("young", "grown", "mature", "old"))

as.data.frame(table(afterage))

png('baseage1.png', width = 580)
par(mfrow=c(1,2))
barplot(table(b4age), col = c("blue","green","orange","red"), main = "Age Distribution Before COVID-19",
        xlab = "Age categories", ylab = "Frequency")
barplot(table(afterage), col = c("blue","green","orange","red"), main = "Age Distribution After COVID-19",
        xlab = "Age categories", ylab = "Frequency")
dev.off()

#=======Merged
#Remove age NA from data
shoot_se <- filter(merged_Before_covid_During_covid, !is.na(age))

#Binning of age group
#Age groups with labels
age2 <- cut(shoot_se$age, 
                    breaks = c(0, 25, 50, 75, 100), 
                    labels=c("young", "grown", "mature", "old"))

png("agebar.png")
barplot(table(age2), col = c("blue","green","orange","red"), main = "Overall Age distribution",
        xlab = "Age categories", ylab = "Frequency")
dev.off()

#ggplot2
shoot_se1 <- select(shoot_se, 15,22)
shoot_se1$agecat <- cut(shoot_se1$age, 
            breaks = c(0, 25, 50, 75, 100), 
            labels=c("young", "grown", "mature", "old"))

shoot_se1 %>%
   group_by(Period, agecat)%>%count(agecat)%>%ungroup()%>%
   group_by(Period)%>%mutate(perc=n/sum(n),tage=perc*100)%>%
   ggplot(aes(agecat, tage,fill=agecat))+geom_bar(stat = "identity", width = 0.5)+
   facet_wrap(~factor(Period,levels = c("Before COVID-19", "During COVID-19")))+
   labs(x="Age Categories", y="Percent (%)", title = "Age Distribution")+
   geom_text(aes(label=percent(perc,accuracy = 0.1)), nudge_y = 2)+
   theme_bw()+ theme(panel.grid=element_blank())+
   theme(legend.position = "none")
ggsave("Agedist.png")



#**************** classification Decision tree ******************************
# Before COVID Age Tree
age_tree_before <- rpart(
  formula = race ~ age,
  data = Before_covid,
  parms = list(split = "information"),
  maxdepth = 3, #a tree with 2 levels
  method = "class"
)
# looking for the model 
age_tree_before
prp(age_tree_before, type = 4, extra = 109)

# During COVID Age Tree
age_tree_during <- rpart(
  formula = race ~ age,
  data = During_covid,
  parms = list(split = "information"),
  maxdepth = 3, #a tree with 2 levels
  method = "class"
)
# looking for the model 
age_tree_during
prp(age_tree_during, type = 4, extra = 109)



#********************* Threat type ********************************

unique(merged_Before_covid_During_covid$threat_type)

merged_Before_covid_During_covid%>%
  filter(!is.na(threat_type))%>%#12 Nas removed by filtering
  group_by(Period)%>%count(threat_type)%>%
  mutate(perc=n/sum(n),tage=perc*100)%>%
  ggplot(aes(threat_type,tage, fill=threat_type))+geom_bar(width = 0.7, stat = "identity")+
  facet_wrap(~factor(Period,levels = c("Before COVID-19", "After COVID-19")))+theme_bw()+
  labs(x="Threat type",y="Percent (%)",title="Threat types involved in police shootings")+
  facet_wrap(~factor(Period,levels = c("Before COVID-19", "During COVID-19")))+
  theme_bw()+theme(legend.position = "none")+
  geom_text(aes(label=percent(perc, accuracy = 0.01)), nudge_y = 0.5)+
  theme(axis.text.x = element_text(angle = 30, vjust = 1))
ggsave("threat_type1.png", width = 8, height = 6)



#********************* armed_with ***********************************

# Extracting unique armed_with
unique(shootings_v2$armed_with)
count(shootings_v2, armed_with, sort = TRUE)

# Define the grouping function
group <- function(string) {
  if (is.na(string)) return("NA")
  if (string == "unarmed") return("unarmed")
  else if (string == "undetermined") return("undetermined")
  else if (string == "vehicle") return("vehicle")
  else if (string %in% c("gun", "toy weapon", "gun and knife", "gun and car", "BB gun", "guns and explosives", "gun and vehicle", "hatchet and gun", "gun and sword", "machete and gun", "vehicle and gun", "pellet gun")) return("gun")
  else if (string %in% c("knife", "ax", "sword", "box cutter", "hatchet", "sharp object", "scissors", "meat cleaver", "pick-axe", "straight edge razor", "pitchfork", "chainsaw", "samurai sword", "spear")) return("sharpObject")
  else return("other")
}

# Apply the grouping function on each element of a vector
shootings_v2$armedType <- sapply(shootings_v2$armed_with, group)

# Before COVID armedType
Before_covid$armedType <- sapply(Before_covid$armed_with, group)

# During COVID armedType
During_covid$armedType <- sapply(During_covid$armed_with, group)


#************** agency_ids **********************************
# Extracting unique agency_ids
unique(Before_covid$agency_ids)
count(Before_covid, agency_ids, sort = TRUE)

# Plot shooting distribution by agency_ids for Before COVID
top_10_agencies <- Before_covid %>%
  count(agency_ids, sort = TRUE) %>%
  head(10)

# Plot the distribution of the top 10 agency_ids for Before_covid
ggplot(top_10_agencies, aes(x = reorder(agency_ids, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Top 10 Agency IDs (Before COVID-19)") +
  xlab("Agency IDs") +
  ylab("Frequency") +
  coord_flip()



# Extracting unique agency_ids during COVID
unique(During_covid$agency_ids)

# Plot shooting distribution by agency_ids for During COVID
top_10_agencies <- During_covid %>%
  count(agency_ids, sort = TRUE) %>%
  head(10)

# Plot the distribution of the top 10 agency_ids for Before_covid
ggplot(top_10_agencies, aes(x = reorder(agency_ids, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Top 10 Agency IDs (During COVID-19)") +
  xlab("Agency IDs") +
  ylab("Frequency") +
  coord_flip()



# Agency ids Associated with the death record(Name of victim)

# Before COVID select the top 3 (80,38,20)
Agent_Before<- Before_covid$name[Before_covid$agency_ids==80]
print(Agent_Before)

Agent_Before<- Before_covid$name[Before_covid$agency_ids==38]
print(Agent_Before)

Agent_Before<- Before_covid$name[Before_covid$agency_ids==20]
print(Agent_Before)

# During COVID select the top 3 (38,20,102)
Agent_During<- During_covid$name[During_covid$agency_ids==38]
print(Agent_During)

Agent_During<- During_covid$name[During_covid$agency_ids==20]
print(Agent_During)

Agent_During<- During_covid$name[During_covid$agency_ids==102]
print(Agent_During)


#******************* was_mental_illness *********************************
unique(Before_covid$was_mental_illness_related)
#count(Before_covid, was_mental_illness_related, sort = TRUE)
print("The Total number of  Mental Illness value True  Before COVID is : ")
sum(Before_covid$was_mental_illness_related == "True") 

print("The Total number of  Mental Illness value False  Before COVID is : ")
sum(Before_covid$was_mental_illness_related == "False") 

# During Covid 
unique(During_covid$was_mental_illness_related)
print("The Total number of  Mental Illness value True  During COVID is : ")
sum(During_covid$was_mental_illness_related == "True") 

print("The Total number of  Mental Illness value False  During COVID is : ")
sum(During_covid$was_mental_illness_related == "False") 



unique(merged_Before_covid_During_covid$was_mental_illness_related)

table(Before_covid$was_mental_illness_related)
table(During_covid$was_mental_illness_related)

#Table for comparison
merged_Before_covid_During_covid %>%
  group_by(Year)%>%count(was_mental_illness_related)%>%
  mutate(perc=n/sum(n),tage=perc*100)

Before_covid %>%count(was_mental_illness_related)%>%
  mutate(perc=n/sum(n),tage=perc*100)

During_covid %>%count(was_mental_illness_related)%>%
  mutate(perc=n/sum(n),tage=perc*100)

#Test of proportion
prop.test(x = c(1181, 560), n = c(4925, 3430))
prop.test(x = c(1181, 560), n = c(4925, 3430), alternative = "less")
prop.test(x = c(1181, 560), n = c(4925, 3430), alternative = "greater")


#Test of proportion
prop.test(x = c(816, 586), n = c(3415, 3659)) # x= vector of successes (Trues),
#n= vector of total counts
table(Before_covid$was_mental_illness_related)
table(During_covid$was_mental_illness_related)
prop.test(table(Before_covid$was_mental_illness_related))
prop.test(table(During_covid$was_mental_illness_related))

#Test of proportion mental_illness_relatedand age
unique(merged_Before_covid_During_covid$gender)

Gender_Before <-filter(Before_covid, !is.na(gender) & gender!="non-binary")
Gender_During <-filter(During_covid, !is.na(gender) & gender!="non-binary")
glimpse(Gender_Before)
glimpse(Gender_During)

table(Gender_Before$was_mental_illness_related, Gender_Before$gender)
table(Gender_During$was_mental_illness_related, Gender_During$ gender)

prop.test(table(Gender_Before$was_mental_illness_related, Gender_Before$gender))
prop.test(table(Gender_During$was_mental_illness_related, Gender_During$ gender))

# T Test
#t.test(table(Before_covid$was_mental_illness_related, Before_covid$gender))
#t.test(table(During_covid$was_mental_illness_related, During_covid$gender))

chisq.test(table(Gender_Before$was_mental_illness_related, Gender_Before$gender))
chisq.test(table(Gender_During$was_mental_illness_related, Gender_During$ gender))

# Age & Gender
table(Before_covid$age, Before_covid$gender)
table(During_covid$age, During_covid$gender )
#Test
chisq.test(table(Before_covid$age, Before_covid$gender))
# Chi-squared approximation may be incorrect
chisq.test(table(During_covid$age, During_covid$gender))
# Chi-squared approximation may be incorrect

#Race & mental
table(Before_covid$race, Before_covid$was_mental_illness_related)
table(During_covid$race, During_covid$was_mental_illness_related)

chisq.test(table(Before_covid$race, Before_covid$was_mental_illness_related))
chisq.test(table(During_covid$race, During_covid$was_mental_illness_related))

#Age & mental
table(Before_covid$age, Before_covid$was_mental_illness_related)
table(During_covid$ age,During_covid$was_mental_illness_related)

chisq.test(table(Before_covid$was_mental_illness_related, Before_covid$age ))
chisq.test(table(During_covid$age, During_covid$was_mental_illness_related))




#Table for comparison
merged_Before_covid_During_covid %>%
  group_by(Year)%>%count(was_mental_illness_related)%>%
  mutate(perc=n/sum(n),tage=perc*100)


#Plot
merged_Before_covid_During_covid %>%
  group_by(Period)%>%count(was_mental_illness_related)%>%
  mutate(perc=n/sum(n),tage=perc*100)%>%
  ggplot(aes(was_mental_illness_related,tage, fill=was_mental_illness_related))+geom_bar(width = 0.5, stat = "identity")+
  facet_wrap(~factor(Period,levels = c("Before COVID-19", "After COVID-19")))+theme_bw()+
  labs(x="Mental Illness",y="Percent (%)",title="Impact of mental health on police shootings")+
  facet_wrap(~factor(Period,levels = c("Before COVID-19", "During COVID-19")))+
  theme_bw()+theme(legend.position = "none")+
  geom_text(aes(label=percent(perc)), nudge_y = 2)
ggsave("mental_illness.png")   

#Test of proportion
prop.test(x = c(816, 586), n = c(3415, 3659)) # x= vector of successes (Trues),
#n= vector of total counts



#******************************Top & Least States *****************

# Print glimpses of the data
glimpse(shootings_v2)
glimpse(Before_covid)
glimpse(During_covid)

# Find incident count for each of the states
state_count <- arrange(count(Before_covid, state), desc(n))
state_count_before <- arrange(count(Before_covid, state), desc(n))
state_count_during <- arrange(count(During_covid, state), desc(n))

# Before
Before_covid %>%
  group_by(state)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>% head(10)%>%
  ggplot(aes(state, n / 3.5, fill=state))+ geom_bar(stat = "identity")+theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Top 10 states affected by police shootings before COVID-19",y="Annual Frequency",x="State")
ggsave("Before COVID-19_least10states.png")

#During 
During_covid %>%
  group_by(state)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>% head(10)%>%
  ggplot(aes(state, n / 3.5, fill=state))+ geom_bar(stat = "identity")+theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Top 10 states affected by police shootings During COVID-19",y="Annual Frequency",x="State")
ggsave("During COVID-19_least10states.png")

merged_Before_covid_During_covid%>%
  group_by(state, Period)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>% head(10)%>%
  ggplot(aes(state, n/3.5))+geom_segment(aes(xend=state),yend=0, colour="grey50")+
  geom_point(size=3, aes(colour=Period))+
  scale_colour_brewer(palette = "Set1")+
  facet_grid(~Period)+
  theme_bw()+theme(legend.position = "none")+
  labs(title="Top 10 states affected by police shootings",y="Annual Frequency",x="State")
ggsave("Leaststates.png")


#=====================Least 10 states with police shooting record===================

Before_covid %>%
  group_by(state)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>% tail(10)%>%
  ggplot(aes(state, n / 3.5, fill=state))+ geom_bar(stat = "identity")+theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Least 10 states affected by police shootings before COVID-19",y="Annual Frequency",x="State")
ggsave("Before COVID-19_least10states.png")

##############
During_covid %>%
  group_by(state)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>% tail(10)%>%
  ggplot(aes(state, n / 3.5, fill=state))+ geom_bar(stat = "identity")+theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Least 10 states affected by police shootings During COVID-19",y="Annual Frequency",x="State")
ggsave("During COVID-19_least10states.png")

merged_Before_covid_During_covid%>%
  group_by(state, Period)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>% tail(10)%>%
  ggplot(aes(state, n))+geom_segment(aes(xend=state),yend=0, colour="grey50")+
  geom_point(size=3, aes(colour=Period))+
  scale_colour_brewer(palette = "Set1")+
  facet_grid(~Period)+
  theme_bw()+theme(legend.position = "none")+
  labs(title="Least states affected by police shootings",y="Annual Frequency",x="State")
ggsave("Leaststates.png")

merged_Before_covid_During_covid %>%
  group_by(state, Period) %>%
  summarize(count = n()) %>%
  group_by(state) %>%
  mutate(percentage = count / sum(count) * 100)%>%
  ggplot(aes(x = state, y = percentage, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "State", y = "Percentage", fill = "Period") +
  theme_minimal()+
  theme(legend.position = "top", legend.justification = "right")



#=======================================States affected=================================

#Top States

b4state <- Before_covid%>%
  #filter(Year==2016)%>%
  group_by(Year)%>%
  count(state)%>%
  ungroup()%>%
  group_by(state)%>%
  mutate(avgg=mean(n))%>%
  select(-c(1,3))%>%
  unique.data.frame()%>%
  arrange(desc(avgg))%>%head()%>%
  ggplot(aes(state, avgg))+geom_segment(aes(xend=state),yend=0, colour="grey50")+
  geom_point(size=3, colour="blue")+theme_bw()+
  labs(title = "Before COVID-19", y="Average per year",x="State")


afterstate <- During_covid%>%
  #filter(Year==2016)%>%
  group_by(Year)%>%
  count(state)%>%
  ungroup()%>%
  group_by(state)%>%
  mutate(avgg=mean(n))%>%
  select(-c(1,3))%>%
  unique.data.frame()%>%
  arrange(desc(avgg))%>%head()%>%
  ggplot(aes(state, avgg))+geom_segment(aes(xend=state),yend=0, colour="grey50")+
  geom_point(size=3, colour="cyan")+theme_bw()+
  labs(title = "During COVID-19", y="Average per year",x="State")


png("states_top_1.png")
grid.arrange(b4state, afterstate, ncol=1, top="TOP STATES AFFECTED BY POLICE SHOOTINGS")
dev.off()

#Least States
b4stateleast <- Before_covid%>%
  group_by(Year)%>%
  count(state)%>%
  ungroup()%>%
  group_by(state)%>%
  mutate(avgg=mean(n))%>%
  select(-c(1,3))%>%
  unique.data.frame()%>%
  arrange(desc(avgg))%>%tail()%>%
  ggplot(aes(state, avgg))+geom_segment(aes(xend=state),yend=0, colour="grey50")+
  geom_point(size=3, colour="blue")+theme_bw()+
  labs(title = "Before COVID-19", y="Average per year",x="State")


afterstateleast <- During_covid%>%
  group_by(Year)%>%
  count(state)%>%
  ungroup()%>%
  group_by(state)%>%
  mutate(avgg=mean(n))%>%
  select(-c(1,3))%>%
  unique.data.frame()%>%
  arrange(desc(avgg))%>%tail()%>%
  ggplot(aes(state, avgg))+geom_segment(aes(xend=state),yend=0, colour="grey50")+
  geom_point(size=3, colour="cyan")+theme_bw()+
  labs(title = "During COVID-19", y="Average per year",x="State")


png("states_least.png")
grid.arrange(b4stateleast, afterstateleast, ncol=1, top="STATES LEAST AFFECTED BY POLICE SHOOTINGS")
dev.off()

#Average shooting per state
merged_Before_covid_During_covid%>%
  group_by(Year, Period)%>%
  count(state)%>%
  ungroup()%>%
  group_by(Period, state)%>%
  mutate(avg=mean(n))%>%
  select(-c(1,4))%>%
  unique.data.frame()%>%
  ggplot(aes(state, avg, fill=factor(Period,levels = c("Before COVID-19", "During COVID-19"))))+
  #scale_fill_manual(c("blue","cyan"))+
  geom_bar(width=0.5, stat = "identity", position = "dodge")+
  theme(legend.title = element_text(face = "italic"))+
  guides(fill=guide_legend(title = "Period"))+
  theme_bw()+
  labs(x="State",y="Average per period ", title = "Average Incidence of Police Shootings per State")+
  theme(panel.grid=element_blank())

ggsave("avgstateperperiod.png", width = 15, height = 10)
ggsave("avgstateperperiod_1.png", width = 20, height = 10)  


#****************** Data preprocessing for classification ****************************


#Select variables(threat_type, flee_statu, armed_with, age. gender,race, mental)
shoot_class <- select(merged_Before_covid_During_covid, 5,6,7,15,16,17,19)#7074

#Remove NAs
shoot_class <- na.omit(shoot_class)

#Inspect the data
glimpse(shoot_class)

#Check unique entries in each variable
unique(shoot_class$threat_type)
unique(shoot_class$flee_status);unique(shoot_class$armed_with)
unique(shoot_class$race); unique(shoot_class$was_mental_illness_related)
unique(shoot_class$age); unique(shoot_class$gender)

#Remove NAs
shoot_class <- filter(shoot_class, !is.na(threat_type) & !is.na(flee_status) & !is.na(armed_with)& 
                                                             !is.na(age) & !is.na(gender) & !is.na(was_mental_illness_related))


#For loop to recode values in 'armed_with' variable
k <- 1
for(i in shoot_class$armed_with){
  if(i=="blunt_object;blunt_object") shoot_class$armed_with[k]="blunt_object"
  else if(i=="blunt_object;knife") shoot_class$armed_with[k]="knife;blunt_object"
  else if(i=="vehicle;gun") shoot_class$armed_with[k]="gun;vehicle"
  #else if(i=="vehicle;knife;other") shoot_class$armed_with[k]="knife;vehicle"
  k = k+1
}

#Convert variables into factor
shoot_class$threat_type <- as.factor(shoot_class$threat_type)
shoot_class$flee_status <- as.factor(shoot_class$flee_status)
shoot_class$armed_with <- as.factor(shoot_class$armed_with)
shoot_class$gender <- as.factor(shoot_class$gender)
shoot_class$race <- as.factor(shoot_class$race)
shoot_class$was_mental_illness_related <- as.factor(shoot_class$was_mental_illness_related)

glimpse(shoot_class)

#Plots to visualize variables
shoot_class%>%
  group_by(race)%>%
  summarise(n=n())%>%
  mutate(perc=n/sum(n),tage=perc*100)%>%
  ggplot(aes(race, tage, fill=race))+geom_bar(stat = "identity")+ theme_bw()+
  labs(y="Percent (%)", title = "Distribution of race")+ theme(legend.position = "none")+
  geom_text(aes(label=percent(perc)), nudge_y = 2)
ggsave("race_dist.png", width = 5.5)



#===============Classification 1 (predict the Race of victims)=============
#To predict the Race of victims.
#Create training and test data
train_set <- createDataPartition(shoot_class$race, p=0.7, list=FALSE)
train <- shoot_class[train_set,]
test <- shoot_class[-train_set,]
dim(train); dim(test)

#Build the model
tree_model <- rpart(race ~ was_mental_illness_related+threat_type+flee_status+armed_with+gender+age, data=train, method="class")
summary(tree_model)

#Visualize the model

prp(tree_model)

#Predict
predict_shoot_train = predict(tree_model, data = train, type = "class")

#Confusion matrix
confusion_matrix <- table(train$race, predict_shoot_train)
sum(diag(confusion_matrix))/sum(confusion_matrix)


#Test data
Predict_shoot_test = predict(tree_model, newdata = test, type = "class")

conf_mat <- table(test$race, Predict_shoot_test)

#Accuracy on test data
sum(diag(conf_mat))/sum(conf_mat)



#******************* Classification Part 2 *****************************
#first classification task was biased towards whites (W)
#Hence reprocess the data by excluding some rows with the "W" race

# Obtain the row index for the "W" race
index_w <- as.numeric(rownames(subset(shoot_class, shoot_class$race=="W")))

# randomly select 2000 values from the index
index_w_2 <- sample(index_w, 2000)

# Remove the index from the data set
shoot_index <- shoot_class[-index_w_2, ]

# Inspect frequency of race
shoot_index%>%
  group_by(race)%>%
  summarise(n=n())


# To predict the Race of victims.
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

#Race 
train_set <- createDataPartition(shoot_class$race, p=0.7, list=FALSE)
train <- shoot_class[train_set,]
test <- shoot_class[-train_set,]
dim(train); dim(test)

#Build the model
tree_model <- rpart(race ~ was_mental_illness_related+threat_type+flee_status+armed_with+gender+age, data=train, method="class")
summary(tree_model)

#Visualize the model

prp(tree_model)

#Predict
predict_shoot_train = predict(tree_model, data = train, type = "class")

#Confusion matrix
confusion_matrix <- table(train$race, predict_shoot_train)
sum(diag(confusion_matrix))/sum(confusion_matrix)


#Test data
Predict_shoot_test = predict(tree_model, newdata = test, type = "class")

conf_mat <- table(test$race, Predict_shoot_test)

#Accuracy on test data
sum(diag(conf_mat))/sum(conf_mat)


#=================================Test of Association==========================================
# using the chi-squared test to test the association between the following
#1. Gender & Mental illness
#2. Gender & Mental illness among Native Americans
#3. Age & Gender

#1. Gender & Mental illness
#Subset for needed variables and remove non-typical entry
shootings_select_1 <- Before_covid%>%
  filter(gender=="male" | gender=="female")

#Create contingency table for gender and mental health
gender_mental_health <- table(shootings_select_1$gender, shootings_select_1$was_mental_illness_related)
table(shootings_select_1$gender, shootings_select_1$was_mental_illness_related)
#Null hypothesis (H0): Gender and mental health status are independent.
#Alternative hypothesis (H1): Gender and mental health status are dependent

chisq.test(gender_mental_health)

#p-value: ~0

#Result: Gender is statistically significantly associated with mental illness


#2. Gender & Mental illness among Native Americans
#SUbset for needed variables
shootings_select_2 <- Before_covid%>%
  filter(gender=="male" | gender=="female")%>%
  filter(race=="N")

#Create contigency table for gender and mental health
gender_mental_health_2 <- table(shootings_select_2$gender, shootings_select_2$was_mental_illness_related)

#Null hypothesis (H0): Gender and mental health status are independent.
#Alternative hypothesis (H1): Gender and mental health status are dependent

chisq.test(gender_mental_health_2)

#p-value: incorrect

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

#p-value: incorrect

#Result: Gender is not statistically significantly associated with age

















