# Author Nasser Albalawi										   
# Date Created: 04/15/2023	
# The capstone (INFO 698)
# Descrption: This script will. 
# Date: 04/15/2023
#
setwd("~/Desktop/R_programming")
##============================Getting the data======================================
#Download the file
download.file("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/567ceb1fd1769bb27fc8740de808400a73fbdf56/v1/fatal-police-shootings-data.csv",
              "v1_fatal-police-shootings-data.csv")

#Download the file
download.file("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/v2/fatal-police-shootings-data.csv",
              "v2_fatal-police-shootings-data.csv")

##================Reading,inspecting and wrangling the data==========================
#Read in the file
#shootings <- read.csv("v1_fatal-police-shootings-data.csv")              
shootings_v2 <- read.csv("v2_fatal-police-shootings-data.csv")  

#Inspect the data
str(shootings_v2)
names(shootings_v2)

#Divide the data into two (Before and After COVID-19)
#Before: 2015- November 2019
#After: December 2019- Present

library(lubridate)
library(dplyr)

#Extracting the months and years
x<- ymd(shootings_v2$date)
Months <- month(x, label = T,abbr = F)
Year <- year(x)
month_year <- paste(Months, Year)
#month_year <- my(month_year)

#Add new columns
library(tibble)
shootings_v2 <- add_column(shootings_v2, month_year, .after = "date")
shootings_v2 <- add_column(shootings_v2, Year, .after = "date")

#After COVID

after <- filter(shootings_v2, Year>=2020)
after1 <- filter(shootings_v2, month_year=="December 2019")
After_covid <- bind_rows(after, after1)
After_covid <- add_column(After_covid, Period="After COVID-19")

#Before COVID
b4 <- filter(shootings_v2, Year<2020)
Before_covid <- filter(b4, month_year!="December 2019")
Before_covid <- add_column(Before_covid, Period="Before COVID-19")

merged_Before_covid_After_covid <- bind_rows(Before_covid, After_covid)

#=================================Plottings==========================================

library(ggplot2)

##=====================Top 10 states with police shooting record=====================

Before_covid %>%
      group_by(state)%>%
      summarise(n=n())%>%
      arrange(desc(n))%>% head(10)%>%
      ggplot(aes(state, n, fill=state))+ geom_bar(stat = "identity")+theme_bw()+
      theme(legend.position = "none")+
      labs(title = "Top 10 states before COVID-19",y="Frequency",x="State")
ggsave("b4_top10states.png")

After_covid %>%
      group_by(state)%>%
      summarise(n=n())%>%
      arrange(desc(n))%>% head(10)%>%
      ggplot(aes(state, n, fill=state))+ geom_bar(stat = "identity")+theme_bw()+
      theme(legend.position = "none")+
      labs(title = "Top 10 states after COVID-19",y="Frequency",x="State")
ggsave("after_top10states.png")
#CA is the most affected state before and after COVID-19

#Dotplot
#scale_colour_brewer(palette = "Set1", limits = c("NL", "AL")) +
merged_Before_covid_After_covid%>%
      group_by(state, Period)%>%
      summarise(n=n())%>%
      arrange(desc(n))%>% head(10)%>%
      ggplot(aes(state, n))+geom_segment(aes(xend=state),yend=0, colour="grey50")+
      geom_point(size=3, aes(colour=Period))+
      scale_colour_brewer(palette = "Set1")+
      facet_grid(~Period)+
      theme_bw()+theme(legend.position = "none")+
      labs(title="Top states affected by police shootings",y="Frequency",x="State")
ggsave("topstates.png")      
      


#=====================Least 10 states with police shooting record===================

Before_covid %>%
      group_by(state)%>%
      summarise(n=n())%>%
      arrange(desc(n))%>% tail(10)%>%
      ggplot(aes(state, n, fill=state))+ geom_bar(stat = "identity")+theme_bw()+
      theme(legend.position = "none")+
      labs(title = "Least 10 states affected by police shootings before COVID-19",y="Frequency",x="State")
ggsave("b4_least10states.png")

After_covid %>%
      group_by(state)%>%
      summarise(n=n())%>%
      arrange(desc(n))%>% tail(10)%>%
      ggplot(aes(state, n, fill=state))+ geom_bar(stat = "identity")+theme_bw()+
      theme(legend.position = "none")+
      labs(title = "Least 10 states affected by police shootings after COVID-19",y="Frequency",x="State")
ggsave("after_least10states.png")

#==============================Gender distribution==================================

Before_covid %>%
      # Remove poorly formatted gender entry by filter()
      filter(gender != "")%>% 
      group_by(gender)%>%
      summarise(n=n())%>%
      ggplot(aes(gender, n, fill=gender))+ geom_bar(stat = "identity",width = 0.5)+theme_bw()+
      theme(legend.position = "none")+
      labs(title = "Gender affected before COVID-19",y="Frequency",x="Gender")
ggsave("b4_gender.png")

After_covid %>%
      # Remove poorly formatted gender entry by filter()
      filter(gender != "" & gender != "non-binary")%>% 
      group_by(gender)%>%
      summarise(n=n())%>%
      ggplot(aes(gender, n, fill=gender))+ geom_bar(stat = "identity",width = 0.5)+theme_bw()+
      theme(legend.position = "none")+
      labs(title = "Gender affected after COVID-19",y="Frequency",x="Gender")
ggsave("after_gender.png")
#Males represent the most affected gender both before and after COVID-19

#========================Representation of mental illness===========================

merged_Before_covid_After_covid%>%
      group_by(was_mental_illness_related, Period)%>%
      summarise(n=n())%>%
      ggplot(aes(was_mental_illness_related, n, fill=was_mental_illness_related))+ geom_bar(stat = "identity",width = 0.5)+
      facet_wrap(~Period)+ 
      labs(x="Mental Illness",y="Frequency","Impact of mental health on police shootings")+
      theme_bw()+theme(legend.position = "none")
ggsave("Mental_illness.png")
#No significant difference

#=========================Age Distribution==========================================
#Mean/median age
mean(shootings_v2$age)
mean(shootings_v2$age, na.rm = T)
med <- median(shootings_v2$age, na.rm = T)

#How many NAs are in age variable?
nn <- NULL
n1 <- 1
for(age in shootings_v2$age){
     nn[n1] <- is.na(age)
     n1 = n1 + 1
}
sum(nn) #549 NAs, is quite large

#===================================Not recommended=======================================
#This segment of code substitutes NA values in age with overall mean age
#You may skip this segment of code and not susbtitute NA age values with mean.
#This will prevent a unimodal form of distribution in the histogram


i <- 1

for(age in merged_Before_covid_After_covid$age){
      if(is.na(age))
            merged_Before_covid_After_covid$age[i] <- 37
      i = i + 1
}
#============================Graphical Illustrations continued=====================

png("baseHist.png")
hist(shootings_v2$age, col="red",xlab = "Age",main = "Histogram of age distribution")
dev.off()

merged_Before_covid_After_covid%>%
      ggplot(aes(age))+geom_histogram(fill="blue",col="black")+theme_bw()+
      labs(x="Age",y="Frequency",title = "Histogram of age")
ggsave("hist_age.png")

shootings_v2%>%
      ggplot(aes(age))+geom_histogram(fill="red",col="black")+theme_bw()+
      labs(x="Age",y="Frequency")
ggsave("hist_age1.png")
#================================Threat type=======================================

mean(merged_Before_covid_After_covid$age)

merged_Before_covid_After_covid%>%
      filter(threat_type != "" & threat_type != "undetermined")%>%
      ggplot(aes(threat_type,fill=threat_type))+geom_bar(width = 0.7)+theme_bw()+
      theme(legend.position = "none")+
      labs(x="Threat type",y="Frequency",title = "Threat type involved in police shootings")+
      facet_wrap(~Period)+
      theme(axis.text.x = element_text(angle = 30, vjust = 1))
ggsave("threat_type.png")
