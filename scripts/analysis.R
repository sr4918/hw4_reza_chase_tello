library(tidyverse)
library(rvest)
library(lubridate)
library(RecordLinkage)
library(ggplot2)
setwd("C:/Users/samee/Dropbox/NYU-PhD/3. Fall 2019/Messy Data and ML/Assignment 4")


#Question A2.2

#In analysis.R, read in the data you saved in the previous step, and create a tibble with two columns:
# crime and count, where crime is each unique crime type, and count is the number of times the corresponding
#crime type occurred (aggregated across neighborhoods and hours). Be alert for misspellings
#of crime types!
# 3. Sort the rows in this tibble in descending order by count, and save the rows for the five most common
#crime types to data/question_a2_2.csv within your submission directory.

    A2.2<-read_csv("hw4_Reza/data/question_a2_1.csv")
    
    # crime and count, where crime is each unique crime type, and count is the number of times the corresponding
    #crime type occurred (aggregated across neighborhoods and hours). Be alert for misspellings
    #of crime types!
    unique(A2.2$crime)
    A2.2<-A2.2%>%mutate(crime = tolower(crime))
    
    #only shooting has been misspelled in data
    A2.2<-A2.2%>%mutate(crime = ifelse(soundex(crime)==soundex('shooting'),"shooting",crime))
    #above changes all except "shootin"
    A2.2<-A2.2%>%mutate(crime = ifelse(soundex(crime)==soundex('shootin'),"shooting",crime))
    
    A2.3#Sort the rows in this tibble in descending order by count, and save the rows for the five most common
        #crime types to data/question_a2_2.csv within your submission directory.
     #counting incidents aggregated across neibourhoods and hours
    A2.3_summary<-A2.2%>%group_by(crime)%>% summarise(count = n()) %>%arrange(desc(count))
    
    A2.3_top5<-A2.3_summary%>%slice(1:5)
    
    write_csv(A2.3_top5, "hw4_Reza/data/question_a2_2.csv")
    
    
#Question A3
   #A3.1
    # Make a plot visualizing the total number of crimes (aggregated across neighborhoods and crime types)
    #on the y-axis and the hour of day on the x-axis, and save this figure to figures/question_a3_1.png.
    A3.1<-A2.2%>%group_by(hour)%>%summarize(count = n())
    g3.1 <- ggplot(A3.1, aes(hour,count))
    # Number of cars in each class:
    g3.1<- g3.1 + geom_col(aes(fill = "red"))  +labs(title="No of Crimes by Hour of Day", x="Hour", y="Crime Count")
    ggsave("hw4_Reza/figures/question_a3_1.png",g3.1)
    
    #A3.2
    
    
    