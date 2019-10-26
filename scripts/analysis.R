library(tidyverse)
library(rvest)
library(lubridate)
library(RecordLinkage)
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
    