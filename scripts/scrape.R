library(tidyverse)
library(rvest)
library(lubridate)
#setwd("C:/Users/samee/Dropbox/NYU-PhD/3. Fall 2019/Messy Data and ML/Assignment 4")

#Assignment 4
            #function to trim trailing white spaces
            trim.trailing <- function (x) sub("\\s+$", "", x)
            
            #function to get crime hour and name from url           
                        GetHour_CrimeType<-function(url)
                        {
                          response<-read_html(url)
                          date <- rvest::html_nodes(x = response,
                                                    xpath = '//td[contains(@class, "date")]')
                          date<-rvest::html_text(date, trim =T)
                          hours<-as.integer(parse_date_time(date, "%m%d%y-%H:%M %p") %>% hour())
                          
                          crime_type <- rvest::html_nodes(x = response,
                                                          xpath = '//td[contains(@class, "name")]')
                          type<-rvest::html_text(crime_type, trim =T)
                          type<-trim.trailing(type)
                          crime_df<-cbind(type, hours )
                          names(crime_df)<-c("name", "hour")
                          return (crime_df)
                        }
            #function to get pages associated with URL
                        
                        get_pages<-function(url)
                        {
                          res <- read_html(url) 
                          nod<-html_nodes(x= res, xpath = '//*[contains(@class, "pager")]')
                          t<-rvest::html_text(nod, trim =T)
                          if(length(t)>0)
                            t<-as.numeric(t[(length(t)-2)])
                          else t<-0
                          return (t)
                          
                        }           
                        
#QA2.1  
            #name of the crime, from the Type field on each page. 
            #hour
            #neighborhood
            #Save crime_data to data/question_a2_1.csv in your submission directory.
            
            url2<-"https://www.universalhub.com/crime"
            url2_home<-"https://www.universalhub.com"
            #getting html from url
            response<-read_html(url2)
          
          
            #getting url names and neighbourhood names
            neighborhood <- rvest::html_nodes(x = response, xpath = '//option[contains(@class, "d-1")]')
            neighborhood_names<-rvest::html_text(neighborhood, trim =T)
            neighborhood_names<-neighborhood_names[1:20]
           
            #col names
            col<-c("crime","hour" ,"neighborhood")       
            
             #get all urls
            urls<- rvest::html_nodes(x = response, xpath = '//option[contains(@class, "d-1")]')
            urls<-html_attr(x= urls, 'value')
            urls<-urls[1:20]
            urls<-paste(url2_home,urls,sep = "")
            urls<-gsub(" ", "", urls, fixed = TRUE)   
            pages<-0
            neighborhood_df<-data.frame(neighborhood_names, urls, pages, stringsAsFactors = F)
           
            
            #check if any url has more than one page, if so add related urls to list
            for (i in 1:length(urls)){
              npages<-get_pages(urls[i])
              neighborhood_df$pages[i]<-npages
              if(npages>0)
                for(j in 1:(npages-1))
                {
                  f <- data.frame( neighborhood_names= character(1), urls = character(1), pages= numeric(1), stringsAsFactors = FALSE)
                  f$neighborhood_names<-neighborhood_df$neighborhood_names[i]
                  f$urls<-paste(urls[i], "?page=", j, sep = "")
                  f$pages<-j
                 # new_url<-paste(i, "?page=", j, sep = "")
                  neighborhood_df<-rbind(neighborhood_df, f)      
                }
              }            
           
          #create Empty dataframe
           crime_data<- data.frame(matrix(ncol = 3, nrow = 0))
            x <- c("crime", "hour", "neighborhood")
            colnames(crime_data) <- x
          
          #loop over all urls in neighborhood_df, get crime and hour data, attach neighborhood name and bind rows for each
           for (i in 1: nrow(neighborhood_df))
           { 
               crime<-GetHour_CrimeType(neighborhood_df$urls[i])    
                #add neighbourhood
               crime<-cbind(crime,neighborhood_df$neighborhood_names[i])
               colnames(crime) <- col
               #rowbind to add to original
               crime_data<-rbind(crime_data, crime,stringsAsFactors = FALSE)
            }
     
            
        crime_data<- crime_data%>%mutate(hour = as.integer(hour))
        write_csv(crime_data, "hw4_reza_chase_tello/data/question_a2_1.csv")
           
           
        