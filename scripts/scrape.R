library(tidyverse)
library(rvest)
library(lubridate)
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
            
            #Q4.1  
            #name of the crime, from the Type field on each page. Make sure to get rid of trailing
            #whitespace and "\\n" in the crime type names.
            #ii. hour: the hour as an integer from 0 to 23, from the Date field. You might find
            #lubridate::parse_date_time() useful.
            #iii. neighborhood (the neighborhood name as a string)
            #Save crime_data to data/question_a2_1.csv in your submission directory.
            #Here are some helpful tips that might save you time:
             # . Make sure to get all data from all neighborhoods; some of the neighborhoods have multiple pages
            #of data or non-standard patterns.
            #. You may also find it helpful to use a for loop and combine to loop over the urls and combine
            #neighborhood tibbles into crime_data.            
            

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
           
          #  urls<-c(urls, new_url)
            #create Empty dataframe
           crime_data<- data.frame(matrix(ncol = 3, nrow = 0))
            x <- c("crime", "hour", "neighborhood")
            colnames(crime_data) <- x
          #   crime_data<-data.frame(crime = character(1), hour= character(1), neighborhood=character(1), stringsAsFactors = F)

          #loop over urls, get crime and hour data, attach neighborhood name and bind rows for each
           for (i in 1: nrow(neighborhood_df))
           { 
              # nh_name<-neighborhood_df$neighborhood_names[i]
               crime<-GetHour_CrimeType(neighborhood_df$urls[i])    
                #add neighbourhood
               crime<-cbind(crime,ifelse(is.na(neighborhood_df$neighborhood_names[i]),"",neighborhood_df$neighborhood_names[i]))
               colnames(crime) <- col
                #rowbind to add to original
                crime_data<-rbind(crime_data, crime,stringsAsFactors = FALSE)
            }
     
            
        crime_data<- crime_data%>%mutate(hour = as.numeric(hour))
           colnames(crime_data) <- col
           
           
        #  
            #testing with one link
            url3<-"https://www.universalhub.com/crime/south-end.html"
            #getting html from url
            response3<-read_html(url3)
            
            
            
            #getting hour of crime
            date <- rvest::html_nodes(x = response3,
                                               xpath = '//td[contains(@class, "date")]')
            date<-rvest::html_text(date, trim =T)
            hours<-as.integer(parse_date_time(date, "%m%d%y-%H:%M %p") %>% hour())
            
            #getting crime_type
            crime_type <- rvest::html_nodes(x = response3,
                                      xpath = '//td[contains(@class, "name")]')
            type<-rvest::html_text(crime_type, trim =T)
            trim.trailing <- function (x) sub("\\s+$", "", x)
            type<-trim.trailing(type)
           
           x<-cbind(type,hours) 