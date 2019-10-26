library(tidyverse)
library(rvest)
library(lubridate)


url<-"https://www.universalhub.com/crime/murder/2018"
#getting html from url
      response<-read_html(url)

#getting specific node containing information
      victim_names<-rvest::html_nodes(x= response, xpath = '//td[contains(@class,"victim-name")]')

#getting the values from the nodes
      victim_names<-rvest::html_text(victim_names, trim =T)
#write a function to do this
      extract_all_victim_names <- function(parsed_html){
        vict_name_els<-rvest::html_nodes(x = parsed_html, xpath = '//td[contains(@class,"victim-name")]')
        vict_names<-rvest::html_text(vict_name_els, trim =T)
        vict_names
        
      }
      
# get links to corresponding elements
      a_els<-html_nodes(x = response, xpath = '//td[contains(@class,"title")]')
      #link information is one tree below
      hrefs<-html_attr(x= html_children(a_els), 'href')
      links<- paste0('https://www.universalhub.com', hrefs)
      
# now grab article text from all the links above
      story_extractor<-function(url)
      {
        content<-read_html(url)
        text<-html_nodes(x= content, xpath = '(//p)[position()<3]') #position added to filter extra information
        text<-html_text(text)
        text<-str_c(text, collapse =' ') #merge teh 2 parts
        text
      }
      story_extractor(links[1])

#put all information from all links in the tibble #relevant to hw b part too
            murder_victims<-tibble('victim' = victim_names,
                                   'link'= links)
            
#putting third column calls function
            story_content<-murder_victims %>%
              rowwise() %>% do(linked_story = story_extractor(.$link)) %>%
              unnest()
           
            murder_victims<-murder_victims %>%
              mutate(linked_story = story_content$linked_story)
            
########################################################## 
#Assignment 4

            
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
            #testing only
            url3<-"https://www.universalhub.com/crime/south-end.html"
            #getting html from url
            response<-read_html(url2)
          
          
            #getting neighbourhood names
            neighbourhood <- rvest::html_nodes(x = response,
                                               xpath = '//option[contains(@class, "d-1")]')
            neighbourhood_names<-rvest::html_text(neighbourhood, trim =T)
            neighbourhood_names<-neighbourhood_names[1:20]
            
            #get all urls
            urls<- rvest::html_nodes(x = response,
                                     xpath = '//option[contains(@class, "d-1")]')
            urls<-html_attr(x= urls, 'value')
            urls<-urls[1:20]
            urls<-paste(url2_home,urls,sep = "")
            urls<-gsub(" ", "", urls, fixed = TRUE)           
            length(urls)
           
           
           
             #testing with one link
            url3<-"https://www.universalhub.com/crime/south-end.html"
            #getting html from url
            response3<-read_html(url3)
            
            
            #getting hour of crime
            date <- rvest::html_nodes(x = response3,
                                               xpath = '//td[contains(@class, "date")]')
            date<-rvest::html_text(date, trim =T)
            hours<-parse_date_time(date, "%m%d%y-%H:%M %p") %>% hour()
            
            #getting crime_type
            crime_type <- rvest::html_nodes(x = response3,
                                      xpath = '//td[contains(@class, "name")]')
            type<-rvest::html_text(crime_type, trim =T)
            trim.trailing <- function (x) sub("\\s+$", "", x)
            type<-trim.trailing(type)
           
            