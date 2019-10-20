library(tidyverse)
#QB1_2
      #set the seed to2048.
      set.seed(2048)
      
      #Read in the stop and frisk data fromdata_hw4, subset itto observations between 2013-2015
      sqf<-read.csv("data_hw4/sqf_08_16.csv")
      sqf_data<-sqf %>% filter(year==2013 | year==2014|year==2015) %>% 
                                  filter(suspected.crime=="cpw") %>%
                                  select(id, year, found.weapon, precinct, location.housing,  
                                         stopped.bc.bulge, stopped.bc.object,
                                         stopped.bc.desc,   stopped.bc.casing, 
                                         stopped.bc.lookout, stopped.bc.clothing,
                                         stopped.bc.drugs,stopped.bc.furtive,
                                         stopped.bc.violent, stopped.bc.other,
                                         additional.report,  additional.investigation, 
                                         additional.associating,  additional.proximity, 
                                         additional.evasive, additional.direction,
                                         additional.highcrime, additional.time, 
                                         additional.sights,additional.other,
                                         suspect.age,suspect.sex,suspect.build,suspect.height, suspect.weight,
                                         inside,radio.run, officer.uniform, observation.period, day, month, time.period
                                          ) %>%
                                          mutate( precinct = factor(precinct),
                                                  time.period= factor(time.period)
                                          )%>% 
                                          filter(complete.cases(.))
#QB1_3  
      #3.Randomly shuffle   the rows of sqf_dataand split it into training, validation, and test sets,
      #using a60%-20%-20% split. Call the resulting tibbles train_sqf,validation_sqf, and test_sqf.                                          
      sqf_data<-sample_n(sqf_data, n())

      temp<-sample(1:nrow(sqf_data),.6*nrow(sqf_data))
      train_sqf <- sqf_data[temp,]
      ## This is used for further bifurcation
      remain40 <- sqf_data[-temp,]  
      samp2 <- sample(1:nrow(remain40),.5*nrow(remain40))
      validation_sqf <- remain40[samp2,] 
      test_sqf <- remain40[-samp2,]


#QB2
      #we will use a validation set and "brute force" to find the best three-feature logistic regression model 
      #for predictingfound.weapon. However, motivated by domain knowledge and computational 
      #feasibility,we will only consider models that include precinct as one of the three features.
#QB2_1  
      #model_performance, # feature_one,feature_two, and validation_auc, defined as follows: 
      names<-colnames(sqf_data)
      #dropped id year precinct and found.weapon
      names<-names[-(1:4)]
      #combination of column names
      result<-combn(names,2, simplify  =FALSE)
      model_performance<-data.frame(matrix(unlist(result), nrow=length(result), byrow=T)) %>% 
        rename( "feature_one" = X1,  "feature_two"= X2) 
      
     #You may find it helpful to write a function,get_auc()that takes two feature names as input,
      #and outputs the corresponding AUC on the validationset (use theROCRpackage).
      #Also note thatmutate()might not behave like you expect when fitting adifferent model for each row; 
      #one solution is to use the rowwise()and do()commands.
      
      
       
      
      
  