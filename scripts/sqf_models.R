library(tidyverse)
library(ROCR)
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
     
      get_auc<-function(X1, X2)
      {
        model <- glm(formula = paste("found.weapon ~", X1," +",X2, "+ precinct"), family = binomial, data = train_sqf)
        pred_B2 <- predict(model, validation_sqf, type = "response")
        ROCR_B2 <- prediction(pred_B2, validation_sqf$found.weapon)
        auc_ROCR_B2 <- performance(ROCR_B2, measure = "auc")
        return (auc <- auc_ROCR_B2@y.values[[1]])
      }
  # t<- get_auc(model_performance[1,1],model_performance[1,2])
      for ( i in 1:nrow(model_performance))
      {
          model_performance[i,3]<-get_auc(model_performance[i,1], model_performance[i,2])
      }

      names(model_performance)<-c("feature_one", "feature_two","validation_auc")
      max_auc <-max(model_performance$validation_auc)
      maxindex <- which(model_performance$validation_auc== max_auc)
      
      #33, 75.48
      
#B2.3
      # Combine train_sqf andvalidation_sqf and fit a logistic regression model on this combined dataset using 
      # the same triplet of features that mazimized the AUC in the previous step. Report the AUCobtained when
      # making predictions with this new model ontest_sqf. Note that although this modeluses the same features 
      # as the model from the previous step, it will have different coefficients since it istrained on more data.
       
      B2.3_data<-rbind(train_sqf, validation_sqf)
      X1<-model_performance[maxindex,1]
        X2<-model_performance[maxindex,2]
      modelB2.3 <- glm(formula = paste("found.weapon ~", X1," +",X2, "+ precinct"), family = binomial, data = B2.3_data)
      pred_B2.3 <- predict(modelB2.3, test_sqf, type = "response")
      ROCR_B2.3 <- prediction(pred_B2.3, test_sqf$found.weapon)
      auc_ROCR_B2.3 <- performance(ROCR_B2.3, measure = "auc")
      this_auc<-auc_ROCR_B2.3@y.values[[1]]
      
      #75.22%
#B2.4 Plot a histogram of the 528 validation set AUC scores from Question B2.1, with a solid red
      #vertical line illustrating the AUC you obtained in question B2.3 (the "best" three-feature model
      #trained on 80%of the data), and a dotted red vertical line illustrating the AUC you obtained for the corresponding model 
      #trained just on train_sqf from Question B2.2. Save this figure tofigures/question_b2.png.
      myhistogram<-ggplot(data=model_performance, aes(model_performance$validation_auc)) +  geom_histogram(col = "blue",aes(fill=..count..))+
        geom_vline(xintercept=max_auc, linetype="solid", color = "red")+
      geom_vline(xintercept = this_auc, linetype = "dashed", color = "red")+
        labs(title="Histogram for AUC of different models", x="validation_AUC", y="Count")
      ggsave("hw4_Reza/figures/question_b2.png", myhistogram)
      