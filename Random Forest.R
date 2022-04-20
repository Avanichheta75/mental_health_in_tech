# Course      : CS 513B
# Name        : Savleen Kaur
# CWID        : 10476867
# Project     : RF


rm(list=ls())

install.packages('randomForest')
library(magrittr)
library(dplyr)

#Importing csv to r
P_RF <- read.csv("survey.csv", header = TRUE, sep = ",", na.strings = c("?"))
View(P_RF)

#Remove Missing Values
P_RF_notmissing<-na.omit(P_RF)
View(P_RF_notmissing)


#Converting NA string of self_employed to Na class
for(i in seq(from=1, to=nrow(P_RF), by=1))
{
  if(P_RF$self_employed[i] == 'NA') 
  {
    P_RF$self_employed[i] <- NA;
  } 
}


#removing not required rows
remove_list <- c('Timestamp', 'state','comments','work_interfere')                
new_df <- P_RF_notmissing[, !names(P_RF_notmissing) %in% remove_list]
P_RF_notmissing = new_df
View(P_RF_notmissing)


#Age
quantile(P_RF_notmissing$Age, probs = c(.25, .5, .75))
firstPecentile<-quantile(P_RF_notmissing$Age, probs = c(.25))
thirdPercentile <-quantile(P_RF_notmissing$Age, probs = c(.75))
iqr<-thirdPercentile - firstPecentile
iqr<-iqr*1.5
for(i in seq(from=1, to=nrow(P_RF_notmissing), by=1))
{
  if(P_RF_notmissing$Age[i]<firstPecentile- iqr|P_RF_notmissing$Age[i]>thirdPercentile+ iqr){
    P_RF_notmissing$Age[i] <- NA;
  } 
}
is.na(P_RF_notmissing$Age)
P_RF_notmissing <- na.omit(P_RF_notmissing)
boxplot(P_RF_notmissing$Age)


#gender
fem_list = c('F', 'female', 'Woman', 'woman', 'Female', 'Femake',
             'femail', 'f', 'cis-female/femme', 'Cis Female')

male_list = c('M', 'm', 'male', 'Mail', 'maile', 'Mal', 'male',
              'Guy (-ish) ^_^', 'cis male', 'Cis Man', 'Cis Male',
              'Male ', 'Male (CIS)', 'male leaning androgynous',
              'Male-ish', 'Malr', 'Man', 'msle', '', 'Male')

getGender <- function(types){
  new_char_list = c(1, length(types))
  for (i in 1:length(types)) {
    type = types[i]
    new_char = 'unknown'   
    
    if(type %in% fem_list){
      new_char = 'Female'
    }
    else if(type %in% male_list){
      new_char = 'Male'
    }
    
    else{
      new_char = 'unknown'
    }
    new_char_list[i] = new_char 
  }
  return(new_char_list) 
}

P_RF_notmissing = P_RF_notmissing %>% 
  mutate(Gender = getGender(P_RF_notmissing$Gender))


View(P_RF_notmissing)


P_RF_notmissing$Age <- as.factor(P_RF_notmissing$Age)      
class(P_RF_notmissing$Age)
is.factor(P_RF_notmissing$Age)
P_RF_notmissing$Gender <- as.factor(P_RF_notmissing$Gender)      
class(P_RF_notmissing$Gender)
is.factor(P_RF_notmissing$Gender)
P_RF_notmissing$Country <- as.factor(P_RF_notmissing$Country)      
class(P_RF_notmissing$Country)
is.factor(P_RF_notmissing$Country)
P_RF_notmissing$self_employed <- as.factor(P_RF_notmissing$self_employed)      
class(P_RF_notmissing$self_employed)
is.factor(P_RF_notmissing$self_employed)
P_RF_notmissing$family_history <- as.factor(P_RF_notmissing$family_history)      
class(P_RF_notmissing$family_history)
is.factor(P_RF_notmissing$family_history)
P_RF_notmissing$treatment <- as.factor(P_RF_notmissing$treatment)      
class(P_RF_notmissing$treatment)
is.factor(P_RF_notmissing$treatment)
P_RF_notmissing$no_employees <- as.factor(P_RF_notmissing$no_employees)      
class(P_RF_notmissing$no_employees)
is.factor(P_RF_notmissing$no_employees)
P_RF_notmissing$remote_work <- as.factor(P_RF_notmissing$remote_work)      
class(P_RF_notmissing$remote_work)
is.factor(P_RF_notmissing$remote_work)
P_RF_notmissing$tech_company <- as.factor(P_RF_notmissing$tech_company)      
class(P_RF_notmissing$tech_company)
is.factor(P_RF_notmissing$tech_company)
P_RF_notmissing$benefits <- as.factor(P_RF_notmissing$benefits)      
class(P_RF_notmissing$benefits)
is.factor(P_RF_notmissing$benefits)
P_RF_notmissing$care_options <- as.factor(P_RF_notmissing$care_options)      
class(P_RF_notmissing$care_options)
is.factor(P_RF_notmissing$care_options)
P_RF_notmissing$wellness_program <- as.factor(P_RF_notmissing$wellness_program)      
class(P_RF_notmissing$wellness_program)
is.factor(P_RF_notmissing$wellness_program)
P_RF_notmissing$seek_help <- as.factor(P_RF_notmissing$seek_help)      
class(P_RF_notmissing$seek_help)
is.factor(P_RF_notmissing$seek_help)
P_RF_notmissing$anonymity <- as.factor(P_RF_notmissing$anonymity)      
class(P_RF_notmissing$anonymity)
is.factor(P_RF_notmissing$anonymity)
P_RF_notmissing$leave <- as.factor(P_RF_notmissing$leave)      
class(P_RF_notmissing$leave)
is.factor(P_RF_notmissing$leave)
P_RF_notmissing$mental_health_consequence <- as.factor(P_RF_notmissing$mental_health_consequence)      
class(P_RF_notmissing$mental_health_consequence)
is.factor(P_RF_notmissing$mental_health_consequence)
P_RF_notmissing$phys_health_consequence <- as.factor(P_RF_notmissing$phys_health_consequence)      
class(P_RF_notmissing$phys_health_consequence)
is.factor(P_RF_notmissing$phys_health_consequence)
P_RF_notmissing$coworkers <- as.factor(P_RF_notmissing$coworkers)      
class(P_RF_notmissing$coworkers)
is.factor(P_RF_notmissing$coworkers)
P_RF_notmissing$supervisor <- as.factor(P_RF_notmissing$supervisor)      
class(P_RF_notmissing$supervisor)
is.factor(P_RF_notmissing$supervisor)
P_RF_notmissing$mental_health_interview <- as.factor(P_RF_notmissing$mental_health_interview)      
class(P_RF_notmissing$mental_health_interview)
is.factor(P_RF_notmissing$mental_health_interview)
P_RF_notmissing$phys_health_interview <- as.factor(P_RF_notmissing$phys_health_interview)      
class(P_RF_notmissing$phys_health_interview)
is.factor(P_RF_notmissing$phys_health_interview)
P_RF_notmissing$mental_vs_physical <- as.factor(P_RF_notmissing$mental_vs_physical)      
class(P_RF_notmissing$mental_vs_physical)
is.factor(P_RF_notmissing$mental_vs_physical)
P_RF_notmissing$obs_consequence <- as.factor(P_RF_notmissing$obs_consequence)      
class(P_RF_notmissing$obs_consequence)
is.factor(P_RF_notmissing$obs_consequence)


#get same data
set.seed(111)

#30% test data & 70% training data
idx<-sort(sample(nrow(P_RF_notmissing),as.integer(.70*nrow(P_RF_notmissing))))
idx
training<-P_RF_notmissing[idx,]
nrow(training)
test<-P_RF_notmissing[-idx,]
nrow(test)


#Random Forest
library(randomForest)

fit <- randomForest( treatment~., data=training, importance=TRUE, ntree=1000)

importance(fit)
varImpPlot(fit)
dev.off()
RPrediction <- predict(fit, test)
a<-table(actual=test$treatment,RPrediction)
a

#Error rate
wrong<- (test$treatment!=RPrediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 

#Accuracy
Accuracy <-(sum(diag(a))/(sum(rowSums(a)))*100) 
Accuracy
