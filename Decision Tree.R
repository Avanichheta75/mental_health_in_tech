#################################################
#Name:Avani Chheta
#CWID: 10471861
#Project: Mental health tech survey
#Model: Decision Tree
################################################

rm(list=ls())

##Load library
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library ("dplyr")

## Load data from csv file
mental_health<-read.csv("/Users/avanichheta/Study/SPRING_2022/KDD/Final Project/survey.csv",na.strings = '?')
View(mental_health)


##Summuray of mental data
summary(mental_health)

## Remove missing value
mental_health_notmissing <- na.omit(mental_health)

##mental_health_notmissing$treatment <- factor(mental_health_notmissing$treatment, labels =c('yes','no')) 

#removing not required rows
remove_list <- c('Timestamp', 'state','comments','work_interfere')                
new_df <- mental_health_notmissing[, !names(mental_health_notmissing) %in% remove_list]
mental_health_notmissing = new_df
cat('Missing Values After removing columns ->', sum(is.na(new_df)))
View(new_df)

##Converting NA string of self_employed to Na class
for(i in seq(from=1, to=nrow(mental_health_notmissing), by=1))
{
  if(mental_health_notmissing$self_employed[i] == 'NA') 
  {
    mental_health_notmissing$self_employed[i] <- NA;
  } 
}


#age
boxplot(mental_health$Age)
quantile(mental_health_notmissing$Age, probs = c(.25, .5, .75))
firstPecentile<-quantile(mental_health_notmissing$Age, probs = c(.25))
thirdPercentile <-quantile(mental_health_notmissing$Age, probs = c(.75))
iqr<-thirdPercentile - firstPecentile
iqr<-iqr*1.5
for(i in seq(from=1, to=nrow(mental_health_notmissing), by=1))
{
  if(mental_health_notmissing$Age[i]<firstPecentile- iqr|mental_health_notmissing$Age[i]>thirdPercentile+ iqr){
    mental_health_notmissing$Age[i] <- NA;
  } 
}
is.na(mental_health_notmissing$Age)
mental_health_notmissing <- na.omit(mental_health_notmissing)
boxplot(mental_health_notmissing$Age)

#gender
F_list = c('F', 'female', 'Woman', 'woman', 'Female', 'Femake',
             'femail', 'f', 'cis-female/femme', 'Cis Female')

M_list = c('M', 'm', 'male', 'Mail', 'maile', 'Mal', 'male',
              'Guy (-ish) ^_^', 'cis male', 'Cis Man', 'Cis Male',
              'Male ', 'Male (CIS)', 'male leaning androgynous',
              'Male-ish', 'Malr', 'Man', 'msle', '','Male')

getGender <- function(types){
  new_char_list = c(1, length(types))
  for (i in 1:length(types)) {
    type = types[i]
    new_char = 'unknown'   
    
    if(type %in% F_list){
      new_char = 'Female'
    }
    else if(type %in% M_list){
      new_char = 'Male'
    }
    
    else{
      new_char = 'unknown'
    }
    print(i,new_char)
    new_char_list[i] = new_char 
  }
  return(new_char_list) 
}

mental_health_notmissing = mental_health_notmissing %>% mutate(Gender = getGender(mental_health_notmissing$Gender))


mental_health_notmissing$Age <- as.factor(mental_health_notmissing$Age)      
mental_health_notmissing$Gender <- as.factor(mental_health_notmissing$Gender)      
mental_health_notmissing$Country <- as.factor(mental_health_notmissing$Country)      
mental_health_notmissing$self_employed <- as.factor(mental_health_notmissing$self_employed)      
mental_health_notmissing$family_history <- as.factor(mental_health_notmissing$family_history)      
mental_health_notmissing$treatment <- as.factor(mental_health_notmissing$treatment)      
mental_health_notmissing$no_employees <- as.factor(mental_health_notmissing$no_employees)      
mental_health_notmissing$remote_work <- as.factor(mental_health_notmissing$remote_work)      
mental_health_notmissing$tech_company <- as.factor(mental_health_notmissing$tech_company)      
mental_health_notmissing$benefits <- as.factor(mental_health_notmissing$benefits)      
mental_health_notmissing$care_options <- as.factor(mental_health_notmissing$care_options)      
mental_health_notmissing$wellness_program <- as.factor(mental_health_notmissing$wellness_program)      
mental_health_notmissing$seek_help <- as.factor(mental_health_notmissing$seek_help)      
mental_health_notmissing$anonymity <- as.factor(mental_health_notmissing$anonymity)      
mental_health_notmissingdata2$mental_health_consequence <- as.factor(mental_health_notmissing$mental_health_consequence)      
mental_health_notmissing$phys_health_consequence <- as.factor(mental_health_notmissing$phys_health_consequence)      
mental_health_notmissing$coworkers <- as.factor(mental_health_notmissing$coworkers)      
mental_health_notmissing$supervisor <- as.factor(mental_health_notmissing$supervisor)      
mental_health_notmissing$mental_health_interview <- as.factor(mental_health_notmissing$mental_health_interview)      
mental_health_notmissing$phys_health_interview <- as.factor(mental_health_notmissing$phys_health_interview)      
mental_health_notmissing$mental_vs_physical <- as.factor(mental_health_notmissing$mental_vs_physical)      
mental_health_notmissing$obs_consequence <- as.factor(mental_health_notmissing$obs_consequence)      

set.seed(111)

#Random_Index_Creation
idx<-sort(sample(nrow(mental_health_notmissing),as.integer((.30*nrow(mental_health_notmissing)))))
training_data <- mental_health_notmissing[-idx,]
nrow(training_data)
test_data =mental_health_notmissing[idx,]
nrow(test_data)

#Grow the tree
dev.off()
Cartclass<- rpart( treatment~., data =training_data )
summary(Cartclass)

#Plotting_Graph
rpart.plot(Cartclass)

predictcart<-predict( Cartclass ,test_data , type="class" )

#Creates_Frequency_Table
table(Actual=test_data[,23],CART=predictcart)
predictcart2<-predict(Cartclass,test_data)
str(predictcart2)
predictcart_cat<-ifelse(predictcart2[,1]<=.5,'yes','no')
table(Actual=test_data[,23],CART=predictcart_cat)

#Percentage Accuracy
match<- (test_data[,23]==predictcart)*100
acc<-sum(match)/length(match)
acc

#ErrorRate
error<- sum(test_data[,23]!=predictcart)
errorrate<-error/length(test_data[,11])
errorrate

library(rpart.plot)
prp(Cartclass)
install.packages("rpart.plot")

#graph
fancyRpartPlot(Cartclass) 


