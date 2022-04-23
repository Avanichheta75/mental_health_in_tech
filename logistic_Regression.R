rm(list=ls()) # clearing object environment


library(ggplot2)
library ("dplyr")

dev.off()

#Load data file CSV
dataFile = file.choose()
data <- read.csv(file = dataFile, header = TRUE, na.strings = "?")
head(data)
print(data)
View(data)
summary(data)

dataWithoutNA <- na.omit(data)

data %>% glimpse()
View(dataWithoutNA)

remove_list <- c('Timestamp', 'state', 'work_interfere', 'comments')                
new_data <- dataWithoutNA[, !names(dataWithoutNA) %in% remove_list]
View(new_data)
dataWithoutNA = new_data
cat('Missing Valus After removing columns ->', sum(is.na(dataWithoutNA)))

View(dataWithoutNA)
summary(dataWithoutNA)


for(i in seq(from=1, to=nrow(dataWithoutNA), by=1))
{
  if(dataWithoutNA$self_employed[i] == "NA") 
  {
    dataWithoutNA$self_employed[i] <- NA;
  } 
}

dataWithoutNA<-na.omit(dataWithoutNA)
View(dataWithoutNA)

#age
boxplot(data$Age)
quantile(dataWithoutNA$Age, probs = c(.25, .5, .75))
firstPecentile<-quantile(dataWithoutNA$Age, probs = c(.25))
thirdPercentile <-quantile(dataWithoutNA$Age, probs = c(.75))
iqr<-thirdPercentile - firstPecentile
iqr<-iqr*1.5
for(i in seq(from=1, to=nrow(dataWithoutNA), by=1))
{
  if(dataWithoutNA$Age[i]<firstPecentile- iqr|dataWithoutNA$Age[i]>thirdPercentile+ iqr){
    dataWithoutNA$Age[i] <- NA;
  } 
}
is.na(dataWithoutNA$Age)
dataWithoutNA <- na.omit(dataWithoutNA)
View(dataWithoutNA)
boxplot(dataWithoutNA$Age)



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

dataWithoutNA = dataWithoutNA %>% 
  mutate(Gender = getGender(dataWithoutNA$Gender))
View(dataWithoutNA)



data2 <- dataWithoutNA
data2$Age <- as.factor(data2$Age)      
class(data2$Age)
data2$Gender <- as.factor(data2$Gender)      
class(data2$Gender)
data2$Country <- as.factor(data2$Country)      
class(data2$Country)
data2$self_employed <- as.factor(data2$self_employed)      
class(data2$self_employed)
data2$family_history <- as.factor(data2$family_history)      
class(data2$family_history)
data2$treatment <- as.factor(data2$treatment)      
class(data2$treatment)
data2$no_employees <- as.factor(data2$no_employees)      
class(data2$no_employees)
data2$remote_work <- as.factor(data2$remote_work)      
class(data2$remote_work)
data2$tech_company <- as.factor(data2$tech_company)      
class(data2$tech_company)
data2$benefits <- as.factor(data2$benefits)      
class(data2$benefits)
data2$care_options <- as.factor(data2$care_options)      
class(data2$care_options)
data2$wellness_program <- as.factor(data2$wellness_program)      
class(data2$wellness_program)
data2$seek_help <- as.factor(data2$seek_help)      
class(data2$seek_help)
data2$anonymity <- as.factor(data2$anonymity)      
class(data2$anonymity)
data2$leave <- as.factor(data2$leave)      
class(data2$leave)
data2$mental_health_consequence <- as.factor(data2$mental_health_consequence)      
class(data2$mental_health_consequence)
data2$phys_health_consequence <- as.factor(data2$phys_health_consequence)      
class(data2$phys_health_consequence)
data2$coworkers <- as.factor(data2$coworkers)      
class(data2$coworkers)
data2$supervisor <- as.factor(data2$supervisor)      
class(data2$supervisor)
data2$mental_health_interview <- as.factor(data2$mental_health_interview)      
class(data2$mental_health_interview)
data2$phys_health_interview <- as.factor(data2$phys_health_interview)      
class(data2$phys_health_interview)
data2$mental_vs_physical <- as.factor(data2$mental_vs_physical)      
class(data2$mental_vs_physical)
data2$obs_consequence <- as.factor(data2$obs_consequence)      
class(data2$obs_consequence)

dataWithoutNA <- data2


mental_health_matrix <- data.matrix(dataWithoutNA)
heatmap(mental_health_matrix)

#data sampling and dividing data into 70% and 30% training and test data
idx<-sort(sample(nrow(dataWithoutNA),as.integer(0.7*nrow(dataWithoutNA))))
idx

# Build train,test
training<-dataWithoutNA[idx,]
training
nrow(training)
test<-dataWithoutNA[-idx,]
test
nrow(test)



######################   LOGISTIC REGRESSION   #######################


final.glm <- glm(formula= treatment ~., data=training[,!colnames(training) %in% c("Country")], family=binomial)
summary(final.glm)
fitted.results<-final.glm %>% predict(test, type = "response")
fitted.results <- ifelse(fitted.results > 0.5,1,0)
demo<-table(Actual=test$treatment, fitted.results)
demo

#Accuracy of Logistic Regression
Accuracy <-(sum(diag(demo))/(sum(rowSums(demo)))*100) 
Accuracy

#Error Rate of Logistic Regression
error_rate<- 1 - as.double(Accuracy/100)
error_rate




