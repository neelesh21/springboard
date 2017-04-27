
getwd() # where am I?


library(arules)
library(rpart)
install.packages("party")
library(ROCR)
library(party)
library(stringr)
library(dplyr)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)
library(e1071)
library(xgboost)
library(stringr)
library(lubridate)
library(tm)
library(rms)
library(glmnet)
library(pROC)
library(doMC)
library(kernlab)
library(plyr)




#reading dataset 
#dataset can be obtained from lending club. It is a publicly available dataset
import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE))
}

write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

my.data <- import.csv('lending_account.csv')

str(my.data)


mydf = my.data[1:(nrow(my.data)-2),]







#data preparation


#check the structure of the data
str(my.data)


#exploratory analysis


my.data %>% group_by(loan_status) %>% summarise(count = n())


#	loan_status 													count
# 	                                                       <fctr> <int>
#	1                                                                  1
#	2                                                  Charged Off  5200
#	3                                                      Current  5379
#	4                                                      Default     3
#	5         Does not meet the credit policy.  Status:Charged Off   752
#	6             Does not meet the credit policy.  Status:Current    85
#	7          Does not meet the credit policy.  Status:Fully Paid  1906
#	8  Does not meet the credit policy.  Status:Late (31-120 days)     6
#	9                                                   Fully Paid 28890
#	10                                             In Grace Period   118
#	11                                           Late (16-30 days)    28
#	12                                          Late (31-120 days)   168




#!!!!!!!!!!!!!!!!!! check plots here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#check home ownership split
table(my.data$home_ownership)







#find columns with missing 

tmp = sort(sapply(my.data, function(x) sum(length(which(is.na(x)))))/nrow(my.data),decreasing = TRUE)

tmp



# remove columns with NAs and 0s  


my.data$desc = NULL

my.data$url = NULL

my.data$emp_title = NULL

my.data$issue_d = NULL

my.data$zip_code = NULL

my.data$policy_code = NULL

my.data$mths_since_last_major_derog = NULL

my.data$last_credit_pull_d <- NULL

my.data$next_pymnt_d <- NULL

my.data$last_pymnt_d = NULL

my.data$earliest_cr_line = NULL

my.data$initial_list_status = NULL

my.data$collections_12_mths_ex_med = NULL

my.data$mths_since_last_major_derog = NULL


my.data$mths_since_last_delinq = NULL

my.data$id = NULL

#check data 
str(my.data)


#check head
head(my.data)




#convert into factors
my.data$grade <- as.factor(my.data$grade)

my.data$sub_grade <- as.factor(my.data$sub_grade)

my.data$is_rent <- my.data$home_ownership=="RENT"

my.data$home_ownership <- as.factor(my.data$home_ownership)

my.data$is_inc_v <- as.factor(my.data$is_inc_v)

my.data$pymnt_plan <- as.factor(my.data$pymnt_plan)



#Remove % and convert rates to numeric 
my.data$int_rate <- str_replace_all(my.data$int_rate, "[%]", "")

my.data$int_rate <- as.numeric(my.data$int_rate) 

bad_indicator = c("Late (16-30 days)", "Late (31-120 days)", "Default", "Charged Off")


#convert into factor for bad indicator

my.data$is_bad <- ifelse(my.data$loan_status %in% bad_indicator, 1, ifelse(my.data$loan_status=="", NA,0))

str(my.data)

#create new variable roi 
all_roi = sum(my.data$total_pymnt)/sum(my.data$funded_amnt) - 1                                      
all_roi


#check for corelation amongst all variables 
#check for numeric values first 
getNumericColumns<-function(t){
  tn = sapply(t,function(x){is.numeric(x)})
  return(names(tn)[which(tn)])
}

#plot corelation plot of all numeric variables
corrplot(cor(my.data[getNumericColumns(my.data)],use="na.or.complete"))



library(caret)
#remove corelated variables from the list 
high_corr <- findCorrelation(cor(my.data[getNumericColumns(my.data)]), cutoff = .75)
high_corr = getNumericColumns(my.data)[high_corr]
high_corr


library(rms)

#scale the data 
trans_model = preProcess(my.data,method=c("center","scale"))
my.data$collections_12_mths_ex_med = NULL


#checking the data and seeing months since last record is mostly NA so removing the column
my.data$mths_since_last_record = NULL



table(my.data$loan_status)
table(my.data$is_bad)
table(my.data$is_rent)
table(my.data$is_inc_v)



set.seed(1234)  #set seed to make the partition reproducible.
trng_ind = sample(seq_len(nrow(my.data)),size=30000,replace=F)
my.data.trng = my.data[trng_ind,]
my.data.test = my.data[-trng_ind,]

#check the proportion of bad loans in both training and test sets.
prop.table(table(my.data.trng$is_bad))
prop.table(table(my.data.test$is_bad))

my.data.test$prediction = ifelse(predictlog>=0.5,1,0)
my.data.test$prediction = factor(my.data.test$prediction, levels = c(0, 1), labels = c("Good", "Bad"))



logmodel1 <- glm(is_bad ~ loan_amnt + annual_inc + grade + int_rate + 
                   dti + addr_state, data = my.data.trng, family = "binomial")
summary(logmodel1)

logmodel <- glm(is_bad ~ loan_amnt + annual_inc + grade + 
                  int_rate , data = my.data.trng, family = "binomial")
summary(logmodel)

#Based on the above data, let us make our predictions on logmodel
predictlog <- predict(logmodel, newdata = my.data.test, type = "response")

#The confidence matrix
table(round(predictlog), my.data.test$is_bad)

library("gmodels")
CrossTable(my.data.test$is_bad,round(predictlog), prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual', 'predicted'))





CrossTable(my.data.test$is_bad, my.data.test$prediction, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual', 'predicted'))



#temporary dataset 
mydf <- my.data
mydf$loan_status = NULL

set.seed(1234)  #set seed to make the partition reproducible.
trng_ind = sample(seq_len(nrow(mydf)),size=30000,replace=F)
mydf.trng = mydf[trng_ind,]
mydf.test = mydf[-trng_ind,]

#check the proportion of bad loans in both training and test sets.
prop.table(table(mydf.trng$is_bad))
prop.table(table(mydf.test$is_bad))


my.data.logModel.3 = glm(is_bad~
                           
                           funded_amnt + 
                           funded_amnt_inv +
                           int_rate +
                           installment +
                           grade +
                           sub_grade +
                           emp_length +
                           pymnt_plan +
                           delinq_2yrs +
                           inq_last_6mths + 
                           revol_bal +
                               is_rent
                           
                         ,data=my.data.trng,family=binomial())

summary(my.data.logModel.3)

anova(my.data.logModel.3, test="Chisq")

library(effects)
#data.frame(Effect("is_bad", df2))



my.data.test$predictedProbabilities <- predict(my.data.logModel.3, newdata = my.data.test, type = "response")
my.data.test$predictedProbabilities <- predict(my.data.logModel.3, newdata = my.data.test[,-which(names(my.data.test) == "is_bad")] , type = "response")
#Warning message:



my.data.test$prediction = ifelse(my.data.test$predictedProbabilities>=0.5,1,0)
my.data.test$prediction = factor(my.data.test$prediction, levels = c(0, 1), labels = c("Good", "Bad"))

#Create a confusion matrix.


CrossTable(my.data.test$is_bad, my.data.test$prediction, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual', 'predicted'))





