setwd('C:/Users/Anant Sharma/Desktop/DATA SCIENCE/Practice/Loan Prediction')

##load the data

train<-read.csv("train_ctrUa4K.csv")
test<-read.csv("test_lAUu6dG.csv")

## substituting the empty data with NA 

train$Gender[train$Gender==""]<-NA
train$Dependents[train$Dependents==""]<-NA
train$Married[train$Married==""]<-NA
train$Self_Employed[train$Self_Employed==""]<-NA

test$Gender[test$Gender==""]<-NA
test$Dependents[test$Dependents==""]<-NA
test$Married[test$Married==""]<-NA
test$Self_Employed[test$Self_Employed==""]<-NA


## imputing na values using mice package

library(mice)
imputedtrain<-mice(train,m=2,method="cart")
train<-complete(imputedtrain,2)

imputedtest<-mice(test,m=2,method="cart")
test<-complete(imputedtest,2)

## Removing outliers using log transformation and combining ApplicantIncome and coapplicantIncome

train$ApplicantIncome<-log(train$ApplicantIncome+train$CoapplicantIncome)
train$LoanAmount<-log(train$LoanAmount)
train<-train[,-8]

test$ApplicantIncome<-log(test$ApplicantIncome+test$CoapplicantIncome)
test$LoanAmount<-log(test$LoanAmount)
test<-test[,-8]

##Training model over training datset

library(caret)
library(randomForest)

fit<-randomForest(Loan_Status~Credit_History+LoanAmount+ApplicantIncome+Property_Area+Education+Loan_Amount_Term,data=train,importance=TRUE)

importance(fit)

## This provides that only three predictors are significant Credit_History,LoanAmount,ApplicantIncome.

fitnew<-randomForest(Loan_Status~Credit_History+LoanAmount+ApplicantIncome,data=train,importance=TRUE)

##Applying the model on test set.

predict<-predict(fitnew,test)

##making the data frame

final<-data.frame(Loan_ID=test$Loan_ID,Loan_Status=predict)

##Making the csv file

write.csv(final,file="submission_rf.csv",row.names=FALSE)

