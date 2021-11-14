#Downloading packages
#install.packages("tidyr")
#install.packages("readr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("class")
#install.packages("neuralnet")
#install.packages("caret")
#install.packages("lolR")
#Loading packages
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(class)
library(nnet)
library(caret)
library(lolR)
#import training dataset
setwd("D:/Σχολή/9ο εξάμηνο/Νευρωνικά δίκτυα - Βαθιά Μάθηση/Εργασίες/1η Εργασία/Data")
train<-read.csv("Train.csv")
str(train)
train$Profession[train$Profession==""]<-"Other"
train$Graduated[train$Graduated==""]<-"Other"
train$Ever_Married[train$Ever_Married==""]<-"Other"
train$Work_Experience[is.na(train$Work_Experience)]<-0
train$Gender<-as.factor(train$Gender)
train$Ever_Married<-as.factor(train$Ever_Married)
train$Graduated<-as.factor(train$Graduated)
train$Profession<-as.factor(train$Profession)
train$Spending_Score<-as.factor(train$Spending_Score)
train$Spending_Score<-factor(train$Spending_Score,levels = c("Low","Average","High"),
                             ordered = TRUE)
train$Var_1<-as.factor(train$Var_1)
train$Segmentation<-as.factor(train$Segmentation)
train<-na.omit(train)
train<-train[-1]
train<-train%>%mutate(Gender=ifelse(Gender=="Male",1,0))
train<-train%>%mutate(Ever_Married=ifelse(Ever_Married=="No",1,0))%>%
  mutate(Graduated=ifelse(Graduated=="No",0,1))
train<-train%>%mutate(Spending_Score=ifelse(Spending_Score=="High",4,ifelse(Spending_Score=="Low",0,2)))%>%
  mutate(Profession=ifelse(Profession=="Artist",9,ifelse(Profession=="Doctor",8,ifelse(Profession=="Engineer",7,
  ifelse(Profession=="Entertainment",6,ifelse(Profession=="Executive",5,ifelse(Profession=="Healthcare",4,
  ifelse(Profession=="Homemaker",3,ifelse(Profession=="Lawyer",2,ifelse(Profession=="Marketing",1,0))))))))))
train<-train%>%mutate(Var_1=ifelse(Var_1=="Cat_1",1,ifelse(Var_1=="Cat_2",2,ifelse(Var_1=="Cat_3",3,
ifelse(Var_1=="Cat_4",4,ifelse(Var_1=="Cat_5",5,ifelse(Var_1=="Cat_6",6,ifelse(Var_1=="Cat_7",7,0))))))))                                        

train_target_category<-train[,10]                                          
train_set<-train[-10]
#Loading test set
test<-read.csv("Test.csv")
test<-test[-1]

test<-test%>%mutate(Gender=ifelse(Gender=="Male",1,0))%>%mutate(Ever_Married=ifelse(Ever_Married=="No",1,0))%>%
  mutate(Graduated=ifelse(Graduated=="No",0,1))%>%mutate(Spending_Score=ifelse(Spending_Score=="High",4,
  ifelse(Spending_Score=="Low",0,2)))%>%mutate(Profession=ifelse(Profession=="Artist",9,ifelse(Profession=="Doctor",8,ifelse(Profession=="Engineer",7,
  ifelse(Profession=="Entertainment",6,ifelse(Profession=="Executive",5,ifelse(Profession=="Healthcare",4,ifelse(Profession=="Homemaker",3,
  ifelse(Profession=="Lawyer",2,ifelse(Profession=="Marketing",1,0))))))))))%>%mutate(Var_1=ifelse(Var_1=="Cat_1",1,ifelse(Var_1=="Cat_2",2,ifelse(Var_1=="Cat_3",3,
  ifelse(Var_1=="Cat_4",4,ifelse(Var_1=="Cat_5",5,ifelse(Var_1=="Cat_6",6,ifelse(Var_1=="Cat_7",7,0))))))))                                       

test$Work_Experience[is.na(test$Work_Experience)]<-0
test$Family_Size[is.na(test$Family_Size)]<-1

#Loanding sample submission
sample_submission<-read.csv("sample_submission.csv")
sample_submission<-sample_submission[,-1]

#Nearest Neighbour
pr_1<-knn(train_set,test,cl=train_target_category,k=1)

pr_3<-knn(train_set,test,cl=train_target_category,k=3)

tab_1<-table(pr_1,sample_submission)
tab_3<-table(pr_3,sample_submission)

accuracy<-function(x){sum(diag(x)/(sum(rowSums(x))))*100}
accuracy(tab_1)
accuracy(tab_3)

#Nearest Centroid 

model<-lol.classify.nearestCentroid(train_set,train_target_category)

vec_NC<-predict(model,test)
vec_NC<-as.factor(vec_NC)

tab_NC<-table(vec_NC,sample_submission)

accuracy(tab_NC)
