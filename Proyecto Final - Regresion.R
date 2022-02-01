library(tidyr) 
library(skimr)
library(dplyr) 
library(ggplot2)
library(corrplot)
library(weights)
library(tidyverse)
library(e1071)
library(Metrics)

#-------------------------
#PREPARANDO EL DATASET
#-------------------------
set.seed(100)

test = read.csv("test.csv", stringsAsFactors = T)
samp = read.csv("sample_submission.csv", stringsAsFactors = T)

train = read.csv("train.csv", stringsAsFactors = T)

str(test)

train = train %>% 
  mutate(
    Driving_License = as_factor(Driving_License),
    Region_Code = as.factor(Region_Code),
    Previously_Insured = as.factor(Previously_Insured),
    Policy_Sales_Channel = as.factor(Policy_Sales_Channel),
    Response = as.factor(Response)
    )

test = test %>% 
  mutate(
    Driving_License = as.factor(Driving_License),
    Region_Code = as.factor(Region_Code),
    Previously_Insured = as.factor(Previously_Insured),
    Policy_Sales_Channel = as.factor(Policy_Sales_Channel)
    )

train$respondio <- ifelse(train$Response == 1, "SI", "NO")

#-------------------------
#DIVIDIENDO LA DATA
#-------------------------
Union1 = left_join(test,train,by="id")
Datos = left_join(Union1,Internet_Serv,by="id")


#-------------------------
#REGRESION LOGISTICA 1
#-------------------------
m1_log = glm(Response ~ Gender + Age + Vehicle_Damage, data = train, family = "binomial")
summary(m1_log)

prediccion_log = predict(m1_log, test_total, type = "response")
prediccion_log2 = ifelse(prediccion_log >= .2,"SI","NO")
resultsLOG <- table(data$Response, prediccion_log2)
resultsLOG
prop.table(resultsLOG)

#Calculo el acurracy
accuracyLOG1<-sum(diag(resultsLOG))/sum(resultsLOG)
accuracyLOG1

#TP/(TP+FN)
recall1<-(resultsLOG[2,2]/(resultsLOG[2,1]+resultsLOG[2,2]))
recall1

#TP/(TP+FP)
precision1<-(resultsLOG[2,2]/(resultsLOG[1,2]+resultsLOG[2,2]))
precision1

#-------------------------
#REGRESION LOGISTICA 2
#-------------------------
m2_log = glm(Response ~ Gender + Age + Vehicle_Damage + Previously_Insured, data = train, family = "binomial")
summary(m2_log)

prediccion_log = predict(m2_log, test_total, type = "response")
prediccion_log2 = ifelse(prediccion_log >= .2,"SI","NO")
resultsLOG <- table(data$Response, prediccion_log2)
resultsLOG
prop.table(resultsLOG)

#Calculo el acurracy
accuracyLOG2<-sum(diag(resultsLOG))/sum(resultsLOG)
accuracyLOG2

#TP/(TP+FN)
recall2<-(resultsLOG[2,2]/(resultsLOG[2,1]+resultsLOG[2,2]))
recall2

#TP/(TP+FP)
precision2<-(resultsLOG[2,2]/(resultsLOG[1,2]+resultsLOG[2,2]))
precision2

