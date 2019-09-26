rm(list=ls(all=T))
#Set the working directory.
setwd("/Users/anchitchaturvedi/Desktop/Study/Edwisor/project2")
getwd()

df = read.csv('day.csv')
head(df)
#Removing the columns 'instant' and 'dteday' because they are not required for the analysis.
df = subset(df, select = -c(instant, dteday) )
#Converting the data types of the columns to categorical.
df[,1] = sapply(df[,1], as.factor)
df[,2] = sapply(df[,2], as.factor)
df[,3] = sapply(df[,3], as.factor)
df[,4] = sapply(df[,4], as.factor)
df[,5] = sapply(df[,5], as.factor)
df[,6] = sapply(df[,6], as.factor)
df[,7] = sapply(df[,7], as.factor)

class(df$weathersit)
sapply(df,class)

#Correlation analysis of the numeric columns.
library(corrgram)
corrgram(df[], order = F,
         upper.panel=panel.pie, text.panel=panel.txt)
summary(df$hum)

#Outlier removal from the 'hum' column. 
val = df[,'hum'][df[,'hum'] %in% boxplot.stats(df[,'hum'])$out]
df[,'hum'][df[,'hum'] %in% val] = NA
val

#Knn imputation.
library(DMwR)
df = knnImputation(data = df, k = 3)
#Selecting the appropriate columns to predict 'casual' bike rental values.
cas = subset(df, select = -c(registered, cnt) )
#Selecting the appropriate columns to predict 'registered' bike rental values.
reg = subset(df, select = -c(casual, cnt) )
head(cas)
head(reg)

#Train - test split.
set.seed(100)
train_index = sample(1:nrow(df), 0.75*nrow(df))
cas_train = cas[train_index,]
cas_test = cas[-train_index,]
reg_train = reg[train_index,]
reg_test = reg[-train_index,]
class(cas_train$yr)

cnt = cas_test$casual + reg_test$registered
library(MASS)
#Implementing the linear regresssion model.
lr = lm(casual ~., data = cas_train)
lr_cas = predict(lr, cas_test[,1:11])

lr_re = lm(registered ~., data = reg_train)
lr_reg = predict(lr_re, reg_test[,1:11])
cnt_lr = lr_reg + lr_cas 
cnt_lr = round(cnt_lr)

#Calculating MAPE for the model.
library(Metrics)
mape(cnt,cnt_lr)

#Implementation of the decision tree model.
library(rpart)
dt_cas = rpart(casual ~., data = cas_train)
cas_dt = predict(dt_cas, cas_test[,1:11])

dt_reg = rpart(registered ~., data = reg_train)
reg_dt = predict(dt_reg, reg_test[,1:11])

cnt_dt = cas_dt + reg_dt 
cnt_dt = round(cnt_dt)

#Calculating MAPE for decision tree model.
mape(cnt, cnt_dt)

#Implementing random forest regression model.
library(randomForest)
cas_rf = randomForest(casual ~., data = cas_train)
rf_cas = predict(cas_rf, cas_test[,1:11])

reg_rf = randomForest(registered ~., data = reg_train)
rf_reg = predict(reg_rf, reg_test[,1:11])

cnt_rf = rf_reg + rf_cas
cnt_rf = round(cnt_rf)

#Calculating the MAPE for Random Forest model.
mape(cnt, cnt_rf)