
rm(list=ls())
setwd("C:/Users/zouziyuan/Desktop/practicum/project/model")
data <- read.csv('line512.csv')



#hist(data$TOTAL_SCORE,labels=TRUE,main="Score Distribution",
#     col=c("pink"))

summary(data$TOTAL_SCORE)

nrow(data[which(data$TOTAL_SCORE==1),])




#install.packages("pROC")
library(pROC)


#Cutoff: >=2, 2.5, 3, 3.5, 4,1.5   (labels - high:1, low:0)
data$cutoff2<-0
data[which(data$TOTAL_SCORE>=2),'cutoff2']<-1

data$cutoff2.5<-0
data[which(data$TOTAL_SCORE>=2.5),'cutoff2.5']<-1

data$cutoff3<-0
data[which(data$TOTAL_SCORE>=3),'cutoff3']<-1

data$cutoff3.5<-0
data[which(data$TOTAL_SCORE>=3.5),'cutoff3.5']<-1

data$cutoff4<-0
data[which(data$TOTAL_SCORE>=4),'cutoff4']<-1

data$cutoff4<-0
data[which(data$TOTAL_SCORE>=4),'cutoff4']<-1


data$cutoff1.5<-0
data[which(data$TOTAL_SCORE>=1.5),'cutoff1.5']<-1




#split to train and val and test, decided later with tianying

#factorize
data$SOURCE <- as.factor(data$SOURCE)
data$RECEIPTS_STATUS <- as.factor(data$RECEIPTS_STATUS)
data$LOCATION <- as.factor(data$LOCATION)
data$PROMPT <- as.factor(data$PROMPT)
data$PERSON_TYPE_ID <- as.factor(data$PERSON_TYPE_ID)



#use XGBOOST to do the model training, need internet research
#Cutoff: >=2, 2.5, 3, 3.5, 4   (labels - high:1, low:0)

dataprep<-data[,-1]
dataprep<-dataprep[,-14:-15]

data2<-dataprep[,1:14]

data2.5<-dataprep[,1:15]
data2.5<-data2.5[,-14]

data3<-dataprep[,1:16]
data3<-data3[,-14:-15]

data3.5<-dataprep[,1:17]
data3.5<-data3.5[,-14:-16]

data4<-dataprep[,1:18]
data4<-data4[,-14:-17]


data1.5<-dataprep[,1:19]
data1.5<-data1.5[,-14:-18]




#install.packages("xgboost")
#install.packages("drat", repos="https://cran.rstudio.com")
#drat:::addRepo("dmlc")
#install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")

library(xgboost)

n=nrow(data)
n1=round(n*0.7)
n2=n=n1
trainrow=sample(1:n,n1)

#for data2
train<-data2[trainrow,]
test<-data2[-trainrow,]

X_train<-train[,-14]
Y_train<-train[,14]


xgb <- xgboost(data = data.matrix(X_train), 
               label = Y_train, 
               eta = 1,
               max_depth = 6, 
               nround=2, 
               objective = "binary:logistic",
               nthread = 3
)


X_test<-test[,-14]
Y_test<-test[,14]

y_pred <- predict(xgb, data.matrix(X_test))

Y_predict <- as.numeric(y_pred > 0.5)

#compare y_predict and actual y test
#use validation data or test data to estimate accuracy
#use xx score or AUC curve to show that we have better estimate

a<-data.frame(Y_test)
b<-data.frame(Y_predict)
df<-cbind(a,b)

tp<-nrow(df[which((df$Y_predict==1)&(df$Y_test==1)),])
fp<-nrow(df[which((df$Y_predict==1)&(df$Y_test==0)),])
tn<-nrow(df[which((df$Y_predict==0)&(df$Y_predict==0)),])
fn<-nrow(df[which((df$Y_predict==0)&(df$Y_test==1)),])

precision <- tp/(tp+fp)
recall    <- tp/(tp+fn)
2/(1/precision + 1/recall) # F1 score 0.7549671

#plot curve
nb.roc<-roc(Y_test,Y_predict)
plot.roc(nb.roc)
auc(Y_test,Y_predict) #0.8592


#for data2.5
train<-data2.5[trainrow,]
test<-data2.5[-trainrow,]
X_train<-train[,-14]
Y_train<-train[,14]


xgb <- xgboost(data = data.matrix(X_train), 
               label = Y_train, 
               eta = 1,
               max_depth = 6, 
               nround=5, 
               objective = "binary:logistic",
               nthread = 3
)   #train error 0.027


X_test<-test[,-14]
Y_test<-test[,14]

y_pred <- predict(xgb, data.matrix(X_test))
Y_predict <- as.numeric(y_pred > 0.5)


a<-data.frame(Y_test)
b<-data.frame(Y_predict)
df<-cbind(a,b)

tp<-nrow(df[which((df$Y_predict==1)&(df$Y_test==1)),])
fp<-nrow(df[which((df$Y_predict==1)&(df$Y_test==0)),])
tn<-nrow(df[which((df$Y_predict==0)&(df$Y_predict==0)),])
fn<-nrow(df[which((df$Y_predict==0)&(df$Y_test==1)),])

precision <- tp/(tp+fp)  #precision or true positive rate
recall    <- tp/(tp+fn)  #recall false positive rate
2/(1/precision + 1/recall) # F1 score 0.6533462


#plot curve
nb.roc<-roc(Y_test,Y_predict)
plot.roc(nb.roc)
auc(Y_test,Y_predict) #0.8377


#for data3
train<-data3[trainrow,]
test<-data3[-trainrow,]
X_train<-train[,-14]
Y_train<-train[,14]


xgb <- xgboost(data = data.matrix(X_train), 
               label = Y_train, 
               eta = 1,
               max_depth = 6, 
               nround=5, 
               objective = "binary:logistic",
               nthread = 3
)   #train error 0.01


X_test<-test[,-14]
Y_test<-test[,14]

y_pred <- predict(xgb, data.matrix(X_test))
Y_predict <- as.numeric(y_pred > 0.5)


a<-data.frame(Y_test)
b<-data.frame(Y_predict)
df<-cbind(a,b)

tp<-nrow(df[which((df$Y_predict==1)&(df$Y_test==1)),])
fp<-nrow(df[which((df$Y_predict==1)&(df$Y_test==0)),])
tn<-nrow(df[which((df$Y_predict==0)&(df$Y_predict==0)),])
fn<-nrow(df[which((df$Y_predict==0)&(df$Y_test==1)),])

precision <- tp/(tp+fp)
recall    <- tp/(tp+fn)
2/(1/precision + 1/recall) # F1 score 0.4716842


#plot curve
nb.roc<-roc(Y_test,Y_predict)
plot.roc(nb.roc)
auc(Y_test,Y_predict) #0.7005


#for data3.5
train<-data3.5[trainrow,]
test<-data3.5[-trainrow,]
X_train<-train[,-14]
Y_train<-train[,14]


xgb <- xgboost(data = data.matrix(X_train), 
               label = Y_train, 
               eta = 1,
               max_depth = 6, 
               nround=5, 
               objective = "binary:logistic",
               nthread = 3
)   #train error 0.003


X_test<-test[,-14]
Y_test<-test[,14]

y_pred <- predict(xgb, data.matrix(X_test))
Y_predict <- as.numeric(y_pred > 0.5)


a<-data.frame(Y_test)
b<-data.frame(Y_predict)
df<-cbind(a,b)

tp<-nrow(df[which((df$Y_predict==1)&(df$Y_test==1)),])
fp<-nrow(df[which((df$Y_predict==1)&(df$Y_test==0)),])
tn<-nrow(df[which((df$Y_predict==0)&(df$Y_predict==0)),])
fn<-nrow(df[which((df$Y_predict==0)&(df$Y_test==1)),])

precision <- tp/(tp+fp)
recall    <- tp/(tp+fn)
2/(1/precision + 1/recall) # F1 score 0.51

#plot curve
nb.roc<-roc(Y_test,Y_predict)
plot.roc(nb.roc)
auc(Y_test,Y_predict) #0.7198


#for data4
train<-data4[trainrow,]
test<-data4[-trainrow,]
X_train<-train[,-14]
Y_train<-train[,14]


xgb <- xgboost(data = data.matrix(X_train), 
               label = Y_train, 
               learning_rate =0.1,
               eta = 1,
               max_depth = 6, 
               nround=2, 
               objective = "binary:logistic",
               nthread = 3
)   #train error 0.001


X_test<-test[,-14]
Y_test<-test[,14]

y_pred <- predict(xgb, data.matrix(X_test))
Y_predict <- as.numeric(y_pred > 0.5)


a<-data.frame(Y_test)
b<-data.frame(Y_predict)
df<-cbind(a,b)

tp<-nrow(df[which((df$Y_predict==1)&(df$Y_test==1)),])
fp<-nrow(df[which((df$Y_predict==1)&(df$Y_test==0)),])
tn<-nrow(df[which((df$Y_predict==0)&(df$Y_predict==0)),])
fn<-nrow(df[which((df$Y_predict==0)&(df$Y_test==1)),])

precision <- tp/(tp+fp)
recall    <- tp/(tp+fn)
2/(1/precision + 1/recall) # F1 score 0.18


#plot curve
nb.roc<-roc(Y_test,Y_predict)
plot.roc(nb.roc)
auc(Y_test,Y_predict)  #0.5185


#for data1.5

train<-data1.5[trainrow,]
test<-data1.5[-trainrow,]
X_train<-train[,-14]
Y_train<-train[,14]


xgb <- xgboost(data = data.matrix(X_train), 
               label = Y_train, 
               eta = 1,
               max_depth = 6, 
               nround=2, 
               objective = "binary:logistic",
               nthread = 3
)   #train error 0.001


X_test<-test[,-14]
Y_test<-test[,14]

y_pred <- predict(xgb, data.matrix(X_test))
Y_predict <- as.numeric(y_pred > 0.5)


a<-data.frame(Y_test)
b<-data.frame(Y_predict)
df<-cbind(a,b)

tp<-nrow(df[which((df$Y_predict==1)&(df$Y_test==1)),])
fp<-nrow(df[which((df$Y_predict==1)&(df$Y_test==0)),])
tn<-nrow(df[which((df$Y_predict==0)&(df$Y_predict==0)),])
fn<-nrow(df[which((df$Y_predict==0)&(df$Y_test==1)),])

precision <- tp/(tp+fp)
recall    <- tp/(tp+fn)
2/(1/precision + 1/recall) # F1 score 0.18


#plot curve
nb.roc<-roc(Y_test,Y_predict)
plot.roc(nb.roc)
auc(Y_test,Y_predict)  #0.8459




#appendix



xgb <- xgboost(data = data.matrix(X_train), 
               label = Y_train, 
               eta = 0.1,
               max_depth = 15, 
               nround=5, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 12,
               nthread = 3
)


#https://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html

#https://www.analyticsvidhya.com/blog/2016/01/xgboost-algorithm-easy-steps/
  