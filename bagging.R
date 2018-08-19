setwd("D:")
bank = read.csv("bank-default2(수정).csv",header=TRUE)
library(rpart)
par(mfrow=c(1,1))
bank$job = factor(bank$job)
bank$marital = factor(bank$marital)
bank$education = factor(bank$education)
bank$default = factor(bank$default)
bank$housing = factor(bank$housing)
bank$loan = factor(bank$loan)
bank$contact = factor(bank$contact)
bank$month = factor(bank$month)
bank$poutcome = factor(bank$poutcome)
bank$balance<-ifelse(bank$balance>=0,log(bank$balance+1),-log(-bank$balance+1))
bank$campaign<-log(bank$campaign)
bank$previous<-ifelse(bank$previous==0,"A","B")
bank$y = ifelse(bank$y==" yes", 1, 0)
bank$y = factor(bank$y)


### Install packages

install.packages("adabag")

library(rpart)
library(adabag)


### Grow a tree
set.seed(1234)
my.control = rpart.control(xval=0, cp=0, minsplit=5, maxdepth=10)
fit = bagging(y~job+marital+education+balance+housing+
                loan+contact+month+duration+campaign+poutcome, 
              data=bank, mfinal=100, control=my.control)

print(fit$importance)
importanceplot(fit)

plot(bank$y, bank$y.hat, xlab="Observed Values", ylab="Fitted Values")
abline(0,1)


### Prediction

pred = predict.bagging(fit, newdata=bank)
cutoff = 0.11
yhat = ifelse(pred$prob[,2] > cutoff, 1, 0)
ctable = table(bank$y, yhat, dnn=c("Actual", "Predicted"));
ctable #classification table

### Errors

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


### Install packages

#install.packages("rpart")
#install.packages("adabag")
library(rpart)
library(adabag)

### ROC and AUC

library(ROCR)

pred2 = predict.bagging(fit, newdata=bank)$prob
pred = prediction(pred2[,2], bank$y)
perf = performance(pred, "tpr","fpr")

par(mfrow=c(1,1))
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.6, 0.3, legend = c("Bagging","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC

#bagging방법의 분류기의 수와 오분류율#
evol.bank = errorevol(fit, newdata = bank)
plot.errorevol(evol.bank)


################################################################################
# Computing the test error by paritioning


### Data partition

set.seed(123)
V = 2
n =  NROW(bank)
id = sample(1:V, n, prob = c(0.6,0.4), replace = T) # Partitioning 6:4
ii = which(id==1)
bank.train = bank[ii,]
bank.test = bank[-ii,]


### Grow trees

set.seed(1234)
my.control = rpart.control(xval=0, cp=0, minsplit=5, maxdepth=10)
fit = bagging(y~job+marital+education+balance+housing+
                loan+contact+month+duration+campaign+poutcome, 
              data=bank.train, mfinal=100, control=my.control)

print(fit$importance)
importanceplot(fit)

### Prediction

pred = predict.bagging(fit, newdata=bank.test)
cutoff = 0.11
yhat = ifelse(pred$prob[,2] > cutoff, 1, 0)
ctable = table(bank.test$y, yhat, dnn=c("Actual", "Predicted"));
ctable #classification table

### Errors

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


### ROC and AUC

library(ROCR)
par(mfrow = c(2,2))

pred2 = predict.bagging(fit, newdata=bank.train)$prob
pred = prediction(pred2[,2], bank.train$y)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") 
#ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Bagging","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


pred2 = predict.bagging(fit, newdata=bank.test)$prob
pred = prediction(pred2[,2], bank.test$y)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Bagging","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC



###########################################################################
# Computing the CV error


V = 10 #V-fold CV
miss.err.train = 0
miss.err.test = 0
cutoff = 0.11

set.seed(12345)
id = sample(1:V, nrow(bank), replace = T)

for(i in 1:V) {
  
  print(i)
  
  bank.train = bank[id != i,] 
  bank.test = bank[id == i,] 
  
  my.control = rpart.control(xval=0, cp=0, minsplit=5, maxdepth=10)
  fit = bagging(y~job+marital+education+balance+housing+
                  loan+contact+month+duration+campaign+poutcome, 
                data=bank.train, mfinal=100, control=my.control)
  
  pred = predict.bagging(fit, newdata=bank.train)
  yhat = ifelse(pred$prob[,2] > cutoff, 1, 0)
  miss.err.train = miss.err.train + mean(bank.train$y != yhat) 
  
  pred = predict.bagging(fit, newdata=bank.test)
  yhat = ifelse(pred$prob[,2] > cutoff, 1, 0)
  miss.err.test = miss.err.test + mean(bank.test$y != yhat) 
  
}

cv.err.train = miss.err.train/ V; cv.err.train # CV training error
cv.err.test = miss.err.test/ V;cv.err.test # CV test error
