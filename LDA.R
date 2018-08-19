###############################################
# Lab: LDA/QDA with German Credit Data        #
###############################################
setwd("D:")
bank = read.csv("bank-default2(수정).csv")
attach(bank)
####age 결측치처리
summary(bank$age)
bank$age[is.na(bank$age)]<-39
summary(bank$campaign)
bank$campaign[is.na(bank$campaign)]<-1
summary(bank$y)

####로그 변환
bank$balance<-ifelse(bank$balance>=0,log(bank$balance+1),-log(-bank$balance+1))
bank$campaign<-log(bank$campaign)
### Install packages
install.packages("MASS")
library(MASS)
### LDA
attach(bank)
fit = lda(y ~job+marital+education+balance+housing+loan+contact+month+duration+campaign+poutcome, data=bank)
plot(fit)
#fit = qda(y ~., data=german)
### Prediction

cutoff = 0.5
pred = predict(fit, newdata=german)$posterior
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(german$y, yhat, dnn=c("Actual", "Predicted")); ctable #classification table


### Errors

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity
###########################################
# Computing the test error by paritioning


### Data partition

set.seed(123)
V = 2
n =  NROW(bank)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
bank.train = bank[ii,]
bank.test  = bank[-ii,]


### LDA/QDA

fit = lda(y ~., data=bank.train)
plot(fit)
#fit = qda(y ~., data=german.train)


### Prediction

cutoff = 0.11
pred = predict(fit, newdata=bank.test)$posterior
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(bank.test$y, yhat, dnn=c("Actual", "Predicted")); ctable #classification table


### Errors

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


### ROC and AUC

library(ROCR)
par(mfrow = c(2,2))

pred2 = predict(fit, newdata=bank.train)$posterior
pred = prediction(pred2[,2], bank.train$y)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("LDA/QDA","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


pred2 = predict(fit, newdata=bank.test)$posterior
pred = prediction(pred2[,2], bank.test$y)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("LDA/QDA","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC

