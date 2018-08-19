setwd("D:")
bank = read.csv("bank-default2.csv")
attach(bank)
summary(bank$age)
bank$age[is.na(bank$age)]<-39
summary(bank$campaign)
bank$campaign[is.na(bank$campaign)]<-1
summary(bank$y)

####분포 분석
barplot(table(bank$y), col = "blue", xlab = "y", ylab = "Frequency")
boxplot(bank$balance,col = "blue",ylim=c(-9000,100000), xlab = "balance", ylab = "Frequency")
hist(bank$campaign,xlab="campaign")

####로그 변환
bank$balance<-ifelse(bank$balance>=0,log(bank$balance+1),-log(-bank$balance+1))
bank$campaign<-log(bank$campaign)
bank$previous<-ifelse(bank$previous==0,"A","B")

####파티션
set.seed(123)
V = 2
n =  NROW(bank)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
bank.train = bank[ii,]
bank.test  = bank[-ii,]
attach(bank.train)
str(bank.train)
####회귀 적합
fit = glm(y ~., data = bank.train, family = binomial(link = "logit"))
summary(fit)

####모델 셀렉션
fit2 = step(fit, direction = "both")
fit2$anova
summary(fit2)
fit3 = step(fit, direction = "backward")
fit3$anova
summary(fit3)
fit4 = step(fit, direction = "forward")
fit4$anova
summary(fit4)

####예측
fit2.pred = predict(fit2, newdata = bank.test, type = "response") 

cutoff = 0.1
fit2.yhat = ifelse(fit2.pred <= cutoff, 0, 1)

ctable = table(bank.test$y, fit2.yhat,  dnn = c("Actual", "Predicted"))  
ctable

### Errors

miss.err = 1-sum(diag(ctable))/sum(ctable) # Misclassification Rate
miss.err

pred.acc = 1 - miss.err #Prediction Accuracy
pred.acc  

diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


### ROC and AUC

#install.packages("ROCR")
library(ROCR)

fit.pred = predict(fit2, newdata =  bank.test, type = "response") 
pred = prediction(fit.pred, bank.train$y)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.6, 0.3, legend = c("Regression","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


