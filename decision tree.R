setwd("D:")
bank = read.csv("bank-default2(수정).csv",header=TRUE)
install.packages("tree")
library(tree)
### Data handling
bank$y = ifelse(bank$y==" yes", 1, 0)
bank$y = factor(bank$y)


summary(bank)

### Grow a tree
fit = tree(y ~ job+marital+education+balance+housing+
             loan+contact+month+duration+campaign+poutcome, 
           data=bank, split="deviance")

fit
summary(fit)
plot(fit);  text(fit)


### Prune the tree

set.seed(3)
cv.fit = cv.tree(fit, FUN=prune.misclass)
names(cv.fit)
cv.fit

par(mfrow=c(2,2))
plot(cv.fit$size,cv.fit$dev, type="b")
plot(cv.fit$k,cv.fit$dev, type="b")

fit.pruned = prune.misclass(fit, best=cv.fit$size[which.min(cv.fit$dev)]) 
plot(fit.pruned);text(fit.pruned)



### Prediction

cutoff = 0.5
pred = predict(fit.pruned, newdata=bank, type="vector") #prediction
yhat = ifelse(pred[,2] > cutoff,1,0)
ctable = table(bank$y, yhat, dnn=c("Actual", "Predicted")); ctable #classification table


### Errors

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


#install.packages("ROCR")
library(ROCR)
pred2 = predict(fit.pruned, newdata=bank, type="vector") #prediction  # fit.pruned 로 수정
pred = prediction(pred2[,2], bank$y)
perf = performance(pred, "tpr","fpr")

par(mfrow=c(1,1))
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.6, 0.3, legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC





### Data partition

set.seed(12)
V = 2
n =  NROW(bank)
id = sample(1:V, n, prob = c(0.6,0.4), replace = T) # Partitioning 7:3
ii = which(id==1)
bank.train = bank[ii,]
bank.test  = bank[-ii,]

### Grow a tree

fit = tree(y ~ job+marital+education+balance+housing+
             loan+contact+month+duration+campaign+poutcome, 
           data=bank, bank.train)
fit
summary(fit)
plot(fit);  text(fit)


### Prune the tree

set.seed(3)
cv.fit = cv.tree(fit, FUN=prune.misclass)
names(cv.fit)
cv.fit

par(mfrow=c(2,2))
plot(cv.fit$size,cv.fit$dev, type="b")
plot(cv.fit$k,cv.fit$dev, type="b")

fit.pruned = prune.misclass(fit, best=cv.fit$size[which.min(cv.fit$dev)])
plot(fit.pruned);text(fit.pruned)



### Prediction

cutoff = 0.1
pred = predict(fit.pruned, newdata=bank.test, type="vector") #prediction
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

pred2 = predict(fit.pruned, newdata=bank.train, type="vector") #prediction
pred = prediction(pred2[,2], bank.train$y)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


pred2 = predict(fit.pruned, newdata=bank.test, type="vector") #prediction
pred = prediction(pred2[,2], bank.test$y)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(0.5, 0.3, legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


# Computing the CV error


V = 10 #V-fold CV
miss.err.train = 0
miss.err.test = 0
cutoff = 0.5

set.seed(12345)
id = sample(1:V, nrow(bank), replace = T)

for(i in 1:V) {
  
  print(i)
  
  bank.train = bank[id != i,] 
  bank.test = bank[id == i,] 
  
  fit = tree(y ~., bank.train) #Growing
  
  cv.fit = cv.tree(fit, FUN=prune.misclass)
  tree.size.min.dev = cv.fit$size[which.min(cv.fit$dev)]
  fit.pruned = prune.misclass(fit, best=tree.size.min.dev) # Pruning
  
  pred = predict(fit.pruned, newdata=bank.train, type="vector") #prediction
  yhat = ifelse(pred[,2] > cutoff, 1, 0)
  miss.err.train = miss.err.train + mean(bank.train$y != yhat) 
  
  pred = predict(fit.pruned, newdata=bank.test, type="vector") #prediction
  yhat = ifelse(pred[,2] > cutoff, 1, 0)
  miss.err.test = miss.err.test + mean(bank.test$y != yhat) 
  
}

cv.err.train = miss.err.train/ V ; cv.err.train # CV training error
cv.err.test = miss.err.test/ V ;cv.err.test # CV test error