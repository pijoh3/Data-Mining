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

####수치형변수 선택
bank2=bank[,c(1,6,11,12,13,15)]
head(bank2)
####표준화작업
bank3=data.frame(scale(bank2[,-6]),bank2[,6])
####KNN
fit=knn(train=bank3[,-6],test=bank3[,-6],cl=bank3[,6],k=10)
####prediction
yhat=fit
ctable = table(bank3[,6], yhat, dnn=c("Actual", "Predicted")); ctable
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
bank.train = bank3[ii,]
bank.test  = bank3[-ii,]


### KNN

fit = knn(train=bank.train[,-6], test=bank.test[,-6], cl=bank.train[,6], k=5) 


### Prediction

yhat=fit
ctable = table(bank.test[,6], yhat, dnn=c("Actual", "Predicted")); ctable #classification table


### Errors

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity

##########################
# Computing the CV error


V = 10 #V-fold CV
miss.err.train = 0
miss.err.test = 0
cutoff = 0.1

set.seed(12345)
id = sample(1:V, nrow(bank), replace = T)

for(i in 1:V) {
  
  print(i)
  
  bank.train = bank3[id != i,] 
  bank.test = bank3[id == i,] 
  
  fit = knn(train=bank.train[,-6],test=bank.train[,-6],cl=bank.train[,6], k=5) 
  yhat=fit
  miss.err.train = miss.err.train + mean(bank.train[,6] != yhat) 
  
  fit = knn(train=bank.test[,-6], test=bank.test[,-6], cl=bank.test[,6], k=5) 
  yhat=fit
  miss.err.test = miss.err.test + mean(bank.test[,6] != yhat) 
  
}

cv.err.train = miss.err.train/ V; cv.err.train # CV training error
cv.err.test = miss.err.test/ V;cv.err.test # CV test error



### END
