mydata<-read.csv(file.choose())
attach(mydata)
str(mydata)
mydata$honors<-as.factor(mydata$honors)
mydata$honors<-relevel(mydata$honors, ref=1)
table1<-table(mydata$prog)
table1
table2<-table(mydata$ses)
table2
table(mydata$honors)
library(nnet)
library('mlogit')
model1<-multinom(honors~., data=mydata)
summary(model1)
z<-summary(model1)$coefficients/summary(model1)$standard.error*2
z

p<-(1-pnorm(abs(z),0,1))*2
p
pred1<-fitted(model1)
  pred1
  class(pred1)
  prob <- data.frame(pred1)
  View(pred1)
  pred1["pred"] <- NULL
  get_names <- function(i){
    return (names(which.max(i)))
  }
  
  pred1$pred <- pred_name
  View(pred1)
cm<-table(predict(model1),mydata$honors)
print(cm)

acc<-1-(sum(diag(cm))/sum(cm)) 
acc  #accuracy value  is 0

