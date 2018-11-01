
ind<-sample(2,nrow(training),replace=TRUE,prob=c(0.8,0.2))
traindata<- clean.data.naomit[ind==1,]
testdata<- training [ind==2,]


library(caret)
folds<-createFolds(y=clean.data.naomit$wt,k=5)
re<-{}
for(i in 1:10){
  traindata<-clean.data.naomit[-folds[[i]],]
  testdata<-clean.data.naomit[folds[[i]],]
  rf <- finalModel
  re=c(re,length(clean.data.naomit$wt[which(predict(rf)==clean.data.naomit$wt)])/length(clean.data.naomit))
}
mean(re)

