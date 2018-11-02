
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





###########
#Another way
library(caTools)
set.seed(110)
split <- sample.split(clean.data.naomit$wt, SplitRatio = 0.8)
training_set <- subset(clean.data.naomit, split = TRUE)
test_set <- subset(clean.data.naomit, split = FALSE)

#Applying 5-fold cross validation
#install.packages("caret")
library(caret)
folds <-createFolds(training_set$wt, k = 5)
cv <- lapply(folds, function(x){
  training_fold <- training_set[-x,]
  test_fold <- training_set[x,]
  dataModel <- lm(wt ~., data = training_fold)
  dataModel <- step(dataModel)
  y_pred <- predict (dataModel, newdata = test_fold[-3])
  cm <- table(test_fold[,3],y_pred)
  accuracy <- (cm[1,1] + cm [2,2]) / (cm[1,1] + cm [2,2] + cm[1,2] + cm [2,1])
  return(accuracy)
})


library(datasets)

?traindata
library(caret)

# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=10)
# Fit Naive Bayes Model
model <- train(Species~., data=iris, trControl=train_control, method="nb")
# Summarise Results
print(model)