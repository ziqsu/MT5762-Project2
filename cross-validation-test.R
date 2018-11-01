
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
  y_pred <- predict (dataModel, newdata = test_fold[-3])
  cm <- table(test_fold[,3],y_pred)
  accuracy <- (cm[1,1] + cm [2,2]) / (cm[1,1] + cm [2,2] + cm[1,2] + cm [2,1])
  return(accuracy)
})
accuracy <- mean(as.numeric(cv))


########Easy one but works
library(DAAG)
# 5 fold cross-validation for finalModel
cv.lm(clean.data.naomit, form.lm = finalModel, plotit = "Observed", m=5)
cv.lm(clean.data.naomit, form.lm = finalModel, plotit = "Residual", m=5)

# 5 fold cross-validation for dataModel
cv.lm(clean.data.naomit, form.lm = dataModel, plotit = "Observed", m=5)
cv.lm(clean.data.naomit, form.lm = dataModel, plotit = "Residual", m=5)

# MSE for finalModel and dataModel
# model with smaller MSE is better

library(dvmisc)
get_mse(finalModel,var.estimate = FALSE)
get_mse(dataModel, var.estimate = FALSE)
