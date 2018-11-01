# Consider 5 fold cross validation:
# We will divide this data set of 1236 observations into 5 partitions
# Generate the training and test samples
set.seed(5762)

##
library(boot)
library(DAAG)

MSE_5_folder_cv <- cv.lm(babies.data,finalModel, m = 5)$delta[1]
MSE_5_folder_cv



# Import the library MASS for dataset and caret for crossvalidation
library(MASS, quietly = TRUE)
library(caret)

#Storing the "babies.data" into Dataframe named "DataFrame"
DataFrame<- babies.data

#Check the structure of the data
str(DataFrame)

#Check the dimension of this data frame
dim(DataFrame)

#Check the first 3 rows
head(DataFrame, 3)

#Check the summary of dataset
summary(DataFrame)

###Create the train and test data set.Target variable is wt
library(caTools)
ind = createDataPartition(DataFrame$wt, p= 4/5, list = FALSE)
trainDF<-DataFrame[ind,]
testDF<-DataFrame[-ind,]

#Choose the parameters for the train function in caret
ControlParameters <- trainControl(method = "cv", number = 5, savePredictions = TRUE, classProbs = TRUE)

###################
fit=lm(babies.data$wt~babies.data$gestation)

cv.lm(fit, m=5, seed=5762)

parameterGrid <- expand.grid(mtry=c(1,2,3,4,5))

modelRandom <- train (wt~gestation, data = trainDF, method = "rf", trControl = ControlParameters, tuneGrid = parameterGrid)








###########################
require(caret)
data(babies.data)
set.seed(62)

model <- train (wt~dwt, babies.data, method = "lm", trControl=trainControl(method="cv",number = 5, verboseIter = TRUE))








#########################
require(caret)
classes <- babies.data [,"wt"]
predictors <- babies.data [, -match(c("gestation","ht","ded","dwt","inc","time","number","inc:parity","wt.1:ed","ed:drace","time:ed","ded:smoke"),colnames(babies.data))]

train_set <- createDataPartition(classes, p= 0.8, list = FALSE)
str(train_set)

train_predictors <- predictors[train_set,]
train_classes <- classes [train_set]
test_predictors <- predictors[-train_set,]
test_classes <- classes [-train_set]

set.seed(6262)
cv_splits <- createFolds(classes, k= 5, returnTrain = TRUE)
str(cv_splits)


######################
require(glmnet)

set.seed(5757)

cs_data_train <- cs_data[train_set, ]
cs_data_test <- cs_data[-train_set, ]

glmnet_grid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),
                           lambda = seq(.01, .2, length = 20))
glmnet_ctrl <- trainControl(method = "cv", number = 5)
glmnet_fit <- train(wt ~ ., data = cs_data_train,
                    method = "glmnet",
                    preProcess = c("gestation","ht","ded","dwt","inc","time","number","inc:parity","wt.1:ed","ed:drace","time:ed","ded:smoke"),
                    tuneGrid = glmnet_grid,
                    trControl = glmnet_ctrl)

trellis.par.set(caretTheme())
plot(glmnet_fit, scales = list(x=list(log =2)))














ind<-sample(2,nrow(babies.data),replace=TRUE,prob=c(0.8,0.2)) 
traindata<- babies.data [ind==1,]
testdata<- babies.data [ind==2,]

library(caret)
folds<-createFolds(y=babies.data$wt,k=5)
re<-{}
for(i in 1:10){
  traindata<-babies.data[-folds[[i]],]
  testdata<-babies.data[folds[[i]],]
  re=c(re,length(babies.data$wt[which(predict(dataModel)==babies.data$wt)])/length(babies.data$wt))
}
mean(re)

#cross validation for final data
ind<-sample(2,nrow(babies.data),replace=TRUE,prob=c(0.8,0.2)) 
traindata<- babies.data [ind==1,]
testdata<- babies.data [ind==2,]

library(caret)
folds<-createFolds(y=babies.data$wt,k=5)
re<-{}
for(i in 1:10){
  traindata<-babies.data[-folds[[i]],]
  testdata<-babies.data[folds[[i]],]
  re=c(re,length(babies.data$wt[which(predict(finalModel)==babies.data$wt)])/length(babies.data$wt))
}
mean(re)

############### 5 fold cross validation for 
library(caret)

train_control <- trainControl (method = "repeatedcv", number = 5, repeats=3)
#grid <- expand.grid(.fL=c(0), .usekernel = c(FALSE))
model<- train (wt~., data = babies.data, trControl = train_control, method = "nb") #, tuneGrid=Grid)
print(model)

############### MSE
library(dvmisc)
get_mse(finalModel,var.estimate = FALSE)
get_mse(dataModel, var.estimate = FALSE)
