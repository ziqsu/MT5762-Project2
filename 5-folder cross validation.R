# Consider 5 fold cross validation:
# We will divide this data set of 1236 observations into 5 partitions
# Generate the training and test samples
set.seed(5762)

##
library(boot)

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
library(DAAG)
cv.lm(fit, k=5, seed=5762, max_cores = 1)

parameterGrid <- expand.grid(mtry=c(1,2,3,4,5))

modelRandom <- train (wt~gestation, data = trainDF, method = "rf", trControl = ControlParameters, tuneGrid = parameterGrid)

