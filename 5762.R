# Loading dataset into R
data <-  read.table(file.choose(), header = T)
data
# Data Cleaning process begins
# Checking for missing values in data
sum(is.na(data))
library(DataExplorer)
# Missing values plot
plot_missing(data)
# No missing values present
# Checking class of each variable
sapply(data, class)
# Converting all variables to numeric using lapply
NCOL(data)
data[,1:23] <- lapply(data[,1:23], as.numeric)
# Summary of dataset
summary(data)
str(data)

