# Loading dataset into R
data <-  read.table("babies23.data", header = T)
data
# Data Cleaning process begins
# Checking for missing values in data
sum(is.na(data))

library(DataExplorer)
# Missing values plot
plot_missing(data)

# Checking class of each variable
sapply(data, class)
# Converting all variables to numeric using lapply
NCOL(data)
data[,1:23] <- lapply(data[,1:23], as.numeric)

# Adjustment of unknowns
data$gestation[data$gestation == "999"] <- NA
data$age[data$age == "99"] <- NA
data$ed[data$ed == "9"] <- NA
data$ht[data$ht == "99"] <- NA
data$wt[data$wt == "99"] <- NA
data$drace[data$drace == "99"] <- NA
data$dage[data$dage == "99"] <- NA
data$ded[data$ded == "9"] <- NA
data$dht[data$dht == "99"] <- NA
data$dwt[data$dwt == "999"] <- NA
data$inc[data$inc == "98"] <- NA
data$smoke[data$smoke == "9"] <- NA
data$time[data$time == "99"] <- NA
data$time[data$time == "98"] <- NA
data$number[data$number == "98"] <- NA

# Summary of dataset
summary(data)
str(data)

#Exploratory analysis of the data
library(ggplot2)
plot_hist.wt <- ggplot(data, aes(x = wt, y = ..density.. )) +
  geom_histogram(binwidth = (5), colour = "black", fill = "steelblue") +
  ggtitle(" Density histogram of Birth weight ") +
  xlab(" Birth weight in ounces ")+ ylab(" Density ")+ theme_dark() 
plot_hist.wt

plot_gest <- ggplot(data, aes(x = log(gestation),y = wt)) +
  geom_point(aes(colour = wt), size = 5) +
  xlab(" Gestation period " ) + ylab(" Birth weight in ounces ") +
  theme_dark() +
  ggtitle(" Scatterplot between Gestation period and Birth Weight ")
plot_gest

plot_dwt <- ggplot(data, aes(x = dwt,y = wt)) +
  geom_point(aes(colour = wt), size = 5) +
  xlab(" Father's weighgt in pounds " ) + ylab(" Birth weight in ounces ") +
  theme_dark() +
  ggtitle(" Scatterplot between Father's weight and Birth Weight ")
plot_dwt

plot_smoke <- ggplot(data = data, aes(x = smoke, y = wt)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_dark()
plot_smoke

plot_age <- ggplot(data, aes(x = age,y = wt)) +
  geom_point(aes(colour = wt), size = 5) +
  xlab(" Mother's Age " ) + ylab(" Birth weight in ounces ") +
  theme_bw() +
  ggtitle(" Scatterplot between Mother's Age and Birth Weight ")
plot_age

Groupby_race <- group_by(data, race)





