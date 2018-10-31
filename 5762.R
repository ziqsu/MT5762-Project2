# Loading dataset into R
data <-  read.table(file.choose(), header = T)
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
data$wt.1[data$wt == "99"] <- NA
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

plot_gest <- ggplot(data, aes(x = gestation,y = wt)) +
  geom_point(aes(colour = wt), size = 5) +
  xlab(" Gestation period " ) + ylab(" Birth weight in ounces ") +
  theme_dark() +
  ggtitle(" Scatterplot between Gestation period and Birth Weight ")
plot_gest

plot_dwt <- ggplot(data, aes(x = dwt,y = wt)) +
  geom_point(aes(colour = dwt), size = 5) +
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
  xlab(" Mother's age " ) + ylab(" Birth weight in ounces ") +
  theme_bw() +
  ggtitle(" Scatterplot between Mother's Age and Birth Weight ")
plot_age

plot_wt.1 <- ggplot(data, aes(x = wt.1,y = wt)) +
  geom_point(aes(colour = wt), size = 5) +
  xlab(" Mother's weight " ) + ylab(" Birth weight in ounces ") +
  theme_bw() +
  ggtitle(" Scatterplot between Mother's Age and Birth Weight ")
plot_wt.1

boxplot((wt)~smoke, data, main = toupper("Boxplot for wt and smoking"),
        xlab = "Smoking levels", ylab = "wt", col = "Blue")
Groupby_race <- group_by(data, race)




plot_correlation(data)

# Model fit for certain variables against baby weight
# Changing the class of categorical variables back to integers
cols_to_change = c(8, 9, 11, 14, 16, 20:23)
for(i in cols_to_change){
  class(data[, i]) = "integer"
}
cols_to_change

# Fitting models for  certain variables against baby weight

# Linear model between race and mother weight against baby weight

race_wt.1 <- lm(wt ~ race*wt.1, data = data)
summary(race_wt.1)
anova(race_wt.1)

# Linear model between mother's weight and smoking against baby weight 
smoke_wt.1 <- lm(wt ~ smoke*wt.1, data = data)
summary(smoke_wt.1)
anova(smoke_wt.1)

# Linear model between mother's weight and parity against baby weight 
parity_wt.1 <- lm(wt ~ parity*wt.1, data = data)
summary(parity_wt.1)
anova(parity_wt.1)

# Linear model between mother's weight and time against baby weight
time_wt.1 <- lm(wt ~ time*wt.1, data = data)
summary(time_wt.1)
anova(time_wt.1)

# Linear model between mother's weight and income against baby weight
inc_wt.1 <- lm(wt ~ inc*wt.1, data = data)
summary(inc_wt.1)
anova(inc_wt.1)


# Model diagnostics for each model built
# Error shape and distribution of model between race and mother weight against baby weight
qqnorm(resid(race_wt.1))
shapiro.test(resid(race_wt.1))
hist(resid(race_wt.1))

# Error shape and distribution of model between mother's weight and smoking against baby weight
qqnorm(resid(smoke_wt.1))
shapiro.test(resid(smoke_wt.1))
hist(resid(smoke_wt.1))

# Error shape and distribution of model between mother's weight and parity against baby weight
qqnorm(resid(parity_wt.1))
shapiro.test(resid(parity_wt.1))
hist(resid(parity_wt.1))

# Error shape and distribution of  model between mother's weight and income against baby weight
qqnorm(resid(inc_wt.1))
shapiro.test(resid(inc_wt.1))
hist(resid(inc_wt.1))

# Error spread of model between race, mother weight against baby weight
race_resid <- resid(race_wt.1)
plot(fitted(race_wt.1), race_resid, ylab = 'residuals', xlab = 'Fitted values')

# Error spread of model between mother's weight and smoking against baby weight
smoke_resid <- resid(smoke_wt.1)
plot(fitted(smoke_wt.1), smoke_resid, ylab = 'residuals', xlab = 'Fitted values')

# Error spread of model between mother's weight and parity against baby weight
parity_resid <- resid(parity_wt.1)
plot(fitted(parity_wt.1), parity_resid, ylab = 'residuals', xlab = 'Fitted values')

# Error spread of model between mother's weight and income against baby weight
inc_resid <- resid(inc_wt.1)
plot(fitted(inc_wt.1), inc_resid, ylab = 'residuals', xlab = 'Fitted values')

# Error independence of model between race, mother weight against baby weight
durbinWatsonTest(race_wt.1)
library(car)
vif()