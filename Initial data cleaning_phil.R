#setwd("~/Masters/")
library(tidyverse)
library(ggplot2)
babies.data <- read.table("babies23.data", header = TRUE)
#since we are working in our directory, I change the directory that I think that
#people use this project can run it.

#observations from data set:
#   pluralty is always 5
#   outcome is always 1
#   there are values of 999 for gestation but readme doc does not clarify if
#       these are unknown - CLEANED ANYWAY
#   all subjects are male
#   for race, I'm unsure why white is assigned six values (0-5) - one unknown
#   two unknown ages (mother) - CLEANED
#   one unknown education (mother) - CLEANED
#   many unknown heights (mother) - CLEANED
#   many unkown weights (mother) - CLEANED
#   five unknown fathers' races as well as values of 10? - 99s CLEANED -
#   many unknown fathers' ages - CLEANED
#   many unknown fathers' educations - CLEANED
#   many unknown fathers' heights - CLEANED
#   many unknown fathers' weights - CLEANED
#   no explanation of 0 in marital status - assume unknown?
#   many unknown incomes - CLEANED
#   ten unknown smokers - CLEANED
#   nine unknown quitting times, one not asked - CLEANED
#   ten unknown number of cigarettes smoked - CLEANED

##### cleaning the data as per unknown values above #####

clean.data <- babies.data
clean.data$gestation[clean.data$gestation == "999"] <- NA
clean.data$age[clean.data$age == "99"] <- NA
clean.data$ed[clean.data$ed == "9"] <- NA
clean.data$ht[clean.data$ht == "99"] <- NA
clean.data$wt[clean.data$wt == "99"] <- NA
clean.data$drace[clean.data$drace == "99"] <- NA
clean.data$dage[clean.data$dage == "99"] <- NA
clean.data$ded[clean.data$ded == "9"] <- NA
clean.data$dht[clean.data$dht == "99"] <- NA
clean.data$dwt[clean.data$dwt == "999"] <- NA
clean.data$inc[clean.data$inc == "98"] <- NA
clean.data$smoke[clean.data$smoke == "9"] <- NA
clean.data$time[clean.data$time == "99"] <- NA
clean.data$time[clean.data$time == "98"] <- NA
clean.data$number[clean.data$number == "98"] <- NA

#make some factors numeric
clean.data <- clean.data %>% mutate_each(funs(as.numeric), 5)
clean.data <- clean.data %>% mutate_each(funs(as.numeric), 7)
clean.data <- clean.data %>% mutate_each(funs(as.numeric), 10)
clean.data <- clean.data %>% mutate_each(funs(as.numeric), 12:13)
clean.data <- clean.data %>% mutate_each(funs(as.numeric), 15)
clean.data <- clean.data %>% mutate_each(funs(as.numeric), 17:18)

#install.packages("corrplot")
library(corrplot)
cor.data <- cor(clean.data, use = "complete.obs")
corrplot(cor.data, method = "circle", type = "lower")

#create correlation values for wt with all other numeric variables
cor.vec <- c(rep(0, ncol(clean.data)))
for(i in 1:ncol(clean.data)){
  cor.vec[i] <- cor(clean.data$wt, clean.data[,i], use = "complete.obs")
}

cor.vec
#no particularly strong correlations but gestation is strongest at 0.19
#I appreciate that this vector isn't clear. I counted along to tell what
#was what. Will make clearer if used in report.

####### Linear regression for baby weight and all numeric variables #######

#Gestation, mother's height and father's weight appear to be significant
lin.reg <- lm(wt ~ gestation + age + ht + wt.1 + dage + dht + dwt, 
              data = clean.data)
summary(lin.reg)

###########################################################################


########## Analysis of gestation ##########

#scatterplot of gestation against birth weight
scat.gest <- ggplot(clean.data, aes(gestation, wt)) +
  geom_point() + geom_smooth(method = lm)
scat.gest

#linear regression for gestation - results are significant
lm.gest <- lm(wt ~ gestation, data = clean.data)
summary(lm.gest)


########## Analysis of smoke ##########

#linear regression for smoke - results are significant for factor 1
#(smokes now)
lm.smoke <- lm(wt ~ factor(smoke), data = clean.data)
summary(lm.smoke)

#the boxplots show smaller mean for 'smokes now' but it is still within the
#confidence intervals of the other levels of smoking
smoke.box <- ggplot(clean.data, aes(factor(smoke), wt)) +
  geom_boxplot()
smoke.box

########## Analysis of wt.1 (mother's weight) ##########

#scatterplot of mother's weight against baby's weight
scat.mwt <- ggplot(clean.data, aes(wt.1, wt)) +
  geom_point() + geom_smooth(method = lm)
scat.mwt

#linear regression for mother's weight - results are NOT significant
lm.mwt <- lm(wt ~ wt.1, data = clean.data)
summary(lm.mwt)

############################################################



##ignore AIC below for now, may try later
#let's try AIC...
#data_NONA <- na.omit(clean.data)
#fullModel <- lm(wt ~ ., data = data_NONA)
#step(fullModel)
clean.data.naomit <- na.omit(clean.data)
dataModel <- lm(wt ~., data = clean.data.naomit)
summary(dataModel)
#try to use Anova, it does not work, may need to install lib
anova(dataModel)
#model selection use AIC
dataModel <- step(dataModel)
anova(dataModel)
#check about normality of dataModel's residual
qqnorm(resid(dataModel))
qqline(resid(dataModel))
shapiro.test(resid(dataModel))
hist(resid(dataModel))
bigResid <- which(abs(resid(dataModel))>5)
clean.data.naomit[bigResid,]
