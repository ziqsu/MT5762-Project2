#setwd("~/Masters/")
library(tidyverse)
library(ggplot2)
library(car)
library(GGally)
library(effects)
library(MuMIn)

setwd("~/Masters/")
babies.data <- read.table("MT5762/Assignment 2/MT5762-Project2/babies23.data", 
                          header = TRUE)
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
clean.data$wt.1[clean.data$wt.1 == "999"] <- NA


#make some factors numeric
clean.data <- clean.data %>% mutate_each(funs(as.numeric), 5)
clean.data <- clean.data %>% mutate_each(funs(as.numeric), 7)
clean.data <- clean.data %>% mutate_each(funs(as.numeric), 10)
clean.data <- clean.data %>% mutate_each(funs(as.numeric), 12:13)
clean.data <- clean.data %>% mutate_each(funs(as.numeric), 15)
clean.data <- clean.data %>% mutate_each(funs(as.numeric), 17:18)

####### Exploration of the birthweight data #######
#normally distributed
hist(clean.data$wt)

summary(clean.data$wt)

##################################

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
#(smokes now) compared to never smoked (the baseline)
lm.smoke <- lm(wt ~ factor(smoke), data = clean.data)
summary(lm.smoke)

#the boxplots show smaller mean for 'smokes now' but it is still within the
#confidence intervals of the other levels of smoking
smoke.box.xlabels <- c("Never", "Smokes now", "Smoked until pregnancy",
                       "Once smoked", "Unknown")

#tried to add means to boxplots but can't get it to work
smoke.means <- aggregate(wt ~ factor(smoke), clean.data, mean)

smoke.box <- ggplot(clean.data, aes(factor(smoke), wt)) +
  geom_boxplot() + labs(title = "Babies' weight per level of mother's smoking",
                        x = "Smoked or not", y = "Babies' weight") +
  scale_x_discrete(labels= smoke.box.xlabels) #+ 
  #geom_text(data = smoke.means, aes(label = wt, y = wt + 0.08))
smoke.box

########## Analysis of wt.1 (mother's weight) ##########

#scatterplot of mother's weight against baby's weight
scat.mwt <- ggplot(clean.data, aes(wt.1, wt)) +
  geom_point() + geom_smooth(method = lm)
scat.mwt

#linear regression for mother's weight - results are NOT significant
lm.mwt <- lm(wt ~ wt.1, data = clean.data)
summary(lm.mwt)


########## Analysis of number (of cigarettes smoked by mother) ##########

#boxplots don't show much
number.box <- ggplot(clean.data, aes(factor(number), wt)) +
  geom_boxplot()
number.box

#linear regression for number of cigarettes smoked
lm.number <- lm(wt ~ factor(number), data = clean.data)
summary(lm.number)


########## Analysis of height (of mother) ##########

#scatterplot of mother's height against baby's weight
scat.mht <- ggplot(clean.data, aes(ht, wt)) +
  geom_point() + geom_smooth(method = lm)
scat.mht

#linear regression for mother's height - results are significant
lm.mht <- lm(wt ~ ht, data = clean.data)
summary(lm.mht)


########## Analysis of weight (of father) ##########

#scatterplot of father's weight against baby's weight
scat.dwt <- ggplot(clean.data, aes(dwt, wt)) +
  geom_point() + geom_smooth(method = lm)
scat.dwt

#linear regression for father's weight - results are significant
lm.dwt <- lm(wt ~ dwt, data = clean.data)
summary(lm.dwt)


############################################################








##ignore AIC below for now, may try later
#let's try AIC...
#data_NONA <- na.omit(clean.data)
#fullModel <- lm(wt ~ ., data = data_NONA)
#step(fullModel)
clean.data.naomit <- na.omit(clean.data)
# select data that does not contain id and data of birth
# consider this two factor does not have effect on baby birth weight
# on the real life
clean.data.naomit <- clean.data.naomit %>% dplyr::select(-id, -date)
#factor(clean.data.naomit$id)
dataModel <- lm(wt ~., data = clean.data.naomit)
summary(dataModel)
#try to use Anova
Anova(dataModel)
#model selection use AIC
dataModel <- step(dataModel)
Anova(dataModel)
#check about normality of dataModel's residual
qqnorm(resid(dataModel))
qqline(resid(dataModel))
#the qq plot looks great but the shapiro test, p value is large than 0.05,
# I think the reason of that maybe that we have a lot of sample(more than 1200)
# so the shapiro test is really senstive to the outlier
shapiro.test(resid(dataModel))
hist(resid(dataModel))

# we track down the extreme residuals
bigResid <- which(abs(resid(dataModel))>5)
clean.data.naomit[bigResid,]
#plot residuals against fitted values
dataResid <- resid(dataModel)
plot(fitted(dataModel),dataResid, ylab= "Residuals", xlab = "Fitted Values")
#it looks good
#https://onlinecourses.science.psu.edu/stat501/node/277/

# do Breusche-Pagan test with respect to fitted model
ncvTest(dataModel)
# null hypothesis: constant error variance. "If we have constant error variance
#then the variation in the residuals should be unrelated to any coveriant."
#MT5761 notes page 22

# need to write durbinWatsonTest on model
durbinWatsonTest(dataModel)
#null hypothesis: error are uncorrelated, fail to reject the null hypothesis
# our  p-value is 0.056, >0.05
plot(dataModel, which = 1:2)


#collinearity
numericOnly <- clean.data.naomit %>% select_if(is.numeric)
#use with caution, picture is sooo huge and difficult to generate
# and do harm to my computer and not useful because we have sooo many variabales
#ggpairs(numericOnly)

vif(dataModel)
# all number is less than 10, do not have to delete any variable

#calculate confidence interval of the model
confint(dataModel)

#add more effect plot if you want and select variable that you 
# think is interested
plot(effect(term="gestation", mod = dataModel))
plot(effect(term="smoke", mod = dataModel))
plot(effect(term="number", mod = dataModel))

#BICModel <- lm(wt ~., data = clean.data.naomit)
#BICModel <- dredge(BICModel, rank = "BIC")
firstorderModel <- lm(wt ~.*., data = numericOnly)
summary(firstorderModel)

#firstorderModel <- firstorderModel %>% update()
#try to use Anova
#Anova(firstorderModel)
#model selection use AIC


firstorderModel <- step(firstorderModel)
summary(firstorderModel)
Anova(firstorderModel)
qqnorm(resid(firstorderModel))
qqline(resid(firstorderModel))
shapiro.test(resid(firstorderModel))
hist(resid(firstorderModel))
firstorderResid <- resid(firstorderModel)
plot(fitted(firstorderModel),firstorderResid, ylab= "Residuals", xlab = "Fitted Values")

ncvTest(firstorderModel)
durbinWatsonTest(firstorderModel)
plot(firstorderModel, which = 1:2)
k<-vif(firstorderModel)
k[which.max(k)]
alteredModel <-update(firstorderModel,.~.-ht:marital )
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-race )
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-smoke )
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-dht:race)
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-dage)
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-age:marital)
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-drace)
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-dht:inc)
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-gestation:number)
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-wt.1)
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-ht:smoke)
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-marital:dage )
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-ed )
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-parity )
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-age:dwt )
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-marital:race  )
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-age:race )
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-dwt:wt.1 )
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-gestation:drace )
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-ded:dwt )
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-dwt:dage )
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-gestation:smoke )
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-ded:time )
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-marital:ed )
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-dage:race )
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-dwt:ed )
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-gestation:parity)
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-ed:smoke)
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-age:drace)
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-dwt:race)
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-dwt:smoke)
p<-vif(alteredModel)
p[which.max(p)]
alteredModel <-update(alteredModel,.~.-inc:ed)
p<-vif(alteredModel)
p[which.max(p)]

summary(alteredModel)
qqnorm(resid(alteredModel))
qqline(resid(alteredModel))
shapiro.test(resid(alteredModel))
Anova(alteredModel)
confint(alteredModel)


#maybe useful, need to ask professor
finalModel <- step(alteredModel)
vif(finalModel)

boot.dataModel <- function(inputdata, nboot){
  NumofRow <- nrow(inputdata)
  bootResult <- matrix(NA,nrow = nboot, ncol = 8)
  for(i in 1:nboot){
    bootdata <- inputdata[sample(1:NumofRow),NumofRow]
    bootLM <- lm(wt~ gestation+parity+ht+drace+dwt+smoke+number, data = inputdata)
    print(coef(bootLM))
    bootResult[i,] <- coef(bootLM)
  }
  bootResult
  
} 