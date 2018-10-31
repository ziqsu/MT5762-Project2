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
library(ggplot2)

#Histogram shows that the baby weight values appear to be normally distributed
plot_hist.wt <- ggplot(data, aes(x = wt, y = ..density.. )) +
  geom_histogram(binwidth = (5), colour = "black", fill = "steelblue") +
  ggtitle(" Density Histogram of Birth Weight ") +
  xlab(" Birth weight in ounces ")+ ylab(" Density ")+ theme_dark() 
plot_hist.wt

#Create data frame of baby weight summary statistics
BabyWeight <- c(summary(clean.data$wt))
wt.df <- as.data.frame(BabyWeight)

#Investigate how each variable correlates with baby weight
#These show what variables are likely to have an effect on baby weight
#The highest ones are gestation, mother's height (ht), mother's weight (wt.1)
#and father's weight (dwt)
CorrelationValue <- cor(clean.data, clean.data$wt, use = "complete.obs")
cor.df <- as.data.frame(CorrelationValue)

#We will now explore each variable in turn

########## Exploration of gestation ##########
#The scatterplot shows data that indicate an increase in birth weight as
#gestation period increases
plot_gest <- ggplot(data, aes(x = gestation,y = wt)) +
  geom_point(size = 1) +
  xlab(" Gestation Period (days) " ) + ylab(" Birth Weight (ounces) ") +
  theme_dark() +
  ggtitle(" Scatterplot between Gestation Period and Birth Weight ")
plot_gest

########## Analysis of smoke ##########
#The boxplots show smaller mean for 'smokes now' but it is still within the
#confidence intervals of the other levels of smoking
#Therefore, the effect may not be large.

#Creating labels for the x axis
smoke.box.xlabels <- c("Never", "Smokes now", "Smoked until pregnancy",
                       "Once smoked", "Unknown")

smoke.box <- ggplot(clean.data, aes(factor(smoke), wt)) +
  geom_boxplot(fill = "seagreen4") + 
  labs(title = "Birth Weight per Level of Mother's Smoking",
                        x = "Smoked or not", y = "Babies' weight") +
  scale_x_discrete(labels= smoke.box.xlabels)
smoke.box
