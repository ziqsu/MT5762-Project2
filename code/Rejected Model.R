# Fitting interaction models for  certain variables against baby weight

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

#Checking AIC scores for each model
AIC(race_wt.1)
AIC(smoke_wt.1)
AIC(parity_wt.1)
AIC(inc_wt.1)

# Model diagnostics for each model built, 
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
library(car)
durbinWatsonTest(race_wt.1)

# Error independence of  model between mother's weight and smoking against baby weight
durbinWatsonTest(smoke_wt.1)

# Error independence of mother's weight and parity against baby weight
durbinWatsonTest(parity_wt.1)

# Error independence of model between mother's weight and income against baby weight
durbinWatsonTest(inc_wt.1)

# Ncv test for the models
ncvTest(smoke_wt.1)
ncvTest(inc_wt.1)
ncvTest(parity_wt.1)