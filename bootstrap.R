#load boot library
library(boot)

#PURPOSE: A bootstrapping function which generates 95% confidence intervals for
#regression coefficients when used as the 'statistic' argument in the function 
#boot()
#INPUTS: The linear model, the data from which the model comes, 
#the index parameters
#OUTPUT: The coefficients of the linear regression model
bst <- function(formula, data, indices){
  d <- data[indices, ]
  fit <- lm(formula, data=d)
  return(coef(fit)) 
}

#The bootstrapping results are stored as 'results'
#1250 replications is the fewest that allow the boot() function to run
#I do not know why that is
results <- boot(data = clean.data, statistic = bst, R = 1250, formula = dataModel)

#View results as density histogram and qqplot 
#The data are normally distributed for all variables 
results
plot(results, index=1) # intercept 
plot(results, index=2) # gestation 
plot(results, index=3) # parity
plot(results, index=4) # ht
plot(results, index=5) # drace
plot(results, index=6) # dwt
plot(results, index=7) # smoke
plot(results, index=8) # number

# Get 95% confidence intervals 
boot.ci(results, type="bca", index=1) # intercept
boot.ci(results, type="bca", index=2) # gestation 
boot.ci(results, type="bca", index=3) # parity
boot.ci(results, type="bca", index=4) # ht
boot.ci(results, type="bca", index=5) # drace
boot.ci(results, type="bca", index=6) # dwt
boot.ci(results, type="bca", index=7) # smoke
boot.ci(results, type="bca", index=8) # number
