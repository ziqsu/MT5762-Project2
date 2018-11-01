#load boot library
library(boot)

#Bootstrapping function which generates 95% confidence intervals for
#regression coefficients
bst <- function(formula, data, indices){
  d <- data[indices, ]
  fit <- lm(formula, data=d)
  return(coef(fit)) 
}

#bootstrapping with 1250 replications
#this is the fewest replications that work
results <- boot(data = clean.data, statistic = bst, R = 1250, formula = dataModel)

# view results as density histogram and qqplot 
results
plot(results, index=1) # intercept 
plot(results, index=2) # gestation 
plot(results, index=3) # parity
plot(results, index=4) # ht
plot(results, index=5) # drace
plot(results, index=6) # dwt
plot(results, index=7) # smoke
plot(results, index=8) # number

# get 95% confidence intervals 
boot.ci(results, type="bca", index=1) # intercept
boot.ci(results, type="bca", index=2) # gestation 
boot.ci(results, type="bca", index=3) # parity
boot.ci(results, type="bca", index=4) # ht
boot.ci(results, type="bca", index=5) # drace
boot.ci(results, type="bca", index=6) # dwt
boot.ci(results, type="bca", index=7) # smoke
boot.ci(results, type="bca", index=8) # number
