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

finalModel.results <- boot(data = clean.data, statistic = bst, R = 1500, formula = finalModel)

# view results as density histogram and qqplot 
finalModel.results
plot(finalModel.results, index=1) # gestation
plot(finalModel.results, index=2) # ht 
plot(finalModel.results, index=3) # ded
plot(finalModel.results, index=4) # dwt
plot(finalModel.results, index=5) # inc
plot(finalModel.results, index=6) # time
plot(finalModel.results, index=7) # number
plot(finalModel.results, index=8) # inc:parity
plot(finalModel.results, index=9) # wt.1:ed
plot(finalModel.results, index=10) # ed:drace
plot(finalModel.results, index=11) # time:ed
plot(finalModel.results, index=12) # ded:smoke

# get 95% confidence intervals 
boot.ci(finalModel.results, type="bca", index=1) # gestation
boot.ci(finalModel.results, type="bca", index=2) # ht 
boot.ci(finalModel.results, type="bca", index=3) # ded
boot.ci(finalModel.results, type="bca", index=4) # dwt
boot.ci(finalModel.results, type="bca", index=5) # inc
boot.ci(finalModel.results, type="bca", index=6) # time
boot.ci(finalModel.results, type="bca", index=7) # number
boot.ci(finalModel.results, type="bca", index=8) # inc:parity
boot.ci(finalModel.results, type="bca", index=9) # wt.1:ed
boot.ci(finalModel.results, type="bca", index=10) # ed:drace
boot.ci(finalModel.results, type="bca", index=11) # time:ed
boot.ci(finalModel.results, type="bca", index=12) # ded:smoke
