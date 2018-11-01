# Bootstrap 95% CI for regression coefficients 
library(boot)
# function to obtain regression weights 
bs <- function(formula,data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula,data=d)
  return(coef(fit)) 
} 
# bootstrapping with 1000 replications 
results <- boot(data=clean.data, statistic=bs, 
                R=10000, formula = wt ~ gestation + parity + ht + drace + dwt + smoke + 
                  number)

# view results
results
plot(results, index=1) # intercept 
plot(results, index=2) # wt 
plot(results, index=3) # disp 

# get 95% confidence intervals 
boot.ci(results, type="bca", index=1) # intercept 
boot.ci(results, type="bca", index=2) # gestation
boot.ci(results, type="bca", index=3) # 