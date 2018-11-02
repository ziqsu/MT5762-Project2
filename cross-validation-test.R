#install package "DAAG"
library(DAAG)

# 5 fold cross-validation for finalModel
# with both Observed and Residual
cv.lm(clean.data.naomit, form.lm = finalModel, plotit = "Observed", m=5)
cv.lm(clean.data.naomit, form.lm = finalModel, plotit = "Residual", m=5)

# 5 fold cross-validation for dataModel
# with both Observed and Residual
cv.lm(clean.data.naomit, form.lm = dataModel, plotit = "Observed", m=5)
cv.lm(clean.data.naomit, form.lm = dataModel, plotit = "Residual", m=5)

# From the plots, we cannot say if dataModel or finalModel is better
# because the five regression lines all seems parallal in both plots
# However from the output, the overall ms of dataModel is 255 whilst which of finalModel is 268
# which means finalModel is little bit better than dataModel

#################

# MSE for finalModel and dataModel
# model with smaller MSE is better

library(dvmisc)
get_mse(finalModel,var.estimate = FALSE)
get_mse(dataModel, var.estimate = FALSE)

# MSE of finalModel is 258 and dataModel is 248
# So, dataModel seems better than finalModel

