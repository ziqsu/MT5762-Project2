library(DAAG)
# 5 fold cross-validation for finalModel
cv.lm(clean.data.naomit, form.lm = finalModel, plotit = "Observed", m=5)
cv.lm(clean.data.naomit, form.lm = finalModel, plotit = "Residual", m=5)

# 5 fold cross-validation for dataModel
cv.lm(clean.data.naomit, form.lm = dataModel, plotit = "Observed", m=5)
cv.lm(clean.data.naomit, form.lm = dataModel, plotit = "Residual", m=5)

# MSE for finalModel and dataModel
# model with smaller MSE is better

library(dvmisc)
get_mse(finalModel,var.estimate = FALSE)
get_mse(dataModel, var.estimate = FALSE)
