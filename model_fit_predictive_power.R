# Code for calculating measures of model fit and predictive power

# I am using this model as an example
# I think that you can do something similar for each model and include these values in the tables

# This is the model I am using
mod_abund_tot_Sphagnum_temp<-glmmTMB(tot_Sphagnum_prop~temp+moist+nutrient+
                                       fire+dry,family="beta_family",
                                     ziformula=~.,data=data_peat) 

# Calculate R2
library(performance)
r2_zeroinflated(mod_abund_tot_Sphagnum_temp) # You can show the adjusted R2 value

# Compute predictions

# Here you have to use the data without missing values
# The code below creates a subset of the data where none of the variables in your model is NA
data_peat_noNAs<-subset(data_peat,!is.na(tot_Sphagnum_prop)&!is.na(temp)&
                          !is.na(moist)&!is.na(nutrient)&!is.na(fire)&!is.na(dry))

predictions <- predict(mod_abund_tot_Sphagnum_temp,newdata = data_peat_noNAs, 
                       type = "response")

library(caret)
# Calculate RMSE
RMSE(predictions,data_peat_noNAs$tot_Sphagnum_prop)

# Calculate MAE
MAE(predictions,data_peat_noNAs$tot_Sphagnum_prop)
