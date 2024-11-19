library(caret)

# setting seed to generate a 
# reproducible random sampling
set.seed(123)

# creating training data as 80% of the dataset
data_peat_noNAs<-subset(data_peat,
                        !is.na(age)&!is.na(temp)&
                          !is.na(moist)&!is.na(nutrient)&
                          !is.na(fire)&!is.na(dry))

random_sample <- createDataPartition(data_peat_noNAs $ tot_Sphagnum_prop, 
                                     p = 0.8, list = FALSE)

# generating training dataset
# from the random_sample
training_dataset  <- data_peat_noNAs[random_sample, ]

# generating testing dataset
# from rows which are not 
# included in random_sample
testing_dataset <- data_peat_noNAs[-random_sample, ]

# Building the model

# training the model 
model <- glmmTMB(tot_Sphagnum_prop~age+temp+moist+nutrient+
                   fire+dry,family="beta_family",
                 ziformula=~.,data=training_dataset)
summary(model)

# predicting the target variable
predictions <- predict(model, testing_dataset)

# computing model performance metrics
data.frame( R2 = R2(predictions, testing_dataset $ tot_Sphagnum_prop),
            RMSE = RMSE(predictions, testing_dataset $ tot_Sphagnum_prop),
            MAE = MAE(predictions, testing_dataset $ tot_Sphagnum_prop))

# R program to implement
# Leave one out cross validation

# defining training control
# as Leave One Out Cross Validation
train_control <- trainControl(method = "LOOCV")

# training the model by assigning sales column
# as target variable and rest other column
# as independent variable
model <- train(sales ~., data = marketing, 
               method = "lm",
               trControl = train_control)

# printing model performance metrics
# along with other details
print(model)

########################

LOOCV <- lapply(1:nrow(data_peat_noNAs), function(x){
  m1 <- glmmTMB(tot_Sphagnum_prop~age+temp+moist+nutrient+
                  fire+dry,family="beta_family",
                ziformula=~.,data = data_peat_noNAs[-x,])
  return(predict(m1, data_peat_noNAs[x,], type = "response"))
})    

median(abs(unlist(LOOCV) - data_peat_noNAs$tot_Sphagnum_prop))

# Calculate RMSE

# Step 1: Obtain predictions and actual values
predictions <- unlist(LOOCV)
actual_values <- data_peat_noNAs$tot_Sphagnum_prop

# Step 2: Calculate the squared errors
squared_errors <- (predictions - actual_values)^2

# Step 3: Compute the mean of the squared errors
mean_squared_error <- mean(squared_errors)

# Step 4: Take the square root of the mean squared error to get RMSE
rmse <- sqrt(mean_squared_error)

# Print RMSE
print(rmse)
