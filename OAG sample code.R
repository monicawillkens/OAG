# Modeling and making prediction

library("tidyverse")
library("caret")
full_data <- read_csv("df_for_387_only_July_cleaned.csv")

#remove irrelevant columns
full_data$total_electric_HVAC_energy_used<-NULL
full_data$month<- NULL
full_data$ducts<- NULL
full_data$county<- NULL
full_data$bldg_id<- NULL
full_data$roof_insulation<- NULL
full_data$lat<- NULL
full_data$long<-NULL

summary(full_data)

# Create a model for prediction using `train_set`, and evaluate your model's performance using `test_set`.

#data split
trainIndex <- createDataPartition(full_data$energy_per_sqft, p = .8, list = FALSE)
train_set <- full_data[trainIndex, ]
test_set <- full_data[-trainIndex, ]

#linear regression model
lm_model <- lm(energy_per_sqft ~ ., data=full_data)
summary(lm_model)

## Make predictions on the test set
test_predictions <- predict(lm_model, newdata=test_set)

## Calculate RMSE (Root Mean Squared Error)
test_actuals <- test_set$energy_per_sqft
rmse <- sqrt(mean((test_predictions - test_actuals)^2))
print(paste("RMSE:", rmse))

# support vector regression
library(e1071)
svr_model <- svm(energy_per_sqft ~ ., data=train_set, type='eps-regression')
print(svr_model)

## Make predictions on the test set
test_predictions_svr <- predict(svr_model, newdata=test_set)

## Calculate RMSE
test_actuals_svr <- test_set$energy_per_sqft
rmse_svr <- sqrt(mean((test_predictions_svr - test_actuals_svr)^2))
print(paste("RMSE for SVR on Test Set:", rmse_svr))

# Compare RMSE of these three models. 

# decision tree
library(rpart)
library(rpart.plot)
library(caret)

## Train the decision tree model
dt_model <- train(energy_per_sqft ~ ., data=train_set, method="rpart")

## Visualize the decision tree
rpart.plot(dt_model$finalModel)

## Make predictions on the test set
test_predictions_dt <- predict(dt_model, newdata=test_set)

## Calculate RMSE
test_actuals_dt <- test_set$energy_per_sqft
rmse_dt <- sqrt(mean((test_predictions_dt - test_actuals_dt)^2))
print(paste("RMSE for Decision Tree on Test Set:", rmse_dt))


# Assume the overall temperature increases by 5 degrees in the next 10 years.

new_data <- full_data
new_data$temp <- new_data$temp + 5
new_data$energy_per_sqft <- NULL

view(new_data)
# dataset created by modifying the temperature in the full data.
# Calculate the total energy consumption by multiplying `energy_per_sqft` with `sqft` to determine the peak energy demand.


# Predictions with the linear model
new_data$lm_predictions <- predict(lm_model, newdata=new_data)

# Predictions with the support vector regression model
new_data$svr_predictions <- predict(svr_model, newdata=new_data)

# Predictions with the decision tree model
new_data$dt_predictions <- predict(dt_model, newdata=new_data)

# get total energy usage by multiplying sqft
new_data$total_lm_predictions<-new_data$lm_predictions*new_data$sqft
new_data$total_svr_predictions<-new_data$svr_predictions*new_data$sqft
new_data$total_dt_predictions<-new_data$dt_predictions*new_data$sqft

library(dplyr)

# Create a new dataframe with the hourly sum of energy usage predictions
#
hourly_energy_usage <- new_data %>%
  group_by(hour) %>%
  summarise(
    lm_sum = sum(total_lm_predictions),
    svr_sum = sum(total_svr_predictions),
    dt_sum = sum(total_dt_predictions)
  )

daily_energy_usage <- new_data %>%
  group_by(day) %>%
  summarise(
    lm_sum = sum(total_lm_predictions),
    svr_sum = sum(total_svr_predictions),
    dt_sum = sum(total_dt_predictions)
  )


# Visualize the prediction results
library(ggplot2)
ggplot(hourly_energy_usage, aes(x = hour)) + 
  geom_line(aes(y = lm_sum, color = "Linear Regression")) + 
  geom_line(aes(y = svr_sum, color = "Support Vector Regression")) + 
  geom_line(aes(y = dt_sum, color = "Decision Tree")) + 
  labs(x = "Hour of the Day", 
       y = "Total Energy Usage (Predicted)", 
       title = "Hourly Sum of Predicted Energy Usage") +
  scale_color_manual(values = c("Linear Regression" = "blue", "Support Vector Regression" = "red", "Decision Tree" = "green")) +
  scale_x_continuous(breaks = 0:23) + # This makes all hours are shown on the x-axis
  theme_minimal()

ggplot(daily_energy_usage, aes(x = day)) + 
  geom_line(aes(y = lm_sum, color = "Linear Regression")) + 
  geom_line(aes(y = svr_sum, color = "Support Vector Regression")) + 
  geom_line(aes(y = dt_sum, color = "Decision Tree")) + 
  labs(x = "Day of the Month", 
       y = "Total Energy Usage (Predicted)", 
       title = "Daily Sum of Predicted Energy Usage") +
  scale_color_manual(values = c("Linear Regression" = "blue", "Support Vector Regression" = "red", "Decision Tree" = "green")) +
  scale_x_continuous(breaks = 0:31) + # This makes all days are shown on the x-axis
  theme_minimal()