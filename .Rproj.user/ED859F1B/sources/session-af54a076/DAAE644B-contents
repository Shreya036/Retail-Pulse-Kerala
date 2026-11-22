
library(tidyverse)
library(lubridate)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(zoo)

# STEP 1: Load historical demand dataset

daily_demand_complete <- read.csv(
  "data/processed/daily_demand_complete.csv"
)

# Convert Date column
daily_demand_complete$Date <- as.Date(
  daily_demand_complete$Date
)

# Check dataset
dim(daily_demand_complete)
head(daily_demand_complete)
str(daily_demand_complete)

# STEP 2: Create date-based features

ml_data <- daily_demand_complete %>%
  arrange(Date) %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Day = day(Date),
    Weekday = wday(Date),
    Weekend = if_else(Weekday %in% c(1, 7), 1, 0)
  )

# Display results
head(ml_data)
dim(ml_data)

# STEP 3: Create lag and rolling demand features

ml_data <- ml_data %>%
  mutate(
    Previous_Day_Demand = lag(Total_Demand, 1),
    Previous_7_Day_Demand = lag(Total_Demand, 7),
    Average_7_Day_Demand = lag(
      zoo::rollmean(
        Total_Demand,
        k = 7,
        fill = NA,
        align = "right"
      ),
      1
    )
  )

# Display results
head(ml_data, 15)

# Check missing values
colSums(is.na(ml_data))

# STEP 4: Remove missing values

ml_data <- ml_data %>%
  drop_na()

# STEP 5: Export dataset

write.csv(
  ml_data,
  "data/processed/ml_feature_engineered.csv",
  row.names = FALSE
)

# Verify file
file.exists(
  "data/processed/ml_feature_engineered.csv"
)

# Check final dimensions
dim(ml_data)

print("STEP 1 COMPLETED SUCCESSFULLY!")


# STEP 2: Prepare Machine Learning Dataset

# Select input features
ml_features <- ml_data %>%
  select(
    Year,
    Month,
    Day,
    Weekday,
    Weekend,
    Previous_Day_Demand,
    Previous_7_Day_Demand,
    Average_7_Day_Demand
  )

# Select target variable
ml_target <- ml_data$Total_Demand

# Check input features
head(ml_features)

# Check target variable
head(ml_target)

# Check dimensions
dim(ml_features)
length(ml_target)

print("STEP 2 COMPLETED SUCCESSFULLY!")


# STEP 3: Split Dataset into Training and Testing Data

# Calculate training size (80%)
train_size <- floor(0.80 * nrow(ml_data))

# Create training data
train_data <- ml_data[1:train_size, ]

# Create testing data
test_data <- ml_data[(train_size + 1):nrow(ml_data), ]

# Separate training features and target
X_train <- ml_features[1:train_size, ]
y_train <- ml_target[1:train_size]

# Separate testing features and target
X_test <- ml_features[(train_size + 1):nrow(ml_data), ]
y_test <- ml_target[(train_size + 1):nrow(ml_data)]

# Check dimensions
dim(X_train)
dim(X_test)

length(y_train)
length(y_test)

# Check date ranges
range(train_data$Date)
range(test_data$Date)

print("STEP 3 COMPLETED SUCCESSFULLY!")


# STEP 4: Linear Regression Model

# Create training dataset
linear_train <- data.frame(
  y_train = y_train,
  X_train
)

# Train Linear Regression Model
linear_model <- lm(
  y_train ~ .,
  data = linear_train
)

# Display model summary
summary(linear_model)

# Predict demand using testing data
linear_predictions <- predict(
  linear_model,
  newdata = X_test
)

# Display first 10 predictions
head(linear_predictions, 10)

# Compare actual and predicted demand
linear_results <- data.frame(
  Actual_Demand = y_test,
  Predicted_Demand = linear_predictions
)

head(linear_results, 10)

print("STEP 4 COMPLETED SUCCESSFULLY!")


# STEP 5: Random Forest Model

# Prepare training dataset
rf_train <- data.frame(
  Total_Demand = y_train,
  X_train
)

# Train Random Forest Model
set.seed(123)

rf_model <- randomForest(
  Total_Demand ~ .,
  data = rf_train,
  ntree = 500,
  importance = TRUE
)

# Display model
print(rf_model)

# Predict demand using testing data
rf_predictions <- predict(
  rf_model,
  newdata = X_test
)

# Compare actual and predicted demand
rf_results <- data.frame(
  Actual_Demand = y_test,
  Predicted_Demand = rf_predictions
)

# Display first 10 predictions
head(rf_results, 10)

print("STEP 5 COMPLETED SUCCESSFULLY!")


# STEP 6: Decision Tree Model

# Prepare training dataset
tree_train <- data.frame(
  Total_Demand = y_train,
  X_train
)

# Train Decision Tree Model
tree_model <- rpart(
  Total_Demand ~ .,
  data = tree_train,
  method = "anova",
  control = rpart.control(
    maxdepth = 5,
    minsplit = 20
  )
)

# Display model
print(tree_model)

# Plot Decision Tree
rpart.plot(
  tree_model,
  main = "Decision Tree - Demand Prediction",
  type = 2,
  extra = 101,
  fallen.leaves = TRUE
)

# Predict demand
tree_predictions <- predict(
  tree_model,
  newdata = X_test
)

# Compare actual and predicted demand
tree_results <- data.frame(
  Actual_Demand = y_test,
  Predicted_Demand = tree_predictions
)

# Display first 10 predictions
head(tree_results, 10)

print("STEP 6 COMPLETED SUCCESSFULLY!")


# STEP 7: Model Evaluation

# Function to calculate evaluation metrics
evaluate_model <- function(actual, predicted) {
  
  MAE <- mean(abs(actual - predicted))
  
  RMSE <- sqrt(mean((actual - predicted)^2))
  
  R2 <- 1 - sum((actual - predicted)^2) /
    sum((actual - mean(actual))^2)
  
  return(data.frame(
    MAE = MAE,
    RMSE = RMSE,
    R2 = R2
  ))
}

# Evaluate Linear Regression
linear_metrics <- evaluate_model(
  y_test,
  linear_predictions
)

# Evaluate Random Forest
rf_metrics <- evaluate_model(
  y_test,
  rf_predictions
)

# Evaluate Decision Tree
tree_metrics <- evaluate_model(
  y_test,
  tree_predictions
)

# Combine model results
model_comparison <- bind_rows(
  Linear_Regression = linear_metrics,
  Random_Forest = rf_metrics,
  Decision_Tree = tree_metrics,
  .id = "Model"
)

# Display comparison
print(model_comparison)

# Identify best model based on lowest RMSE
best_model <- model_comparison %>%
  arrange(RMSE) %>%
  slice(1)

print(best_model)

print("STEP 7 COMPLETED SUCCESSFULLY!")


# STEP 8: Feature Importance

# Calculate feature importance
importance_values <- importance(rf_model)

# Convert into a data frame
importance_df <- data.frame(
  Feature = rownames(importance_values),
  Importance = importance_values[, "%IncMSE"]
)

# Sort by importance
importance_df <- importance_df %>%
  arrange(desc(Importance))

# Display feature importance
print(importance_df)

# Create feature importance chart
importance_chart <- ggplot(
  importance_df,
  aes(
    x = reorder(Feature, Importance),
    y = Importance
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Feature Importance - Random Forest",
    x = "Features",
    y = "Importance"
  ) +
  theme_minimal()

# Display chart
print(importance_chart)

# Save chart
ggsave(
  "reports/feature_importance.png",
  plot = importance_chart,
  width = 9,
  height = 6
)

print("STEP 8 COMPLETED SUCCESSFULLY!")


# STEP 9: Demand Prediction

# Get the latest available date
last_date <- max(ml_data$Date)

# Create future dates (next 30 days)
future_dates <- data.frame(
  Date = seq(
    last_date + 1,
    by = "day",
    length.out = 30
  )
)

# Create future date features
future_data <- future_dates %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Day = day(Date),
    Weekday = wday(Date),
    Weekend = if_else(Weekday %in% c(1, 7), 1, 0)
  )

# Start with the latest historical demand values
historical_demand <- ml_data$Total_Demand

# Create future demand predictions one day at a time
future_predictions <- numeric(30)

for (i in 1:30) {
  
  future_data$Previous_Day_Demand[i] <-
    tail(historical_demand, 1)
  
  future_data$Previous_7_Day_Demand[i] <-
    tail(historical_demand, 7)[1]
  
  future_data$Average_7_Day_Demand[i] <-
    mean(tail(historical_demand, 7))
  
  future_predictions[i] <- predict(
    rf_model,
    newdata = future_data[i, -1]
  )
  
  # Use today's prediction for the next day's features
  historical_demand <- c(
    historical_demand,
    max(0, future_predictions[i])
  )
}

# Create forecast results
future_demand_results <- data.frame(
  Date = future_dates$Date,
  Predicted_Demand = pmax(0, future_predictions)
)

# Display predictions
print(head(future_demand_results, 10))

# Save predictions
write.csv(
  future_demand_results,
  "reports/ml_future_demand_predictions.csv",
  row.names = FALSE
)

print("STEP 9 COMPLETED SUCCESSFULLY!")


# STEP 10: Export Machine Learning Models and Results

# Save trained models
saveRDS(
  linear_model,
  "models/linear_regression_model.rds"
)

saveRDS(
  rf_model,
  "models/random_forest_model.rds"
)

saveRDS(
  tree_model,
  "models/decision_tree_model.rds"
)

# Export model comparison
write.csv(
  model_comparison,
  "reports/ml_model_comparison.csv",
  row.names = FALSE
)

# Export feature importance
write.csv(
  importance_df,
  "reports/ml_feature_importance.csv",
  row.names = FALSE
)

# Export future predictions
write.csv(
  future_demand_results,
  "reports/ml_future_demand_predictions.csv",
  row.names = FALSE
)

# Verify exported files
file.exists("models/linear_regression_model.rds")
file.exists("models/random_forest_model.rds")
file.exists("models/decision_tree_model.rds")

file.exists("reports/ml_model_comparison.csv")
file.exists("reports/ml_feature_importance.csv")
file.exists("reports/ml_future_demand_predictions.csv")

print("STEP 10 COMPLETED SUCCESSFULLY!")
print("PHASE 8 MACHINE LEARNING COMPLETED!")
