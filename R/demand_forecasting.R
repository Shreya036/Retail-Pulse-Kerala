# Load required packages
library(tidyverse)
library(lubridate)
library(forecast)

# Load cleaned retail dataset
retail_clean <- readRDS(
  "data/processed/retail_cleaned.rds"
)

# Check dataset
dim(retail_clean)
names(retail_clean)

# Check date range
min(retail_clean$Date, na.rm = TRUE)
max(retail_clean$Date, na.rm = TRUE)

# Create daily demand dataset
daily_demand <- retail_clean %>%
  filter(Quantity > 0) %>%
  group_by(Date) %>%
  summarise(
    Total_Demand = sum(Quantity, na.rm = TRUE),
    Total_Revenue = sum(Revenue, na.rm = TRUE),
    Total_Orders = n_distinct(Invoice),
    .groups = "drop"
  ) %>%
  arrange(Date)

# Display daily demand
head(daily_demand, 10)

# Check dimensions
dim(daily_demand)

# Summary statistics
summary(daily_demand)

print("Demand forecasting dataset prepared successfully!")

# 1. Total demand
total_demand <- sum(
  daily_demand$Total_Demand,
  na.rm = TRUE
)

cat("Total Demand:", total_demand, "\n")

# 2. Average daily demand
average_daily_demand <- mean(
  daily_demand$Total_Demand,
  na.rm = TRUE
)

cat("Average Daily Demand:", average_daily_demand, "\n")

# 3. Highest demand day
highest_demand_day <- daily_demand %>%
  arrange(desc(Total_Demand)) %>%
  slice_head(n = 1)

print("Highest Demand Day:")
print(highest_demand_day)

# 4. Lowest demand day
lowest_demand_day <- daily_demand %>%
  arrange(Total_Demand) %>%
  slice_head(n = 1)

print("Lowest Demand Day:")
print(lowest_demand_day)

# 5. Highest revenue day
highest_revenue_day <- daily_demand %>%
  arrange(desc(Total_Revenue)) %>%
  slice_head(n = 1)

print("Highest Revenue Day:")
print(highest_revenue_day)

# 6. Daily demand variation
demand_sd <- sd(
  daily_demand$Total_Demand,
  na.rm = TRUE
)

cat("Demand Standard Deviation:", demand_sd, "\n")

print("STEP 2 COMPLETED SUCCESSFULLY!")


# Create complete date sequence
complete_dates <- tibble(
  Date = seq(
    min(daily_demand$Date),
    max(daily_demand$Date),
    by = "day"
  )
)

# Check total calendar days
cat("Total Calendar Days:", nrow(complete_dates), "\n")

# Find missing dates
missing_dates <- complete_dates %>%
  anti_join(daily_demand, by = "Date")

cat("Missing Dates:", nrow(missing_dates), "\n")

# Display missing dates
print(head(missing_dates, 20))

# Fill missing dates with zero demand
daily_demand_complete <- complete_dates %>%
  left_join(daily_demand, by = "Date") %>%
  mutate(
    Total_Demand = replace_na(Total_Demand, 0),
    Total_Revenue = replace_na(Total_Revenue, 0),
    Total_Orders = replace_na(Total_Orders, 0)
  ) %>%
  arrange(Date)

# Check completed dataset
dim(daily_demand_complete)

# Verify missing values
colSums(is.na(daily_demand_complete))

# Display first 10 rows
head(daily_demand_complete, 10)

# Save completed dataset
write.csv(
  daily_demand_complete,
  "data/processed/daily_demand_complete.csv",
  row.names = FALSE
)

print("STEP 3 COMPLETED SUCCESSFULLY!")

library(ggplot2)

# Chart 1: Daily Demand Trend
daily_demand_chart <- ggplot(
  daily_demand_complete,
  aes(x = Date, y = Total_Demand)
) +
  geom_line(linewidth = 0.5) +
  labs(
    title = "Daily Demand Trend",
    subtitle = "Historical Retail Product Demand",
    x = "Date",
    y = "Total Demand (Units)"
  ) +
  theme_minimal()

print(daily_demand_chart)

# Save chart
ggsave(
  "reports/daily_demand_trend.png",
  plot = daily_demand_chart,
  width = 12,
  height = 6
)

# Chart 2: Daily Revenue Trend
daily_revenue_chart <- ggplot(
  daily_demand_complete,
  aes(x = Date, y = Total_Revenue)
) +
  geom_line(linewidth = 0.5) +
  labs(
    title = "Daily Revenue Trend",
    x = "Date",
    y = "Revenue"
  ) +
  theme_minimal()

print(daily_revenue_chart)

ggsave(
  "reports/daily_revenue_trend.png",
  plot = daily_revenue_chart,
  width = 12,
  height = 6
)

# Chart 3: Daily Orders Trend
daily_orders_chart <- ggplot(
  daily_demand_complete,
  aes(x = Date, y = Total_Orders)
) +
  geom_line(linewidth = 0.5) +
  labs(
    title = "Daily Orders Trend",
    x = "Date",
    y = "Number of Orders"
  ) +
  theme_minimal()

print(daily_orders_chart)

ggsave(
  "reports/daily_orders_trend.png",
  plot = daily_orders_chart,
  width = 12,
  height = 6
)

print("STEP 4 COMPLETED SUCCESSFULLY!")


# ==========================================
# PHASE 7: STEP 5
# MONTHLY DEMAND AND SEASONALITY
# ==========================================

library(tidyverse)
library(lubridate)
library(ggplot2)

# 1. Monthly demand analysis
monthly_demand <- daily_demand_complete %>%
  mutate(
    Year = year(Date),
    Month_Number = month(Date),
    Month = month(Date, label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Year, Month_Number, Month) %>%
  summarise(
    Monthly_Demand = sum(Total_Demand, na.rm = TRUE),
    Monthly_Revenue = sum(Total_Revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Year, Month_Number) %>%
  mutate(
    Month_Date = as.Date(
      paste(Year, Month_Number, "01", sep = "-")
    )
  )

# Display monthly demand
print(monthly_demand)

# 2. Monthly seasonality
monthly_seasonality <- monthly_demand %>%
  group_by(Month_Number, Month) %>%
  summarise(
    Average_Monthly_Demand = mean(Monthly_Demand),
    .groups = "drop"
  ) %>%
  arrange(Month_Number)

print(monthly_seasonality)

# 3. Monthly demand trend chart
monthly_demand_chart <- ggplot(
  monthly_demand,
  aes(
    x = Month_Date,
    y = Monthly_Demand,
    color = factor(Year),
    group = Year
  )
) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Monthly Demand Trend",
    x = "Month",
    y = "Monthly Demand (Units)",
    color = "Year"
  ) +
  theme_minimal()

print(monthly_demand_chart)

ggsave(
  "reports/monthly_demand_trend.png",
  monthly_demand_chart,
  width = 12,
  height = 6
)

# 4. Seasonality chart
seasonality_chart <- ggplot(
  monthly_seasonality,
  aes(x = Month, y = Average_Monthly_Demand)
) +
  geom_col() +
  labs(
    title = "Average Monthly Demand - Seasonality",
    x = "Month",
    y = "Average Monthly Demand"
  ) +
  theme_minimal()

print(seasonality_chart)

ggsave(
  "reports/monthly_demand_seasonality.png",
  seasonality_chart,
  width = 10,
  height = 6
)

# 5. Export results
write.csv(
  monthly_demand,
  "reports/monthly_demand_analysis.csv",
  row.names = FALSE
)

write.csv(
  monthly_seasonality,
  "reports/monthly_demand_seasonality.csv",
  row.names = FALSE
)

print("STEP 5 COMPLETED SUCCESSFULLY!")


# ==========================================
# PHASE 7: STEP 6
# TRAINING AND TESTING DATA SPLIT
# ==========================================

# Arrange data by date
daily_demand_complete <- daily_demand_complete %>%
  arrange(Date)

# Calculate 80% training records
train_size <- floor(
  nrow(daily_demand_complete) * 0.80
)

# Create training dataset
train_data <- daily_demand_complete %>%
  slice_head(n = train_size)

# Create testing dataset
test_data <- daily_demand_complete %>%
  slice((train_size + 1):n())

# Display record counts
cat("Total Records:", nrow(daily_demand_complete), "\n")
cat("Training Records:", nrow(train_data), "\n")
cat("Testing Records:", nrow(test_data), "\n")

# Display date ranges
cat("Training Start:",
    as.character(min(train_data$Date)), "\n")

cat("Training End:",
    as.character(max(train_data$Date)), "\n")

cat("Testing Start:",
    as.character(min(test_data$Date)), "\n")

cat("Testing End:",
    as.character(max(test_data$Date)), "\n")

# Export datasets
write.csv(
  train_data,
  "data/processed/demand_train.csv",
  row.names = FALSE
)

write.csv(
  test_data,
  "data/processed/demand_test.csv",
  row.names = FALSE
)

print("STEP 6 COMPLETED SUCCESSFULLY!")


# ==========================================
# PHASE 7: STEP 7
# MOVING AVERAGE FORECASTING
# ==========================================

# Store training demand
historical_demand <- train_data$Total_Demand

# Create prediction vector
moving_average_predictions <- numeric(nrow(test_data))

# Generate predictions
for (i in seq_len(nrow(test_data))) {
  
  # Previous 7 days demand
  previous_7_days <- tail(historical_demand, 7)
  
  # Calculate moving average
  moving_average_predictions[i] <- mean(previous_7_days)
  
  # Add actual demand for next prediction
  historical_demand <- c(
    historical_demand,
    test_data$Total_Demand[i]
  )
}

# Create results dataset
moving_average_results <- tibble(
  Date = test_data$Date,
  Actual_Demand = test_data$Total_Demand,
  Predicted_Demand = moving_average_predictions
)

# Display first 10 predictions
head(moving_average_results, 10)

# Create chart
moving_average_chart <- ggplot(
  moving_average_results,
  aes(x = Date)
) +
  geom_line(
    aes(y = Actual_Demand, color = "Actual Demand")
  ) +
  geom_line(
    aes(y = Predicted_Demand, color = "Moving Average Forecast")
  ) +
  labs(
    title = "Moving Average - Actual vs Predicted Demand",
    x = "Date",
    y = "Demand (Units)",
    color = "Legend"
  ) +
  theme_minimal()

print(moving_average_chart)

# Save chart
ggsave(
  "reports/moving_average_forecast.png",
  moving_average_chart,
  width = 12,
  height = 6
)

# Export predictions
write.csv(
  moving_average_results,
  "reports/moving_average_predictions.csv",
  row.names = FALSE
)

print("STEP 7 COMPLETED SUCCESSFULLY!")


# ==========================================
# PHASE 7: STEP 8
# EXPONENTIAL SMOOTHING
# ==========================================

library(forecast)

# Convert training demand into time series
train_ts <- ts(
  train_data$Total_Demand,
  frequency = 7
)

# Build Exponential Smoothing model
ets_model <- ets(train_ts)

# Forecast testing period
ets_forecast <- forecast(
  ets_model,
  h = nrow(test_data)
)

# Display model
print(ets_model)
print(ets_forecast)

# Create results dataset
ets_results <- tibble(
  Date = test_data$Date,
  Actual_Demand = test_data$Total_Demand,
  Predicted_Demand = as.numeric(ets_forecast$mean)
)

# Display first 10 predictions
print(head(ets_results, 10))

# Plot forecast
autoplot(ets_forecast) +
  labs(
    title = "Exponential Smoothing Forecast",
    x = "Time",
    y = "Demand (Units)"
  ) +
  theme_minimal()

# Save forecast chart
ggsave(
  "reports/exponential_smoothing_forecast.png",
  width = 12,
  height = 6
)

# Export predictions
write.csv(
  ets_results,
  "reports/exponential_smoothing_predictions.csv",
  row.names = FALSE
)

print("STEP 8 COMPLETED SUCCESSFULLY!")


# ==========================================
# PHASE 7: STEP 9
# ARIMA FORECASTING
# ==========================================

# Build ARIMA model
arima_model <- auto.arima(
  train_ts,
  seasonal = TRUE
)

# Display model
print(arima_model)

# Forecast testing period
arima_forecast <- forecast(
  arima_model,
  h = nrow(test_data)
)

# Create prediction dataset
arima_results <- tibble(
  Date = test_data$Date,
  Actual_Demand = test_data$Total_Demand,
  Predicted_Demand = as.numeric(arima_forecast$mean)
)

# Display first 10 predictions
print(head(arima_results, 10))

# Plot ARIMA forecast
autoplot(arima_forecast) +
  labs(
    title = "ARIMA Demand Forecast",
    x = "Time",
    y = "Demand (Units)"
  ) +
  theme_minimal()

# Save chart
ggsave(
  "reports/arima_forecast.png",
  width = 12,
  height = 6
)

# Export predictions
write.csv(
  arima_results,
  "reports/arima_predictions.csv",
  row.names = FALSE
)

print("STEP 9 COMPLETED SUCCESSFULLY!")

# ==========================================
# PHASE 7: STEP 10
# MODEL EVALUATION
# ==========================================

# Evaluation function
evaluate_model <- function(actual, predicted) {
  
  mae <- mean(abs(actual - predicted), na.rm = TRUE)
  
  rmse <- sqrt(
    mean((actual - predicted)^2, na.rm = TRUE)
  )
  
  # MAPE calculated only for nonzero actual values
  nonzero <- actual != 0
  
  mape <- mean(
    abs(
      (actual[nonzero] - predicted[nonzero]) /
        actual[nonzero]
    ),
    na.rm = TRUE
  ) * 100
  
  return(
    data.frame(
      MAE = mae,
      RMSE = rmse,
      MAPE = mape
    )
  )
}

# Evaluate Moving Average
ma_metrics <- evaluate_model(
  moving_average_results$Actual_Demand,
  moving_average_results$Predicted_Demand
)

# Evaluate Exponential Smoothing
ets_metrics <- evaluate_model(
  ets_results$Actual_Demand,
  ets_results$Predicted_Demand
)

# Evaluate ARIMA
arima_metrics <- evaluate_model(
  arima_results$Actual_Demand,
  arima_results$Predicted_Demand
)

# Combine model results
model_comparison <- bind_rows(
  Moving_Average = ma_metrics,
  Exponential_Smoothing = ets_metrics,
  ARIMA = arima_metrics,
  .id = "Model"
)

# Display comparison
print(model_comparison)

# Identify best model by RMSE
best_model <- model_comparison %>%
  arrange(RMSE) %>%
  slice(1)

print("Best Model Based on RMSE:")
print(best_model)

# Export evaluation results
write.csv(
  model_comparison,
  "reports/forecast_model_comparison.csv",
  row.names = FALSE
)

print("STEP 10 COMPLETED SUCCESSFULLY!")


# Fair Moving Average Forecast
historical_demand <- train_data$Total_Demand

moving_average_fixed <- numeric(nrow(test_data))

for (i in seq_len(nrow(test_data))) {
  
  previous_7_days <- tail(historical_demand, 7)
  
  moving_average_fixed[i] <- mean(previous_7_days)
  
  # Use prediction, not actual test demand
  historical_demand <- c(
    historical_demand,
    moving_average_fixed[i]
  )
}

# Evaluate all models on the same test period
ma_fixed_metrics <- evaluate_model(
  test_data$Total_Demand,
  moving_average_fixed
)

ets_fixed_metrics <- evaluate_model(
  test_data$Total_Demand,
  as.numeric(ets_forecast$mean)
)

arima_fixed_metrics <- evaluate_model(
  test_data$Total_Demand,
  as.numeric(arima_forecast$mean)
)

# Create fair comparison
model_comparison_fair <- bind_rows(
  Moving_Average = ma_fixed_metrics,
  Exponential_Smoothing = ets_fixed_metrics,
  ARIMA = arima_fixed_metrics,
  .id = "Model"
)

print(model_comparison_fair)

# Identify best model based on RMSE
best_model <- model_comparison_fair %>%
  arrange(RMSE) %>%
  slice(1)

print("Best Model Based on RMSE:")
print(best_model)

# Export fair comparison
write.csv(
  model_comparison_fair,
  "reports/fair_forecast_model_comparison.csv",
  row.names = FALSE
)



# ==========================================
# PHASE 7: STEP 11
# FINAL 7, 30 AND 90-DAY FORECASTS
# ==========================================

# Train the selected model using ALL historical data
full_demand_ts <- ts(
  daily_demand_complete$Total_Demand,
  frequency = 7
)

final_ets_model <- ets(full_demand_ts)

# Generate future forecasts
final_forecast_7 <- forecast(final_ets_model, h = 7)
final_forecast_30 <- forecast(final_ets_model, h = 30)
final_forecast_90 <- forecast(final_ets_model, h = 90)

# Create 7-day forecast table
forecast_7_df <- tibble(
  Date = seq(
    max(daily_demand_complete$Date) + 1,
    by = "day",
    length.out = 7
  ),
  Predicted_Demand = pmax(
    0,
    as.numeric(final_forecast_7$mean)
  )
)

# Create 30-day forecast table
forecast_30_df <- tibble(
  Date = seq(
    max(daily_demand_complete$Date) + 1,
    by = "day",
    length.out = 30
  ),
  Predicted_Demand = pmax(
    0,
    as.numeric(final_forecast_30$mean)
  )
)

# Create 90-day forecast table
forecast_90_df <- tibble(
  Date = seq(
    max(daily_demand_complete$Date) + 1,
    by = "day",
    length.out = 90
  ),
  Predicted_Demand = pmax(
    0,
    as.numeric(final_forecast_90$mean)
  )
)

# Display forecast results
print(head(forecast_7_df))
print(head(forecast_30_df))
print(head(forecast_90_df))

# Export forecast files
write.csv(
  forecast_7_df,
  "reports/demand_forecast_7_days.csv",
  row.names = FALSE
)

write.csv(
  forecast_30_df,
  "reports/demand_forecast_30_days.csv",
  row.names = FALSE
)

write.csv(
  forecast_90_df,
  "reports/demand_forecast_90_days.csv",
  row.names = FALSE
)

# Create 90-day forecast chart
forecast_90_plot <- autoplot(final_forecast_90) +
  labs(
    title = "90-Day Retail Demand Forecast",
    x = "Time",
    y = "Predicted Demand (Units)"
  ) +
  theme_minimal()

print(forecast_90_plot)

# Save chart
ggsave(
  "reports/demand_forecast_90_days.png",
  plot = forecast_90_plot,
  width = 12,
  height = 6
)

print("FINAL DEMAND FORECASTS CREATED SUCCESSFULLY!")


# ==========================================
# PHASE 7: STEP 12
# FINAL REPORTS AND EXPORTS
# ==========================================

# 1. Create forecasting summary

forecast_summary <- tibble(
  Metric = c(
    "Total Historical Demand",
    "Average Daily Demand",
    "Highest Daily Demand",
    "Lowest Daily Demand",
    "Training Days",
    "Testing Days",
    "Best Forecasting Model",
    "7-Day Forecast Total",
    "30-Day Forecast Total",
    "90-Day Forecast Total"
  ),
  
  Value = c(
    sum(daily_demand_complete$Total_Demand),
    mean(daily_demand_complete$Total_Demand),
    max(daily_demand_complete$Total_Demand),
    min(daily_demand_complete$Total_Demand),
    nrow(train_data),
    nrow(test_data),
    as.character(best_model$Model[1]),
    sum(forecast_7_df$Predicted_Demand),
    sum(forecast_30_df$Predicted_Demand),
    sum(forecast_90_df$Predicted_Demand)
  )
)

print(forecast_summary)

# 2. Export forecasting summary
write.csv(
  forecast_summary,
  "reports/demand_forecasting_summary.csv",
  row.names = FALSE
)

# 3. Export fair model comparison
write.csv(
  model_comparison_fair,
  "reports/fair_forecast_model_comparison.csv",
  row.names = FALSE
)

# 4. Export monthly demand analysis
write.csv(
  monthly_demand,
  "reports/monthly_demand_analysis.csv",
  row.names = FALSE
)

# 5. Create final text report
sink("reports/demand_forecasting_report.txt")

cat("RETAILPULSE KERALA\n")
cat("DEMAND FORECASTING REPORT\n")
cat("==============================\n\n")

cat("Historical Demand Summary\n")
print(forecast_summary)

cat("\nFair Model Comparison\n")
print(model_comparison_fair)

cat("\nSelected Model\n")
print(best_model)

cat("\n7-Day Forecast\n")
print(forecast_7_df)

cat("\n30-Day Forecast\n")
print(forecast_30_df)

cat("\n90-Day Forecast\n")
print(forecast_90_df)

sink()

# 6. Verify important files
files_to_check <- c(
  "reports/demand_forecasting_summary.csv",
  "reports/fair_forecast_model_comparison.csv",
  "reports/demand_forecasting_report.txt",
  "reports/demand_forecast_7_days.csv",
  "reports/demand_forecast_30_days.csv",
  "reports/demand_forecast_90_days.csv",
  "reports/demand_forecast_90_days.png"
)

file_check <- tibble(
  File = files_to_check,
  Exists = file.exists(files_to_check)
)

print(file_check)

print("PHASE 7 DEMAND FORECASTING COMPLETED!")

