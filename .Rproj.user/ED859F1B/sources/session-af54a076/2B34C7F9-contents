
# ==========================================
# RetailPulse Kerala
# Phase 3: Business Insights
# ==========================================

# Load packages
library(tidyverse)
library(lubridate)

# Load cleaned dataset
retail_clean <- readRDS("data/processed/retail_cleaned.rds")

# Check dataset
dim(retail_clean)

# Calculate important business metrics

total_revenue <- sum(retail_clean$Revenue, na.rm = TRUE)

total_orders <- n_distinct(retail_clean$Invoice)

total_customers <- n_distinct(
  retail_clean$`Customer ID`,
  na.rm = TRUE
)

total_products <- n_distinct(retail_clean$StockCode)

cat("Total Revenue:", total_revenue, "\n")
cat("Total Orders:", total_orders, "\n")
cat("Total Customers:", total_customers, "\n")
cat("Total Products:", total_products, "\n")


# Step 2: Average Order Value (AOV)

average_order_value <- total_revenue / total_orders

cat(
  "Average Order Value:",
  round(average_order_value, 2),
  "\n"
)


# Step 3: Average Selling Price (ASP)

total_quantity_sold <- sum(
  retail_clean$Quantity[retail_clean$Quantity > 0],
  na.rm = TRUE
)

total_sales_revenue <- sum(
  retail_clean$Revenue[retail_clean$Quantity > 0],
  na.rm = TRUE
)

average_selling_price <- total_sales_revenue / total_quantity_sold

cat(
  "Average Selling Price:",
  round(average_selling_price, 2),
  "\n"
)

# Step 4: Total Quantity Sold

cat(
  "Total Quantity Sold:",
  total_quantity_sold,
  "\n"
)

# Step 5: Total Returned Quantity

total_returns <- abs(sum(
  retail_clean$Quantity[retail_clean$Quantity < 0],
  na.rm = TRUE
))

cat(
  "Total Returned Quantity:",
  total_returns,
  "\n"
)

# Step 6: Return Rate

return_rate <- (total_returns / total_quantity_sold) * 100

cat(
  "Return Rate:",
  round(return_rate, 2),
  "%",
  "\n"
)
# Step 7: Monthly Revenue

monthly_revenue <- retail_clean %>%
  group_by(Month) %>%
  summarise(
    Total_Revenue = sum(Revenue, na.rm = TRUE)
  )

print(monthly_revenue)

# Step 8: Top 10 Best-Selling Products

top_products <- retail_clean %>%
  filter(Quantity > 0) %>%
  group_by(Description) %>%
  summarise(
    Total_Quantity = sum(Quantity, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Quantity)) %>%
  slice_head(n = 10)

print(top_products)

# Step 9: Top 10 Countries by Revenue

top_countries <- retail_clean %>%
  group_by(Country) %>%
  summarise(
    Total_Revenue = sum(Revenue, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Revenue)) %>%
  slice_head(n = 10)

print(top_countries)

# Step 10: Yearly Revenue

yearly_revenue <- retail_clean %>%
  group_by(Year) %>%
  summarise(
    Total_Revenue = sum(Revenue, na.rm = TRUE)
  ) %>%
  arrange(Year)

print(yearly_revenue)

# Step 11: Revenue by Weekday

weekday_revenue <- retail_clean %>%
  group_by(Weekday) %>%
  summarise(
    Total_Revenue = sum(Revenue, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Revenue))

print(weekday_revenue)

# Step 12: Top 10 Customers by Revenue

top_customers <- retail_clean %>%
  filter(!is.na(`Customer ID`)) %>%
  group_by(`Customer ID`) %>%
  summarise(
    Total_Revenue = sum(Revenue, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Revenue)) %>%
  slice_head(n = 10)

print(top_customers)

# Step 13: Revenue Contribution by Country

country_contribution <- retail_clean %>%
  group_by(Country) %>%
  summarise(
    Total_Revenue = sum(Revenue, na.rm = TRUE)
  ) %>%
  mutate(
    Revenue_Percentage =
      (Total_Revenue / sum(Total_Revenue)) * 100
  ) %>%
  arrange(desc(Total_Revenue)) %>%
  slice_head(n = 10)

print(country_contribution)

# Step 14: Monthly Returns Analysis

monthly_returns <- retail_clean %>%
  filter(Quantity < 0) %>%
  group_by(Month) %>%
  summarise(
    Total_Returns = abs(sum(Revenue, na.rm = TRUE))
  )

print(monthly_returns)



# Step 15: Business Insights Summary

business_summary <- tibble(
  KPI = c(
    "Total Revenue",
    "Total Orders",
    "Total Customers",
    "Total Products",
    "Average Order Value",
    "Average Selling Price",
    "Total Quantity Sold",
    "Total Returned Quantity",
    "Return Rate"
  ),
  
  Value = c(
    round(total_revenue, 2),
    total_orders,
    total_customers,
    total_products,
    round(average_order_value, 2),
    round(average_selling_price, 2),
    total_quantity_sold,
    total_returns,
    round(return_rate, 2)
  )
)

print(business_summary)


# Step 16: Export Business Summary

write.csv(
  business_summary,
  "data/processed/business_summary.csv",
  row.names = FALSE
)

cat("Business summary saved successfully!\n")


# Step 17: Export Detailed Business Insights

write.csv(
  monthly_revenue,
  "data/processed/monthly_revenue.csv",
  row.names = FALSE
)

write.csv(
  top_products,
  "data/processed/top_products.csv",
  row.names = FALSE
)

write.csv(
  top_countries,
  "data/processed/top_countries.csv",
  row.names = FALSE
)

write.csv(
  yearly_revenue,
  "data/processed/yearly_revenue.csv",
  row.names = FALSE
)

write.csv(
  weekday_revenue,
  "data/processed/weekday_revenue.csv",
  row.names = FALSE
)

write.csv(
  top_customers,
  "data/processed/top_customers.csv",
  row.names = FALSE
)

write.csv(
  country_contribution,
  "data/processed/country_contribution.csv",
  row.names = FALSE
)

write.csv(
  monthly_returns,
  "data/processed/monthly_returns.csv",
  row.names = FALSE
)

cat("All business insight files exported successfully!\n")


# Step 19: Create Business Insights Report

report_lines <- c(
  "RETAILPULSE-KERALA",
  "BUSINESS INSIGHTS REPORT",
  "------------------------------",
  "",
  paste("Total Revenue:", round(total_revenue, 2)),
  paste("Total Orders:", total_orders),
  paste("Total Customers:", total_customers),
  paste("Total Products:", total_products),
  paste("Average Order Value:", round(average_order_value, 2)),
  paste("Average Selling Price:", round(average_selling_price, 2)),
  paste("Total Quantity Sold:", total_quantity_sold),
  paste("Total Returned Quantity:", total_returns),
  paste("Return Rate:", round(return_rate, 2), "%"),
  "",
  "KEY BUSINESS FINDINGS",
  "------------------------------",
  paste(
    "Highest revenue month:",
    as.character(monthly_revenue$Month[
      which.max(monthly_revenue$Total_Revenue)
    ])
  ),
  paste(
    "Best-selling product:",
    top_products$Description[1]
  ),
  paste(
    "Top revenue-generating country:",
    top_countries$Country[1]
  ),
  paste(
    "Highest revenue weekday:",
    as.character(weekday_revenue$Weekday[1])
  )
)

writeLines(
  report_lines,
  "data/processed/business_insights_report.txt"
)

cat("Business insights report created successfully!\n")

# Step 20: Verify Business Insights Report

report_content <- readLines(
  "data/processed/business_insights_report.txt"
)

cat(report_content, sep = "\n")