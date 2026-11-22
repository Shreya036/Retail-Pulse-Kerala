
# PHASE 9: RECOMMENDATION ENGINE

# Load required packages
library(tidyverse)

# Load inventory dataset
inventory <- read.csv(
  "data/simulated/inventory.csv"
)

# Load products dataset
products <- read.csv(
  "data/simulated/products.csv"
)

# Load inventory analysis
inventory_analysis <- inventory %>%
  left_join(products, by = "product_id")

# Check inventory dataset
dim(inventory_analysis)

head(inventory_analysis)

# Check product dataset
dim(products)

head(products)

print(" STEP 1 COMPLETED SUCCESSFULLY!")


# STEP 2: Identify Products That Need Restocking

reorder_recommendations <- inventory_analysis %>%
  filter(stock_quantity <= reorder_level) %>%
  mutate(
    Recommendation = "Restock Immediately",
    Priority = "High"
  ) %>%
  select(
    store_id,
    product_id,
    product_name,
    category,
    stock_quantity,
    reorder_level,
    supplier_id,
    Recommendation,
    Priority
  ) %>%
  arrange(stock_quantity)

# Display recommendations
head(reorder_recommendations, 10)

# Count products requiring restocking
nrow(reorder_recommendations)

print("STEP 2 COMPLETED SUCCESSFULLY!")


# STEP 3: Identify Fast-Moving Products

# Load cleaned retail dataset
retail_clean <- readRDS(
  "data/processed/retail_cleaned.rds"
)

# Calculate product sales quantity
product_sales <- retail_clean %>%
  filter(
    Quantity > 0,
    Price > 0
  ) %>%
  group_by(StockCode) %>%
  summarise(
    Total_Quantity_Sold = sum(Quantity, na.rm = TRUE),
    Total_Revenue = sum(Revenue, na.rm = TRUE),
    .groups = "drop"
  )

# Identify top 10 fast-moving products
fast_moving_recommendations <- product_sales %>%
  arrange(desc(Total_Quantity_Sold)) %>%
  slice_head(n = 10) %>%
  mutate(
    Recommendation = "Maintain Higher Stock",
    Priority = "High"
  )

# Display recommendations
print(fast_moving_recommendations)

print("STEP 3 COMPLETED SUCCESSFULLY!")


# STEP 4: Identify Slow-Moving Products

slow_moving_recommendations <- product_sales %>%
  filter(Total_Quantity_Sold > 0) %>%
  arrange(Total_Quantity_Sold) %>%
  slice_head(n = 10) %>%
  mutate(
    Recommendation = "Review Stock and Promotions",
    Priority = "Medium"
  )

# Display recommendations
print(slow_moving_recommendations)

print("STEP 4 COMPLETED SUCCESSFULLY!")


# STEP 5: Identify Products with Low Inventory

low_inventory_recommendations <- inventory_analysis %>%
  mutate(
    Stock_Status = case_when(
      stock_quantity <= reorder_level ~ "Low Stock",
      stock_quantity <= reorder_level * 1.5 ~ "Warning",
      TRUE ~ "Sufficient Stock"
    ),
    Recommendation = case_when(
      Stock_Status == "Low Stock" ~ "Restock Immediately",
      Stock_Status == "Warning" ~ "Monitor Stock",
      TRUE ~ "No Action Required"
    )
  ) %>%
  filter(Stock_Status != "Sufficient Stock") %>%
  arrange(stock_quantity)

# Display low inventory products
print(head(low_inventory_recommendations, 10))

# Count low inventory products
nrow(low_inventory_recommendations)

print("STEP 5 COMPLETED SUCCESSFULLY!")

# STEP 6: Prioritize Replenishment

replenishment_recommendations <- inventory_analysis %>%
  mutate(
    Priority = case_when(
      stock_quantity <= reorder_level * 0.5 ~ "Critical",
      stock_quantity <= reorder_level ~ "High",
      stock_quantity <= reorder_level * 1.5 ~ "Medium",
      TRUE ~ "Low"
    ),
    Recommended_Action = case_when(
      Priority == "Critical" ~ "Urgent Replenishment",
      Priority == "High" ~ "Restock Immediately",
      Priority == "Medium" ~ "Monitor and Plan Reorder",
      TRUE ~ "Stock Sufficient"
    )
  ) %>%
  arrange(
    factor(
      Priority,
      levels = c("Critical", "High", "Medium", "Low")
    ),
    stock_quantity
  )

# Display replenishment recommendations
print(head(replenishment_recommendations, 15))

# Count products by priority
priority_summary <- replenishment_recommendations %>%
  count(Priority)

print(priority_summary)

print("STEP 6 COMPLETED SUCCESSFULLY!")


# STEP 7: Supplier-Based Recommendations

supplier_recommendations <- replenishment_recommendations %>%
  filter(Priority %in% c("Critical", "High")) %>%
  group_by(supplier_id) %>%
  summarise(
    Products_To_Restock = n(),
    Total_Stock_Available = sum(stock_quantity, na.rm = TRUE),
    Total_Reorder_Level = sum(reorder_level, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Products_To_Restock))

# Display supplier recommendations
print(supplier_recommendations)

print("STEP 7 COMPLETED SUCCESSFULLY!")


# STEP 8: Recommendation Summary

recommendation_summary <- replenishment_recommendations %>%
  group_by(Priority) %>%
  summarise(
    Total_Products = n(),
    Average_Stock = mean(stock_quantity, na.rm = TRUE),
    Average_Reorder_Level = mean(reorder_level, na.rm = TRUE),
    .groups = "drop"
  )

# Display recommendation summary
print(recommendation_summary)

# Create priority chart
recommendation_chart <- ggplot(
  recommendation_summary,
  aes(x = Priority, y = Total_Products, fill = Priority)
) +
  geom_col() +
  labs(
    title = "Inventory Replenishment Priority Summary",
    x = "Priority",
    y = "Number of Products"
  ) +
  theme_minimal()

print(recommendation_chart)

# Save chart
ggsave(
  "reports/recommendation_priority_summary.png",
  plot = recommendation_chart,
  width = 9,
  height = 6
)

print("STEP 8 COMPLETED SUCCESSFULLY!")


# STEP 9: Export Recommendation Results

# Save restocking recommendations
write.csv(
  reorder_recommendations,
  "reports/restock_recommendations.csv",
  row.names = FALSE
)

# Save fast-moving products
write.csv(
  fast_moving_recommendations,
  "reports/fast_moving_recommendations.csv",
  row.names = FALSE
)

# Save slow-moving products
write.csv(
  slow_moving_recommendations,
  "reports/slow_moving_recommendations.csv",
  row.names = FALSE
)

# Save low inventory recommendations
write.csv(
  low_inventory_recommendations,
  "reports/low_inventory_recommendations.csv",
  row.names = FALSE
)

# Save replenishment recommendations
write.csv(
  replenishment_recommendations,
  "reports/replenishment_recommendations.csv",
  row.names = FALSE
)

# Save supplier recommendations
write.csv(
  supplier_recommendations,
  "reports/supplier_recommendations.csv",
  row.names = FALSE
)

# Save recommendation summary
write.csv(
  recommendation_summary,
  "reports/recommendation_summary.csv",
  row.names = FALSE
)

print("STEP 9 COMPLETED SUCCESSFULLY!")


# STEP 10: Save Final Recommendation Report

# Store all recommendation results together
final_recommendation_results <- list(
  Restock = reorder_recommendations,
  Fast_Moving = fast_moving_recommendations,
  Slow_Moving = slow_moving_recommendations,
  Low_Inventory = low_inventory_recommendations,
  Replenishment = replenishment_recommendations,
  Supplier = supplier_recommendations,
  Summary = recommendation_summary
)

# Save complete recommendation results
saveRDS(
  final_recommendation_results,
  "reports/final_recommendation_results.rds"
)

# Create text report
report_lines <- c(
  "RETAILPULSE KERALA",
  "RECOMMENDATION ENGINE REPORT",
  "--------------------------------",
  paste("Restock recommendations:", nrow(reorder_recommendations)),
  paste("Fast-moving products:", nrow(fast_moving_recommendations)),
  paste("Slow-moving products:", nrow(slow_moving_recommendations)),
  paste("Low inventory products:", nrow(low_inventory_recommendations)),
  paste("Total replenishment records:", nrow(replenishment_recommendations)),
  paste("Suppliers requiring attention:", nrow(supplier_recommendations)),
  "",
  "Recommendation Priority Summary:",
  capture.output(print(recommendation_summary))
)

writeLines(
  report_lines,
  "reports/recommendation_engine_report.txt"
)

# Verify exported files
file.exists("reports/final_recommendation_results.rds")
file.exists("reports/recommendation_engine_report.txt")

print("STEP 10 COMPLETED SUCCESSFULLY!")
print("PHASE 9 RECOMMENDATION ENGINE COMPLETED!")
