
# ==========================================
# RETAILPULSE-KERALA
# PHASE 6: SMART INVENTORY ANALYTICS
# ==========================================

library(tidyverse)
library(lubridate)

# ==========================================
# STEP 1: LOAD DATASETS
# ==========================================

inventory <- read.csv(
  "data/simulated/inventory.csv",
  stringsAsFactors = FALSE
)

products <- read.csv(
  "data/simulated/products.csv",
  stringsAsFactors = FALSE
)

print("Datasets loaded successfully!")

# ==========================================
# STEP 2: INSPECT DATASETS
# ==========================================

cat("\nINVENTORY DATA\n")

print(dim(inventory))
print(names(inventory))
print(head(inventory, 10))
print(summary(inventory))

cat("\nPRODUCT DATA\n")

print(dim(products))
print(names(products))
print(head(products, 10))
print(summary(products))

# Check missing values
cat("\nMissing values in inventory:\n")
print(colSums(is.na(inventory)))

cat("\nMissing values in products:\n")
print(colSums(is.na(products)))

# Check duplicates
cat("\nDuplicate inventory rows:\n")
print(sum(duplicated(inventory)))

cat("\nDuplicate product rows:\n")
print(sum(duplicated(products)))

# ==========================================
# STEP 3: CLEAN INVENTORY DATA
# ==========================================

inventory_clean <- inventory %>%
  mutate(
    stock_quantity = as.numeric(stock_quantity),
    reorder_level = as.numeric(reorder_level)
  ) %>%
  distinct() %>%
  filter(
    !is.na(store_id),
    !is.na(product_id),
    !is.na(stock_quantity),
    !is.na(reorder_level),
    stock_quantity >= 0,
    reorder_level >= 0
  )

cat("\nCleaned inventory dimensions:\n")
print(dim(inventory_clean))

cat("\nInventory cleaning completed!\n")

# ==========================================
# STEP 4: MERGE INVENTORY AND PRODUCTS
# ==========================================

inventory_analysis <- inventory_clean %>%
  left_join(
    products,
    by = "product_id"
  )

cat("\nMerged inventory dataset:\n")

print(dim(inventory_analysis))
print(head(inventory_analysis, 10))
print(names(inventory_analysis))

cat("\nDataset merging completed!\n")

# ==========================================
# STEP 5: CALCULATE CURRENT STOCK LEVELS
# ==========================================

stock_summary <- inventory_analysis %>%
  group_by(
    product_id,
    product_name,
    category
  ) %>%
  summarise(
    Current_Stock = sum(
      stock_quantity,
      na.rm = TRUE
    ),
    Store_Count = n_distinct(store_id),
    .groups = "drop"
  ) %>%
  arrange(desc(Current_Stock))

cat("\nTOP 10 PRODUCTS BY CURRENT STOCK:\n")

print(head(stock_summary, 10))

# Total stock quantity
total_stock <- sum(
  stock_summary$Current_Stock,
  na.rm = TRUE
)

cat("\nTotal stock quantity:\n")
print(total_stock)

# Total unique products
cat("\nTotal products:\n")
print(nrow(stock_summary))

# ==========================================
# STEP 6: IDENTIFY LOW-STOCK PRODUCTS
# ==========================================

low_stock_products <- inventory_analysis %>%
  filter(
    stock_quantity <= reorder_level
  ) %>%
  arrange(stock_quantity) %>%
  select(
    store_id,
    product_id,
    product_name,
    category,
    stock_quantity,
    reorder_level,
    supplier_id
  )

cat("\nLOW-STOCK PRODUCTS:\n")

print(low_stock_products)

# Number of low-stock store-product records
cat("\nTotal low-stock records:\n")
print(nrow(low_stock_products))

# Number of unique low-stock products
cat("\nUnique low-stock products:\n")
print(n_distinct(low_stock_products$product_id))

# Total low-stock quantity
cat("\nTotal stock quantity of low-stock records:\n")

print(
  sum(
    low_stock_products$stock_quantity,
    na.rm = TRUE
  )
)

print("COMPLETED!")

# STEP 7: FAST-MOVING PRODUCTS


product_sales <- retail_clean %>%
  filter(Quantity > 0, Price > 0) %>%
  group_by(StockCode) %>%
  summarise(
    Product_Name = first(Description),
    Total_Quantity_Sold = sum(Quantity, na.rm = TRUE),
    Total_Revenue = sum(Revenue, na.rm = TRUE),
    Total_Orders = n_distinct(Invoice),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Quantity_Sold))

fast_moving_products <- product_sales %>%
  slice_head(n = 10)

print(fast_moving_products)

# Plot fast-moving products
fast_chart <- ggplot(
  fast_moving_products,
  aes(
    x = reorder(Product_Name, Total_Quantity_Sold),
    y = Total_Quantity_Sold
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Fast-Moving Products",
    x = "Product",
    y = "Quantity Sold"
  ) +
  theme_minimal()

print(fast_chart)


# ==========================================
# STEP 8: SLOW-MOVING PRODUCTS
# ==========================================

slow_moving_products <- product_sales %>%
  filter(Total_Quantity_Sold > 0) %>%
  arrange(Total_Quantity_Sold) %>%
  slice_head(n = 10)

print(slow_moving_products)

# Plot slow-moving products
slow_chart <- ggplot(
  slow_moving_products,
  aes(
    x = reorder(Product_Name, Total_Quantity_Sold),
    y = Total_Quantity_Sold
  )
) +
  geom_col(fill = "orange") +
  coord_flip() +
  labs(
    title = "Top 10 Slow-Moving Products",
    x = "Product",
    y = "Quantity Sold"
  ) +
  theme_minimal()

print(slow_chart)


# ==========================================
# STEP 9: REORDER ANALYSIS
# ==========================================

reorder_analysis <- inventory_analysis %>%
  mutate(
    Stock_Status = if_else(
      stock_quantity <= reorder_level,
      "Reorder Required",
      "Stock Available"
    )
  )

print(
  reorder_analysis %>%
    select(
      store_id,
      product_id,
      product_name,
      stock_quantity,
      reorder_level,
      Stock_Status
    ) %>%
    head(20)
)

# Count products requiring reorder
reorder_summary <- reorder_analysis %>%
  count(Stock_Status)

print(reorder_summary)


# ==========================================
# STEP 10: SAFETY STOCK CALCULATION
# ==========================================

safety_stock_analysis <- inventory_analysis %>%
  mutate(
    Safety_Stock = ceiling(
      reorder_level * 0.20
    ),
    Minimum_Stock_Level = reorder_level + Safety_Stock,
    Stock_Risk = case_when(
      stock_quantity <= reorder_level ~ "High Risk",
      stock_quantity <= Minimum_Stock_Level ~ "Medium Risk",
      TRUE ~ "Low Risk"
    )
  )

print(
  safety_stock_analysis %>%
    select(
      store_id,
      product_id,
      product_name,
      stock_quantity,
      reorder_level,
      Safety_Stock,
      Minimum_Stock_Level,
      Stock_Risk
    ) %>%
    head(20)
)

# Risk summary
risk_summary <- safety_stock_analysis %>%
  count(Stock_Risk)

print(risk_summary)


# ==========================================
# STEP 11: INVENTORY VISUALIZATION
# ==========================================

# Chart 1: Stock by category
category_stock <- inventory_analysis %>%
  group_by(category) %>%
  summarise(
    Total_Stock = sum(stock_quantity, na.rm = TRUE),
    .groups = "drop"
  )

category_chart <- ggplot(
  category_stock,
  aes(
    x = reorder(category, Total_Stock),
    y = Total_Stock,
    fill = category
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Current Stock by Product Category",
    x = "Category",
    y = "Total Stock"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(category_chart)


# Chart 2: Inventory risk
risk_chart <- ggplot(
  risk_summary,
  aes(
    x = Stock_Risk,
    y = n,
    fill = Stock_Risk
  )
) +
  geom_col() +
  geom_text(
    aes(label = n),
    vjust = -0.3
  ) +
  labs(
    title = "Inventory Risk Analysis",
    x = "Stock Risk",
    y = "Number of Records"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(risk_chart)


# Chart 3: Reorder status
reorder_chart <- ggplot(
  reorder_summary,
  aes(
    x = Stock_Status,
    y = n,
    fill = Stock_Status
  )
) +
  geom_col() +
  geom_text(
    aes(label = n),
    vjust = -0.3
  ) +
  labs(
    title = "Products Requiring Reordering",
    x = "Stock Status",
    y = "Number of Records"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(reorder_chart)




# STEP 12: EXPORT INVENTORY REPORTS


# Export stock summary
write.csv(
  stock_summary,
  "reports/inventory_stock_summary.csv",
  row.names = FALSE
)

# Export low-stock products
write.csv(
  low_stock_products,
  "reports/low_stock_products.csv",
  row.names = FALSE
)

# Export fast-moving products
write.csv(
  fast_moving_products,
  "reports/fast_moving_products.csv",
  row.names = FALSE
)

# Export slow-moving products
write.csv(
  slow_moving_products,
  "reports/slow_moving_products.csv",
  row.names = FALSE
)

# Export reorder analysis
write.csv(
  reorder_analysis,
  "reports/inventory_reorder_analysis.csv",
  row.names = FALSE
)

# Export safety stock analysis
write.csv(
  safety_stock_analysis,
  "reports/inventory_safety_stock_analysis.csv",
  row.names = FALSE
)

print("Inventory analysis reports exported successfully!")

# Verify files
file.exists("reports/inventory_stock_summary.csv")
file.exists("reports/low_stock_products.csv")
file.exists("reports/fast_moving_products.csv")
file.exists("reports/slow_moving_products.csv")
file.exists("reports/inventory_reorder_analysis.csv")
file.exists("reports/inventory_safety_stock_analysis.csv")
