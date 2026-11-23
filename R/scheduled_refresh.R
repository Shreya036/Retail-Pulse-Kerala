
# ==========================================
# RETAILPULSE KERALA
# SCHEDULED DATA REFRESH
# ==========================================

library(dplyr)

# Load cleaned dataset
retail_data <- readRDS(
  "data/processed/retail_cleaned.rds"
)

# Generate business summary
business_summary <- retail_data %>%
  summarise(
    Total_Revenue = sum(Revenue, na.rm = TRUE),
    Total_Orders = n_distinct(Invoice),
    Total_Customers = n_distinct(`Customer ID`),
    Total_Products = n_distinct(StockCode)
  )

# Create reports folder if missing
if (!dir.exists("reports")) {
  dir.create("reports")
}

# Save refreshed report
write.csv(
  business_summary,
  "reports/scheduled_business_summary.csv",
  row.names = FALSE
)

# Log refresh
if (!dir.exists("logs")) {
  dir.create("logs")
}

cat(
  paste(Sys.time(), "Data refresh completed successfully\n"),
  file = "logs/scheduled_refresh_log.txt",
  append = TRUE
)

print("SCHEDULED DATA REFRESH COMPLETED!")
