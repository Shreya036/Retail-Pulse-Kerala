
# ==========================================
# RETAILPULSE KERALA
# TARGETS PIPELINE
# ==========================================

library(targets)

# Set project options
tar_option_set(
  packages = c("dplyr", "readr", "lubridate")
)

# Define pipeline
list(
  
  # Target 1: Load cleaned retail dataset
  tar_target(
    retail_data,
    readRDS("data/processed/retail_cleaned.rds")
  ),
  
  # Target 2: Calculate business summary
  tar_target(
    business_summary,
    retail_data %>%
      summarise(
        Total_Revenue = sum(Revenue, na.rm = TRUE),
        Total_Orders = n_distinct(Invoice),
        Total_Customers = n_distinct(`Customer ID`),
        Total_Products = n_distinct(StockCode)
      )
  ),
  
  # Target 3: Export business summary
  tar_target(
    business_summary_report,
    {
      dir.create("reports", showWarnings = FALSE)
      write.csv(
        business_summary,
        "reports/targets_business_summary.csv",
        row.names = FALSE
      )
      "reports/targets_business_summary.csv"
    },
    format = "file"
  )
  
)
