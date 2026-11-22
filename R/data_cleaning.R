
# RetailPulse Kerala - Data Cleaning

library(tidyverse)
library(lubridate)
library(janitor)

# Load original imported dataset
retail_data <- readRDS("data/processed/retail_imported.rds")

# Check dataset dimensions
dim(retail_data)

# Display column names
colnames(retail_data)

# Check structure
glimpse(retail_data)

# Check missing values
missing_values <- colSums(is.na(retail_data))
print(missing_values)

# Check duplicate rows
duplicate_count <- sum(duplicated(retail_data))
print(duplicate_count)

# Summary statistics
summary(retail_data)


# Step 14: Create cleaned dataset

# Keep original data unchanged
retail_clean <- retail_data

# Remove exact duplicate rows
retail_clean <- retail_clean %>%
  distinct()

# Remove rows with missing product descriptions
retail_clean <- retail_clean %>%
  filter(!is.na(Description))

# Check cleaning results
dim(retail_clean)

# Check missing values again
colSums(is.na(retail_clean))

# Check duplicates again
sum(duplicated(retail_clean))

# Save cleaned dataset
saveRDS(
  retail_clean,
  "data/processed/retail_cleaned.rds"
)

print("Initial data cleaning completed!")


# Step 17: Check invalid quantities and prices

# Check negative quantities
sum(retail_clean$Quantity < 0, na.rm = TRUE)

# Check zero quantities
sum(retail_clean$Quantity == 0, na.rm = TRUE)

# Check negative prices
sum(retail_clean$Price < 0, na.rm = TRUE)

# Check zero prices
sum(retail_clean$Price == 0, na.rm = TRUE)

# Summary of quantity
summary(retail_clean$Quantity)

# Summary of price
summary(retail_clean$Price)


# Step 19: Remove negative price records

retail_clean <- retail_clean %>%
  filter(Price >= 0)

# Check negative prices again
sum(retail_clean$Price < 0, na.rm = TRUE)

# Check zero-price records
sum(retail_clean$Price == 0, na.rm = TRUE)

# Check negative quantities
sum(retail_clean$Quantity < 0, na.rm = TRUE)

# Display new dimensions
dim(retail_clean)

# Save updated cleaned dataset
saveRDS(
  retail_clean,
  "data/processed/retail_cleaned.rds"
)

print("Negative price records removed successfully!")


# Step 21: Convert InvoiceDate

# Check current data type
class(retail_clean$InvoiceDate)

# Convert to date-time format
retail_clean <- retail_clean %>%
  mutate(
    InvoiceDate = as.POSIXct(
      InvoiceDate,
      format = "%m/%d/%Y %H:%M",
      tz = "UTC"
    )
  )

# Check converted data type
class(retail_clean$InvoiceDate)

# Check date range
range(retail_clean$InvoiceDate, na.rm = TRUE)

# Check missing dates
sum(is.na(retail_clean$InvoiceDate))

# Extract date information

retail_clean <- retail_clean %>%
  mutate(
    Year = year(InvoiceDate),
    Month = month(InvoiceDate, label = TRUE),
    Month_Number = month(InvoiceDate),
    Day = day(InvoiceDate),
    Weekday = weekdays(InvoiceDate),
    Date = as.Date(InvoiceDate)
  )

# Check new columns
glimpse(retail_clean)

# Save final cleaned dataset
saveRDS(
  retail_clean,
  "data/processed/retail_cleaned.rds"
)

print("Date features created successfully!")


# Step 26: Create sales and revenue columns

retail_clean <- retail_clean %>%
  mutate(
    Revenue = Quantity * Price
  )

# Check revenue
summary(retail_clean$Revenue)

# Check total revenue
sum(retail_clean$Revenue, na.rm = TRUE)

# Check first 10 records
head(retail_clean, 10)

# Save updated dataset
saveRDS(
  retail_clean,
  "data/processed/retail_cleaned.rds"
)

print("Revenue column created successfully!")


# Step 28: Create data quality report

data_quality_report <- tibble(
  Metric = c(
    "Total Rows",
    "Total Columns",
    "Duplicate Rows",
    "Missing Descriptions",
    "Missing Customer IDs",
    "Negative Quantities",
    "Negative Prices",
    "Zero Prices",
    "Total Revenue"
  ),
  
  Value = c(
    nrow(retail_clean),
    ncol(retail_clean),
    sum(duplicated(retail_clean)),
    sum(is.na(retail_clean$Description)),
    sum(is.na(retail_clean$`Customer ID`)),
    sum(retail_clean$Quantity < 0),
    sum(retail_clean$Price < 0),
    sum(retail_clean$Price == 0),
    sum(retail_clean$Revenue, na.rm = TRUE)
  )
)

# Display report
print(data_quality_report)

# Save report
write_csv(
  data_quality_report,
  "data/processed/data_quality_report.csv"
)

print("Data quality report saved successfully!")

