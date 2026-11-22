
library(tidyverse)
library(readr)

# CSV file location
file_path <- "data/raw/online_retail_II.csv"

# Check file
if (!file.exists(file_path)) {
  stop("Dataset not found. Check file location.")
}

# Import CSV dataset
retail_data <- read_csv(
  file_path,
  show_col_types = FALSE
)

# Inspect dataset
print(dim(retail_data))
glimpse(retail_data)
summary(retail_data)

# Create processed folder
dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

# Save imported dataset
saveRDS(
  retail_data,
  "data/processed/retail_imported.rds"
)

print("Retail dataset imported successfully!")

# Import simulated datasets

stores <- read_csv("data/simulated/stores.csv", show_col_types = FALSE)
products <- read_csv("data/simulated/products.csv", show_col_types = FALSE)
suppliers <- read_csv("data/simulated/suppliers.csv", show_col_types = FALSE)
holidays <- read_csv("data/simulated/holidays.csv", show_col_types = FALSE)
promotions <- read_csv("data/simulated/promotions.csv", show_col_types = FALSE)
inventory <- read_csv("data/simulated/inventory.csv", show_col_types = FALSE)


dim(stores)
dim(products)
dim(suppliers)
dim(holidays)
dim(promotions)
dim(inventory)

print("All simulated datasets imported successfully!")

# View dataset structures

glimpse(stores)
glimpse(products)
glimpse(suppliers)
glimpse(holidays)
glimpse(promotions)
glimpse(inventory)

# Save simulated datasets

saveRDS(stores, "data/processed/stores.rds")
saveRDS(products, "data/processed/products.rds")
saveRDS(suppliers, "data/processed/suppliers.rds")
saveRDS(holidays, "data/processed/holidays.rds")
saveRDS(promotions, "data/processed/promotions.rds")
saveRDS(inventory, "data/processed/inventory.rds")

print("All datasets saved successfully!")
