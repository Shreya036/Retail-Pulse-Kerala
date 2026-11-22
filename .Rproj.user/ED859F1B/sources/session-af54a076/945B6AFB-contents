library(DBI)
library(RSQLite)
library(readr)

retail_data <- readRDS("data/processed/retail_imported.rds")
stores <- readRDS("data/processed/stores.rds")
products <- readRDS("data/processed/products.rds")
suppliers <- readRDS("data/processed/suppliers.rds")
holidays <- readRDS("data/processed/holidays.rds")
promotions <- readRDS("data/processed/promotions.rds")
inventory <- readRDS("data/processed/inventory.rds")


con <- dbConnect(
  SQLite(),
  "data/retailpulse.sqlite"
)

dbWriteTable(con, "retail_data", retail_data, overwrite = TRUE)
dbWriteTable(con, "stores", stores, overwrite = TRUE)
dbWriteTable(con, "products", products, overwrite = TRUE)
dbWriteTable(con, "suppliers", suppliers, overwrite = TRUE)
dbWriteTable(con, "holidays", holidays, overwrite = TRUE)
dbWriteTable(con, "promotions", promotions, overwrite = TRUE)
dbWriteTable(con, "inventory", inventory, overwrite = TRUE)


print(dbListTables(con))


dbDisconnect(con)

print("SQLite database created successfully!")
