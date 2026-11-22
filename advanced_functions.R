# Function 1: Check missing values
check_missing_values <- function(data) {
  
  missing_values <- colSums(is.na(data))
  
  result <- data.frame(
    Column = names(missing_values),
    Missing_Count = as.integer(missing_values)
  )
  
  return(result)
}


# Function 2: Remove duplicate rows
remove_duplicates <- function(data) {
  
  cleaned_data <- data[!duplicated(data), ]
  
  return(cleaned_data)
}


# Function 3: Calculate business summary
calculate_business_summary <- function(data) {
  
  summary <- data.frame(
    Total_Revenue = sum(data$Revenue, na.rm = TRUE),
    Total_Orders = dplyr::n_distinct(data$Invoice),
    Total_Customers = dplyr::n_distinct(
      data$`Customer ID`, na.rm = TRUE
    ),
    Total_Products = dplyr::n_distinct(data$StockCode)
  )
  
  return(summary)
}


# Function 4: Export report
export_report <- function(data, file_path) {
  
  write.csv(
    data,
    file_path,
    row.names = FALSE
  )
  
  message("Report exported successfully: ", file_path)
}


# Function 5: Validate required columns
validate_columns <- function(data, required_columns) {
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    
    stop(
      paste(
        "Missing columns:",
        paste(missing_columns, collapse = ", ")
      )
    )
    
  } else {
    
    message("All required columns are available.")
    
  }
}


cat("ADVANCED R FUNCTIONS CREATED SUCCESSFULLY!\n")

