# 1. CHECK MISSING VALUES

check_missing_values <- function(data) {
  
  missing_summary <- data.frame(
    Column = names(data),
    
    Missing_Count = sapply(data, function(x) {
      sum(is.na(x))
    }),
    
    Missing_Percentage = sapply(data, function(x) {
      round(mean(is.na(x)) * 100, 2)
    })
  )
  
  return(missing_summary)
}


# 2. REMOVE DUPLICATES

remove_duplicates <- function(data) {
  
  cleaned_data <- unique(data)
  
  return(cleaned_data)
}


# 3. CALCULATE BUSINESS SUMMARY

calculate_business_summary <- function(data) {
  
  required_columns <- c(
    "Revenue",
    "Invoice",
    "Customer ID",
    "StockCode"
  )
  
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
    
  }
  
  summary <- data.frame(
    
    Total_Revenue = sum(
      data$Revenue,
      na.rm = TRUE
    ),
    
    Total_Orders = length(
      unique(data$Invoice)
    ),
    
    Total_Customers = length(
      unique(na.omit(data[["Customer ID"]]))
    ),
    
    Total_Products = length(
      unique(data$StockCode)
    )
    
  )
  
  return(summary)
}


# 4. EXPORT REPORT

export_report <- function(data, file_path) {
  
  output_folder <- dirname(file_path)
  
  if (!dir.exists(output_folder)) {
    
    dir.create(
      output_folder,
      recursive = TRUE
    )
    
  }
  
  write.csv(
    data,
    file = file_path,
    row.names = FALSE
  )
  
  message("REPORT EXPORTED SUCCESSFULLY!")
  
}


# 5. VALIDATE COLUMNS

validate_columns <- function(data, required_columns) {
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) == 0) {
    
    message("ALL REQUIRED COLUMNS ARE AVAILABLE!")
    
    return(TRUE)
    
  } else {
    
    message(
      "Missing columns: ",
      paste(missing_columns, collapse = ", ")
    )
    
    return(FALSE)
    
  }
  
}


# ==========================================
# STEP 3 - ERROR HANDLING FUNCTIONS
# ==========================================


# 6. SAFE CSV IMPORT

safe_read_csv <- function(file_path) {
  
  tryCatch({
    
    if (!file.exists(file_path)) {
      
      stop("File does not exist!")
      
    }
    
    data <- read.csv(file_path)
    
    return(data)
    
  }, error = function(e) {
    
    message("ERROR: ", e$message)
    
    return(NULL)
    
  })
  
}


# 7. SAFE RDS IMPORT

safe_read_rds <- function(file_path) {
  
  tryCatch({
    
    if (!file.exists(file_path)) {
      
      stop("File does not exist!")
      
    }
    
    data <- readRDS(file_path)
    
    return(data)
    
  }, error = function(e) {
    
    message("ERROR: ", e$message)
    
    return(NULL)
    
  })
  
}


# ==========================================
# COMPLETION MESSAGE
# ==========================================

print("ADVANCED R FUNCTIONS CREATED SUCCESSFULLY!")
