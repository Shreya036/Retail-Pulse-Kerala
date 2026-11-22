
# ==========================================
# RETAILPULSE KERALA
# PHASE 11 - STEP 2
# DATA VALIDATION FUNCTIONS
# ==========================================

# Function 1: Check missing values
validate_missing_values <- function(data) {
  
  missing_count <- sum(is.na(data))
  
  if (missing_count == 0) {
    message("SUCCESS: No missing values found.")
  } else {
    message("WARNING: Missing values found: ", missing_count)
  }
  
  return(missing_count)
}


# Function 2: Check duplicate records
validate_duplicates <- function(data) {
  
  duplicate_count <- sum(duplicated(data))
  
  if (duplicate_count == 0) {
    message("SUCCESS: No duplicate records found.")
  } else {
    message("WARNING: Duplicate records found: ", duplicate_count)
  }
  
  return(duplicate_count)
}


# Function 3: Check required columns
validate_required_columns <- function(data, required_columns) {
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) == 0) {
    
    message("SUCCESS: All required columns are available.")
    
    return(TRUE)
    
  } else {
    
    message(
      "WARNING: Missing columns: ",
      paste(missing_columns, collapse = ", ")
    )
    
    return(FALSE)
  }
}


# Function 4: Check dataset dimensions
validate_dataset_size <- function(data) {
  
  result <- list(
    Rows = nrow(data),
    Columns = ncol(data)
  )
  
  print(result)
  
  return(result)
}


cat("DATA VALIDATION FUNCTIONS CREATED SUCCESSFULLY!\n")


# ==========================================
# RETAILPULSE KERALA
# PHASE 11 - STEP 3
# ERROR HANDLING
# ==========================================

# Function 1: Safely load CSV files
safe_read_csv <- function(file_path) {
  
  tryCatch({
    
    data <- read.csv(file_path)
    
    message("SUCCESS: File loaded - ", file_path)
    
    return(data)
    
  }, error = function(e) {
    
    message("ERROR: Could not load file - ", file_path)
    message("Reason: ", e$message)
    
    return(NULL)
    
  })
}


# Function 2: Safely load RDS files
safe_read_rds <- function(file_path) {
  
  tryCatch({
    
    data <- readRDS(file_path)
    
    message("SUCCESS: RDS file loaded - ", file_path)
    
    return(data)
    
  }, error = function(e) {
    
    message("ERROR: Could not load RDS file - ", file_path)
    message("Reason: ", e$message)
    
    return(NULL)
    
  })
}


# Function 3: Safely execute code
safe_execute <- function(expression) {
  
  tryCatch({
    
    result <- expression()
    
    message("SUCCESS: Operation completed.")
    
    return(result)
    
  }, error = function(e) {
    
    message("ERROR: ", e$message)
    
    return(NULL)
    
  })
}


cat("ERROR HANDLING FUNCTIONS CREATED SUCCESSFULLY!\n")
