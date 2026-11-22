
# Function 1: Create log messages
log_message <- function(message_text,
                        level = "INFO") {
  
  log_folder <- "logs"
  
  if (!dir.exists(log_folder)) {
    dir.create(log_folder)
  }
  
  log_file <- file.path(
    log_folder,
    "retailpulse_log.txt"
  )
  
  timestamp <- format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  )
  
  log_entry <- paste(
    timestamp,
    level,
    message_text,
    sep = " | "
  )
  
  cat(
    log_entry,
    "\n",
    file = log_file,
    append = TRUE
  )
  
  message(log_entry)
}


# Function 2: Safely execute operations
log_safe_execute <- function(expression) {
  
  tryCatch({
    
    result <- expression()
    
    log_message(
      "Operation completed successfully",
      "SUCCESS"
    )
    
    return(result)
    
  }, error = function(e) {
    
    log_message(
      paste("Operation failed:", e$message),
      "ERROR"
    )
    
    return(NULL)
    
  })
}


cat("ERROR HANDLING AND LOGGING FUNCTIONS CREATED!\n")
