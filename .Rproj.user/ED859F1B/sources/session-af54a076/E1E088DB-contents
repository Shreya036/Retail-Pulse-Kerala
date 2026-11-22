
# ==========================================
# PHASE 11 - STEP 9
# AUTOMATED TESTING
# ==========================================

library(testthat)

# Load existing project functions
source("../R/advanced_functions.R")
source("../R/data_validation.R")


# ==========================================
# TEST 1 - MISSING VALUES
# ==========================================

test_that("Missing values are detected", {
  
  test_data <- data.frame(
    Name = c("A", "B", NA),
    Age = c(20, NA, 25)
  )
  
  result <- check_missing_values(test_data)
  
  expect_true(is.data.frame(result))
  
})


# ==========================================
# TEST 2 - REMOVE DUPLICATES
# ==========================================

test_that("Duplicate rows are removed", {
  
  test_data <- data.frame(
    ID = c(1, 2, 2),
    Name = c("A", "B", "B")
  )
  
  result <- remove_duplicates(test_data)
  
  expect_equal(nrow(result), 2)
  
})


# ==========================================
# TEST 3 - REQUIRED COLUMNS
# ==========================================

test_that("Required columns are validated", {
  
  test_data <- data.frame(
    Revenue = c(100, 200),
    Invoice = c("A1", "A2")
  )
  
  result <- validate_required_columns(
    test_data,
    c("Revenue", "Invoice")
  )
  
  expect_true(result)
  
})


# ==========================================
# TEST 4 - MISSING REQUIRED COLUMN
# ==========================================

test_that("Missing required columns are detected", {
  
  test_data <- data.frame(
    Revenue = c(100, 200)
  )
  
  result <- validate_required_columns(
    test_data,
    c("Revenue", "Invoice")
  )
  
  expect_false(result)
  
})


print("PHASE 11 STEP 9 TESTS COMPLETED!")
