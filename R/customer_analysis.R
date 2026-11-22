library(tidyverse)
library(lubridate)

# Load cleaned retail dataset
retail_clean <- readRDS(
  "data/processed/retail_cleaned.rds"
)

# Check dataset dimensions
dim(retail_clean)

# Check column names
names(retail_clean)

# Inspect dataset structure
glimpse(retail_clean)

# Count unique customers
n_distinct(retail_clean$`Customer ID`, na.rm = TRUE)


# STEP 2: PREPARE CUSTOMER PURCHASE DATA

# Keep valid customer purchases
customer_sales <- retail_clean %>%
  filter(
    !is.na(`Customer ID`),
    Quantity > 0,
    Price > 0
  )

# Check dimensions
dim(customer_sales)

# Count unique customers
n_distinct(customer_sales$`Customer ID`)

# Check missing customer IDs
sum(is.na(customer_sales$`Customer ID`))

# Check negative quantities
sum(customer_sales$Quantity < 0)

# Check total positive sales revenue
sum(customer_sales$Revenue, na.rm = TRUE)

# STEP 3: CALCULATE RFM METRICS

# Set reference date
reference_date <- max(customer_sales$InvoiceDate) + days(1)

# Calculate RFM
rfm <- customer_sales %>%
  group_by(`Customer ID`) %>%
  summarise(
    Last_Purchase_Date = max(InvoiceDate),
    
    Recency = as.numeric(
      reference_date - max(InvoiceDate)
    ),
    
    Frequency = n_distinct(Invoice),
    
    Monetary = sum(Revenue, na.rm = TRUE),
    
    .groups = "drop"
  )

# View first 10 customers
head(rfm, 10)

# Check RFM dimensions
dim(rfm)

# Summary of RFM metrics
summary(rfm)



# STEP 4: CREATE RFM SCORES

rfm <- rfm %>%
  mutate(
    R_Score = ntile(-Recency, 5),
    F_Score = ntile(Frequency, 5),
    M_Score = ntile(Monetary, 5)
  )

# Calculate combined RFM score
rfm <- rfm %>%
  mutate(
    RFM_Score = R_Score + F_Score + M_Score
  )

# View first 10 customers
head(rfm, 10)

# Summary of RFM scores
summary(rfm[, c(
  "R_Score",
  "F_Score",
  "M_Score",
  "RFM_Score"
)])

# STEP 5: CUSTOMER SEGMENTATION

rfm <- rfm %>%
  mutate(
    Segment = case_when(
      
      R_Score >= 4 & F_Score >= 4 & M_Score >= 4 ~
        "Champions",
      
      F_Score >= 4 ~
        "Loyal Customers",
      
      R_Score >= 4 & F_Score >= 2 ~
        "Potential Loyalists",
      
      R_Score == 5 & F_Score == 1 ~
        "New Customers",
      
      R_Score <= 2 & F_Score >= 3 ~
        "At Risk",
      
      TRUE ~
        "Hibernating"
    )
  )

# View customer segments
head(rfm, 10)

# Count customers in each segment
segment_summary <- rfm %>%
  group_by(Segment) %>%
  summarise(
    Total_Customers = n(),
    Total_Revenue = sum(Monetary),
    Average_Spending = mean(Monetary),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Customers))

print(segment_summary)



# STEP 6: CUSTOMER SEGMENTATION CHART

library(ggplot2)

segment_chart <- ggplot(
  segment_summary,
  aes(
    x = reorder(Segment, Total_Customers),
    y = Total_Customers,
    fill = Segment
  )
) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = Total_Customers),
    hjust = -0.2,
    size = 4
  ) +
  coord_flip() +
  labs(
    title = "Customer Segmentation Analysis",
    subtitle = "Number of customers in each RFM segment",
    x = "Customer Segment",
    y = "Number of Customers"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(
      face = "bold",
      size = 16
    )
  )

print(segment_chart)


# STEP 7: TOP 10 HIGH-VALUE CUSTOMERS

high_value_customers <- rfm %>%
  arrange(desc(Monetary)) %>%
  select(
    `Customer ID`,
    Frequency,
    Monetary,
    Recency,
    RFM_Score,
    Segment
  ) %>%
  slice_head(n = 10)

# Display top 10 customers
print(high_value_customers)

# Create high-value customer chart
high_value_chart <- ggplot(
  high_value_customers,
  aes(
    x = reorder(
      as.factor(`Customer ID`),
      Monetary
    ),
    y = Monetary
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 High-Value Customers",
    x = "Customer ID",
    y = "Total Spending"
  ) +
  theme_minimal()

print(high_value_chart)

# STEP 8: IDENTIFY AT-RISK CUSTOMERS

at_risk_customers <- rfm %>%
  filter(Segment == "At Risk") %>%
  arrange(desc(Monetary)) %>%
  select(
    `Customer ID`,
    Recency,
    Frequency,
    Monetary,
    Segment
  )

# Display top 10 at-risk customers
head(at_risk_customers, 10)

# Count at-risk customers
nrow(at_risk_customers)

# Calculate total spending from at-risk customers
sum(at_risk_customers$Monetary)



# STEP 9: EXPORT CUSTOMER ANALYSIS


# Save complete RFM customer data
write.csv(
  rfm,
  "reports/rfm_customer_analysis.csv",
  row.names = FALSE
)

# Save customer segment summary
write.csv(
  segment_summary,
  "reports/customer_segment_summary.csv",
  row.names = FALSE
)

# Save top 10 high-value customers
write.csv(
  high_value_customers,
  "reports/high_value_customers.csv",
  row.names = FALSE
)

# Save at-risk customers
write.csv(
  at_risk_customers,
  "reports/at_risk_customers.csv",
  row.names = FALSE
)

print("Customer analysis files exported successfully!")



# STEP 10: REVENUE BY CUSTOMER SEGMENT


segment_revenue_chart <- ggplot(
  segment_summary,
  aes(
    x = reorder(Segment, Total_Revenue),
    y = Total_Revenue,
    fill = Segment
  )
) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = round(Total_Revenue)),
    hjust = -0.2,
    size = 3.5
  ) +
  coord_flip() +
  labs(
    title = "Revenue Contribution by Customer Segment",
    subtitle = "Total spending generated by each RFM segment",
    x = "Customer Segment",
    y = "Total Revenue"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 15)
  )

print(segment_revenue_chart)



# STEP 11: SAVE RFM DATASET


saveRDS(
  rfm,
  "data/processed/rfm_customer_data.rds"
)

print("RFM dataset saved successfully!")

# Verify the saved file
file.exists("data/processed/rfm_customer_data.rds")

# STEP 12: FINAL CUSTOMER ANALYSIS REPORT

# Create report file
sink("reports/customer_analysis_report.txt")

cat("RETAILPULSE-KERALA\n")
cat("CUSTOMER ANALYSIS REPORT\n")
cat("====================================\n\n")

cat("1. CUSTOMER OVERVIEW\n")
cat("------------------------------------\n")
cat("Total Eligible Customers:", nrow(rfm), "\n")
cat("Total Customer Segments:", n_distinct(rfm$Segment), "\n\n")

cat("2. RFM SEGMENT SUMMARY\n")
cat("------------------------------------\n")
print(segment_summary)

cat("\n3. HIGH-VALUE CUSTOMERS\n")
cat("------------------------------------\n")
print(high_value_customers)

cat("\n4. AT-RISK CUSTOMERS\n")
cat("------------------------------------\n")
cat("Total At-Risk Customers:", nrow(at_risk_customers), "\n")
cat("Total At-Risk Customer Spending:",
    round(sum(at_risk_customers$Monetary), 2), "\n")

cat("\n5. KEY BUSINESS INSIGHTS\n")
cat("------------------------------------\n")
cat("Champions are the highest revenue-generating segment.\n")
cat("Hibernating customers form the largest customer segment.\n")
cat("At-risk customers need targeted re-engagement campaigns.\n")
cat("High-value customers should receive loyalty benefits.\n")

cat("\n====================================\n")
cat("END OF REPORT\n")

sink()

print("Customer analysis report generated successfully!")

# Verify report
file.exists("reports/customer_analysis_report.txt")
