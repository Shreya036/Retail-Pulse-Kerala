
# RetailPulse Kerala - Exploratory Data Analysis

library(tidyverse)
library(lubridate)
library(ggplot2)

# Load cleaned dataset
retail_clean <- readRDS("data/processed/retail_cleaned.rds")

# Check dataset
dim(retail_clean)

# Check revenue summary
summary(retail_clean$Revenue)

print("EDA dataset loaded successfully!")


# Monthly Revenue Analysis

monthly_revenue <- retail_clean %>%
  group_by(Year, Month_Number) %>%
  summarise(
    Total_Revenue = sum(Revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Year, Month_Number)

# Create monthly revenue chart

ggplot(monthly_revenue,
       aes(x = Month_Number,
           y = Total_Revenue,
           color = factor(Year),
           group = Year)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Monthly Revenue Trend",
    x = "Month",
    y = "Total Revenue",
    color = "Year"
  ) +
  scale_x_continuous(breaks = 1:12) +
  theme_minimal()


# Top 10 Best-Selling Products

top_products <- retail_clean %>%
  filter(Quantity > 0) %>%
  group_by(Description) %>%
  summarise(
    Total_Quantity = sum(Quantity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Quantity)) %>%
  slice_head(n = 10)

# Display top 10 products
print(top_products)

# Create bar chart
ggplot(top_products,
       aes(x = reorder(Description, Total_Quantity),
           y = Total_Quantity,
           fill = Description)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Best-Selling Products",
    x = "Product",
    y = "Total Quantity Sold"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# Chart 3: Revenue by Country

country_revenue <- retail_clean %>%
  group_by(Country) %>%
  summarise(
    Total_Revenue = sum(Revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Revenue)) %>%
  slice_head(n = 10)

# Display results
print(country_revenue)

# Create chart
ggplot(country_revenue,
       aes(x = reorder(Country, Total_Revenue),
           y = Total_Revenue,
           fill = Country)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Countries by Revenue",
    x = "Country",
    y = "Total Revenue"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# Chart 4: Daily Sales Trend

daily_sales <- retail_clean %>%
  group_by(Date) %>%
  summarise(
    Daily_Revenue = sum(Revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Date)

# Display first 10 records
head(daily_sales, 10)

# Create daily sales chart
ggplot(daily_sales,
       aes(x = Date, y = Daily_Revenue)) +
  geom_line(color = "steelblue", linewidth = 0.7) +
  labs(
    title = "Daily Revenue Trend",
    x = "Date",
    y = "Daily Revenue"
  ) +
  theme_minimal()

# Chart 5: Yearly Revenue

yearly_revenue <- retail_clean %>%
  group_by(Year) %>%
  summarise(
    Total_Revenue = sum(Revenue, na.rm = TRUE),
    .groups = "drop"
  )

print(yearly_revenue)

ggplot(yearly_revenue,
       aes(x = factor(Year), y = Total_Revenue)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Yearly Revenue Analysis",
    x = "Year",
    y = "Total Revenue"
  ) +
  theme_minimal()

# Chart 6: Revenue by Weekday

weekday_revenue <- retail_clean %>%
  group_by(Weekday) %>%
  summarise(
    Total_Revenue = sum(Revenue, na.rm = TRUE),
    .groups = "drop"
  )

print(weekday_revenue)

ggplot(weekday_revenue,
       aes(x = Weekday, y = Total_Revenue)) +
  geom_col(fill = "coral") +
  labs(
    title = "Revenue by Weekday",
    x = "Weekday",
    y = "Total Revenue"
  ) +
  theme_minimal()

# Chart 7: Top 10 Customers by Revenue

top_customers <- retail_clean %>%
  filter(!is.na(`Customer ID`), Revenue > 0) %>%
  group_by(`Customer ID`) %>%
  summarise(
    Total_Revenue = sum(Revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Revenue)) %>%
  slice_head(n = 10)

print(top_customers)

ggplot(top_customers,
       aes(x = reorder(factor(`Customer ID`), Total_Revenue),
           y = Total_Revenue)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Customers by Revenue",
    x = "Customer ID",
    y = "Total Revenue"
  ) +
  theme_minimal()

# Chart 8: Revenue by Month

monthly_analysis <- retail_clean %>%
  group_by(Month_Number, Month) %>%
  summarise(
    Total_Revenue = sum(Revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Month_Number)

print(monthly_analysis)

ggplot(monthly_analysis,
       aes(x = Month, y = Total_Revenue)) +
  geom_col(fill = "darkcyan") +
  labs(
    title = "Revenue by Month",
    x = "Month",
    y = "Total Revenue"
  ) +
  theme_minimal()
# Chart 9: Top 10 Products by Revenue

top_products_revenue <- retail_clean %>%
  filter(Quantity > 0, Price > 0) %>%
  group_by(Description) %>%
  summarise(
    Total_Revenue = sum(Revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Revenue)) %>%
  slice_head(n = 10)

print(top_products_revenue)

ggplot(top_products_revenue,
       aes(x = reorder(Description, Total_Revenue),
           y = Total_Revenue)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Top 10 Products by Revenue",
    x = "Product",
    y = "Total Revenue"
  ) +
  theme_minimal()
# Chart 10: Top 5 Countries by Revenue

top_5_countries <- retail_clean %>%
  group_by(Country) %>%
  summarise(
    Total_Revenue = sum(Revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Revenue)) %>%
  slice_head(n = 5)

print(top_5_countries)

ggplot(top_5_countries,
       aes(x = reorder(Country, Total_Revenue),
           y = Total_Revenue)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(
    title = "Top 5 Countries by Revenue",
    x = "Country",
    y = "Total Revenue"
  ) +
  theme_minimal()

# Chart 11: Monthly Revenue by Year

year_month_revenue <- retail_clean %>%
  group_by(Year, Month_Number) %>%
  summarise(
    Total_Revenue = sum(Revenue, na.rm = TRUE),
    .groups = "drop"
  )

print(head(year_month_revenue, 15))

ggplot(year_month_revenue,
       aes(x = Month_Number,
           y = Total_Revenue,
           color = factor(Year),
           group = Year)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Monthly Revenue Comparison by Year",
    x = "Month Number",
    y = "Total Revenue",
    color = "Year"
  ) +
  scale_x_continuous(breaks = 1:12) +
  theme_minimal()

# Chart 12: Revenue Distribution - Final Version

ggplot(retail_clean %>% filter(Revenue > 0),
       aes(x = Revenue)) +
  geom_histogram(
    binwidth = 5,
    fill = "skyblue",
    color = "black"
  ) +
  coord_cartesian(xlim = c(0, 200)) +
  labs(
    title = "Revenue Distribution",
    subtitle = "Transactions with revenue between 0 and 200",
    x = "Revenue",
    y = "Number of Transactions"
  ) +
  theme_minimal()

# Chart 13: Revenue Contribution by Country

country_share <- retail_clean %>%
  group_by(Country) %>%
  summarise(
    Total_Revenue = sum(Revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Revenue)) %>%
  mutate(
    Country_Group = if_else(
      row_number() <= 5,
      Country,
      "Others"
    )
  ) %>%
  group_by(Country_Group) %>%
  summarise(
    Total_Revenue = sum(Total_Revenue),
    .groups = "drop"
  ) %>%
  mutate(
    Percentage = Total_Revenue / sum(Total_Revenue) * 100
  )

print(country_share)

ggplot(country_share,
       aes(x = "", y = Total_Revenue, fill = Country_Group)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Revenue Contribution by Country",
    fill = "Country"
  ) +
  theme_void()

# Chart 14: Monthly Returns Analysis

monthly_returns <- retail_clean %>%
  filter(Quantity < 0) %>%
  group_by(Month_Number, Month) %>%
  summarise(
    Return_Value = abs(sum(Revenue, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(Month_Number)

print(monthly_returns)

ggplot(monthly_returns,
       aes(x = Month, y = Return_Value)) +
  geom_col(fill = "tomato") +
  labs(
    title = "Monthly Returns Value",
    x = "Month",
    y = "Return Value"
  ) +
  theme_minimal()


# Chart 15: Revenue vs Quantity Sold

library(tidyverse)
library(ggplot2)

# Prepare data
revenue_quantity <- retail_clean %>%
  filter(
    Quantity > 0,
    Revenue > 0
  )

# Display first 10 records
head(revenue_quantity, 10)

# Take a random sample for faster plotting
set.seed(123)
sample_size <- min(5000, nrow(revenue_quantity))

revenue_sample <- revenue_quantity %>%
  slice_sample(n = sample_size)

# Create scatter plot
chart15 <- ggplot(
  revenue_sample,
  aes(x = Quantity, y = Revenue)
) +
  geom_point(
    color = "steelblue",
    alpha = 0.3,
    size = 1.5
  ) +
  labs(
    title = "Revenue vs Quantity Sold",
    subtitle = "Random sample of up to 5,000 transactions",
    x = "Quantity Sold",
    y = "Revenue"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(size = 10)
  )

# Display Chart 15
print(chart15)

# Save Chart 15
ggsave(
  filename = "Chart15_Revenue_vs_Quantity.png",
  plot = chart15,
  width = 10,
  height = 6,
  dpi = 300
)

# Completion message
cat("Chart 15 completed successfully!\n")
