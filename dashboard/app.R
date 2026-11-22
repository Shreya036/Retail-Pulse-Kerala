
# ==========================================
# RETAILPULSE KERALA
# PROFESSIONAL SHINY DASHBOARD
# ==========================================

library(shiny)
library(tidyverse)
library(lubridate)

# ==========================================
# LOAD DATASETS
# ==========================================

retail_clean <- readRDS(
  "../data/processed/retail_cleaned.rds"
)

rfm <- readRDS(
  "../data/processed/rfm_customer_data.rds"
)

inventory <- read.csv(
  "../data/simulated/inventory.csv"
)

forecast_7 <- read.csv(
  "../reports/demand_forecast_7_days.csv"
)

forecast_30 <- read.csv(
  "../reports/demand_forecast_30_days.csv"
)

forecast_90 <- read.csv(
  "../reports/demand_forecast_90_days.csv"
)

# ==========================================
# LOAD RECOMMENDATION DATASETS
# ==========================================

restock_recommendations <- read.csv(
  "../reports/restock_recommendations.csv"
)

fast_moving_recommendations <- read.csv(
  "../reports/fast_moving_recommendations.csv"
)

slow_moving_recommendations <- read.csv(
  "../reports/slow_moving_recommendations.csv"
)

low_inventory_recommendations <- read.csv(
  "../reports/low_inventory_recommendations.csv"
)

supplier_recommendations <- read.csv(
  "../reports/supplier_recommendations.csv"
)

# ==========================================
# CONVERT FORECAST DATES
# ==========================================

forecast_7$Date <- as.Date(forecast_7$Date)
forecast_30$Date <- as.Date(forecast_30$Date)
forecast_90$Date <- as.Date(forecast_90$Date)

# ==========================================
# CALCULATE KPIs
# ==========================================

total_revenue <- sum(
  retail_clean$Revenue,
  na.rm = TRUE
)

total_orders <- n_distinct(
  retail_clean$Invoice
)

total_customers <- n_distinct(
  retail_clean$`Customer ID`,
  na.rm = TRUE
)

total_products <- n_distinct(
  retail_clean$StockCode
)

# ==========================================
# PROFESSIONAL USER INTERFACE
# ==========================================

ui <- fluidPage(
  
  # ========================================
  # CUSTOM CSS DESIGN
  # ========================================
  
  tags$head(
    
    tags$style(HTML("

      body {
        background-color: #f3f6fb;
        font-family: 'Segoe UI', sans-serif;
        color: #263445;
      }

      .container-fluid {
        padding: 20px 28px;
      }

      .dashboard-header {
        background: linear-gradient(
          120deg,
          #102a43,
          #176b87
        );
        color: white;
        padding: 25px;
        border-radius: 12px;
        margin-bottom: 25px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.12);
      }

      .dashboard-header h1 {
        font-weight: 700;
        margin-top: 0;
      }

      .dashboard-header p {
        color: #d8e9f2;
        margin-bottom: 0;
      }

      .sidebar-panel {
        background: white;
        padding: 20px;
        border-radius: 12px;
        box-shadow: 0 3px 12px rgba(0,0,0,0.07);
      }

      .main-panel {
        background: white;
        padding: 25px;
        border-radius: 12px;
        box-shadow: 0 3px 12px rgba(0,0,0,0.07);
      }

      .kpi-card {
        background: white;
        border-left: 5px solid #168aad;
        border-radius: 10px;
        padding: 20px;
        margin-bottom: 18px;
        min-height: 115px;
        box-shadow: 0 3px 12px rgba(0,0,0,0.07);
      }

      .kpi-card h4 {
        color: #627d98;
        font-size: 15px;
        font-weight: 600;
      }

      .kpi-card h2 {
        color: #102a43;
        font-weight: 700;
        margin-top: 8px;
      }

      .section-heading {
        color: #102a43;
        font-weight: 700;
        border-bottom: 2px solid #d9eaf2;
        padding-bottom: 10px;
        margin-top: 25px;
        margin-bottom: 20px;
      }

      .status-box {
        background: #e8f4f8;
        color: #176b87;
        border-radius: 8px;
        padding: 12px;
        font-weight: 600;
        margin-bottom: 20px;
      }

      .form-control {
        border-radius: 7px;
        border: 1px solid #d5e0ea;
      }

      .control-label {
        color: #334e68;
        font-weight: 600;
      }

      .well {
        background: #f7f9fc;
        border: 1px solid #e1e8f0;
        border-radius: 10px;
        box-shadow: none;
      }

      table {
        background: white;
        border-radius: 8px;
      }

      table th {
        background: #e8f1f7;
        color: #102a43;
      }

      .footer-text {
        color: #829ab1;
        text-align: center;
        margin-top: 30px;
        padding: 15px;
      }

      hr {
        border-top: 1px solid #e5edf4;
        margin-top: 25px;
        margin-bottom: 25px;
      }

    "))
    
  ),
  
  # ========================================
  # HEADER
  # ========================================
  
  div(
    class = "dashboard-header",
    
    h1("RetailPulse Kerala"),
    
    h4(
      "Smart Retail Analytics and Demand Forecasting System"
    ),
    
    p(
      "Sales | Customers | Inventory | Forecasting | Recommendations"
    )
    
  ),
  
  sidebarLayout(
    
    # ======================================
    # SIDEBAR
    # ======================================
    
    sidebarPanel(
      
      div(
        class = "sidebar-panel",
        
        h3("Dashboard Navigation"),
        
        selectInput(
          "analysis",
          "Select Analysis:",
          choices = c(
            "Sales Overview",
            "Customer Analytics",
            "Inventory Analytics",
            "Demand Forecasting",
            "Recommendation Engine"
          ),
          selected = "Sales Overview"
        ),
        
        hr(),
        
        h4("Project Information"),
        
        p("Project: RetailPulse Kerala"),
        
        p("Platform: R and Shiny"),
        
        p("Data Source: Online Retail II"),
        
        p("Analytics: Machine Learning and Forecasting")
        
      ),
      
      width = 3
      
    ),
    
    # ======================================
    # MAIN DASHBOARD
    # ======================================
    
    mainPanel(
      
      div(
        class = "main-panel",
        
        h2(
          "Business Performance Overview",
          class = "section-heading"
        ),
        
        # ==================================
        # KPI CARDS
        # ==================================
        
        fluidRow(
          
          column(
            6,
            
            div(
              class = "kpi-card",
              
              h4("Total Revenue"),
              
              h2(textOutput("revenue"))
              
            )
            
          ),
          
          column(
            6,
            
            div(
              class = "kpi-card",
              
              h4("Total Orders"),
              
              h2(textOutput("orders"))
              
            )
            
          )
          
        ),
        
        fluidRow(
          
          column(
            6,
            
            div(
              class = "kpi-card",
              
              h4("Total Customers"),
              
              h2(textOutput("customers"))
              
            )
            
          ),
          
          column(
            6,
            
            div(
              class = "kpi-card",
              
              h4("Total Products"),
              
              h2(textOutput("products"))
              
            )
            
          )
          
        ),
        
        # ==================================
        # STATUS
        # ==================================
        
        div(
          class = "status-box",
          
          textOutput("welcome_message")
          
        ),
        
        # ==================================
        # SALES OVERVIEW
        # ==================================
        
        conditionalPanel(
          
          condition = "input.analysis == 'Sales Overview'",
          
          h3(
            "Sales Overview",
            class = "section-heading"
          ),
          
          h4("Monthly Revenue Trend"),
          
          plotOutput("monthly_revenue_chart"),
          
          hr(),
          
          h4("Top 10 Products by Quantity"),
          
          plotOutput("top_products_chart")
          
        ),
        
        # ==================================
        # CUSTOMER ANALYTICS
        # ==================================
        
        conditionalPanel(
          
          condition = "input.analysis == 'Customer Analytics'",
          
          h3(
            "Customer Analytics",
            class = "section-heading"
          ),
          
          h4("Customer Segmentation Analysis"),
          
          plotOutput("customer_segment_chart")
          
        ),
        
        # ==================================
        # INVENTORY ANALYTICS
        # ==================================
        
        conditionalPanel(
          
          condition = "input.analysis == 'Inventory Analytics'",
          
          h3(
            "Inventory Analytics",
            class = "section-heading"
          ),
          
          h4("Inventory Stock Status"),
          
          plotOutput("inventory_chart")
          
        ),
        
        # ==================================
        # DEMAND FORECASTING
        # ==================================
        
        conditionalPanel(
          
          condition = "input.analysis == 'Demand Forecasting'",
          
          h3(
            "Demand Forecasting",
            class = "section-heading"
          ),
          
          h4("7-Day Demand Forecast"),
          
          plotOutput("forecast_7_chart"),
          
          hr(),
          
          h4("30-Day Demand Forecast"),
          
          plotOutput("forecast_30_chart"),
          
          hr(),
          
          h4("90-Day Demand Forecast"),
          
          plotOutput("forecast_90_chart")
          
        ),
        
        # ==================================
        # RECOMMENDATION ENGINE
        # ==================================
        
        conditionalPanel(
          
          condition = "input.analysis == 'Recommendation Engine'",
          
          h3(
            "Smart Recommendation Engine",
            class = "section-heading"
          ),
          
          h4("Restock Recommendations"),
          
          tableOutput("restock_table"),
          
          hr(),
          
          h4("Fast-Moving Products"),
          
          tableOutput("fast_moving_table"),
          
          hr(),
          
          h4("Slow-Moving Products"),
          
          tableOutput("slow_moving_table"),
          
          hr(),
          
          h4("Low-Inventory Products"),
          
          tableOutput("low_inventory_table"),
          
          hr(),
          
          h4("Supplier Recommendations"),
          
          tableOutput("supplier_table")
          
        )
        
      ),
      
      width = 9
      
    )
    
  ),
  
  # ========================================
  # FOOTER
  # ========================================
  
  div(
    class = "footer-text",
    
    "RetailPulse Kerala | Smart Retail Analytics Project"
    
  )
  
)

# ==========================================
# SERVER
# ==========================================

server <- function(input, output) {
  
  # ======================================
  # KPI OUTPUTS
  # ======================================
  
  output$revenue <- renderText({
    
    paste0(
      "₹ ",
      format(
        round(total_revenue, 2),
        big.mark = ","
      )
    )
    
  })
  
  output$orders <- renderText({
    
    format(
      total_orders,
      big.mark = ","
    )
    
  })
  
  output$customers <- renderText({
    
    format(
      total_customers,
      big.mark = ","
    )
    
  })
  
  output$products <- renderText({
    
    format(
      total_products,
      big.mark = ","
    )
    
  })
  
  # ======================================
  # DASHBOARD STATUS
  # ======================================
  
  output$welcome_message <- renderText({
    
    paste(
      "Currently viewing:",
      input$analysis
    )
    
  })
  
  # ======================================
  # MONTHLY REVENUE CHART
  # ======================================
  
  output$monthly_revenue_chart <- renderPlot({
    
    monthly_sales <- retail_clean %>%
      
      group_by(
        Year,
        Month_Number,
        Month
      ) %>%
      
      summarise(
        Monthly_Revenue = sum(
          Revenue,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      
      arrange(
        Year,
        Month_Number
      )
    
    ggplot(
      monthly_sales,
      aes(
        x = interaction(Year, Month, sep = "-"),
        y = Monthly_Revenue,
        group = 1
      )
    ) +
      
      geom_line(
        color = "#168aad",
        linewidth = 1.2
      ) +
      
      geom_point(
        color = "#f4a261",
        size = 2.5
      ) +
      
      labs(
        title = "Monthly Revenue Trend",
        x = "Month",
        y = "Revenue"
      ) +
      
      theme_minimal(base_size = 13) +
      
      theme(
        plot.title = element_text(
          face = "bold",
          color = "#102a43"
        ),
        
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
  })
  
  # ======================================
  # TOP PRODUCTS CHART
  # ======================================
  
  output$top_products_chart <- renderPlot({
    
    top_products <- retail_clean %>%
      
      filter(Quantity > 0) %>%
      
      group_by(Description) %>%
      
      summarise(
        Total_Quantity = sum(
          Quantity,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      
      slice_max(
        Total_Quantity,
        n = 10
      ) %>%
      
      arrange(Total_Quantity)
    
    ggplot(
      top_products,
      aes(
        x = reorder(Description, Total_Quantity),
        y = Total_Quantity
      )
    ) +
      
      geom_col(
        fill = "#168aad",
        width = 0.7
      ) +
      
      coord_flip() +
      
      labs(
        title = "Top 10 Best-Selling Products",
        x = "Product",
        y = "Quantity Sold"
      ) +
      
      theme_minimal(base_size = 13) +
      
      theme(
        plot.title = element_text(
          face = "bold",
          color = "#102a43"
        )
      )
    
  })
  
  # ======================================
  # CUSTOMER SEGMENTATION CHART
  # ======================================
  
  output$customer_segment_chart <- renderPlot({
    
    customer_segments <- rfm %>%
      
      group_by(Segment) %>%
      
      summarise(
        Total_Customers = n(),
        .groups = "drop"
      )
    
    ggplot(
      customer_segments,
      aes(
        x = reorder(Segment, Total_Customers),
        y = Total_Customers,
        fill = Segment
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        title = "Customer Segmentation",
        x = "Customer Segment",
        y = "Number of Customers"
      ) +
      
      theme_minimal(base_size = 13) +
      
      theme(
        plot.title = element_text(
          face = "bold",
          color = "#102a43"
        ),
        
        legend.position = "none"
      )
    
  })
  
  # ======================================
  # INVENTORY STOCK STATUS CHART
  # ======================================
  
  output$inventory_chart <- renderPlot({
    
    inventory_status <- inventory %>%
      
      mutate(
        Stock_Status = if_else(
          stock_quantity <= reorder_level,
          "Low Stock",
          "Sufficient Stock"
        )
      ) %>%
      
      count(Stock_Status)
    
    ggplot(
      inventory_status,
      aes(
        x = Stock_Status,
        y = n,
        fill = Stock_Status
      )
    ) +
      
      geom_col(width = 0.6) +
      
      labs(
        title = "Inventory Stock Status",
        x = "Stock Status",
        y = "Number of Products"
      ) +
      
      theme_minimal(base_size = 13) +
      
      theme(
        plot.title = element_text(
          face = "bold",
          color = "#102a43"
        ),
        
        legend.position = "none"
      )
    
  })
  
  # ======================================
  # 7-DAY FORECAST
  # ======================================
  
  output$forecast_7_chart <- renderPlot({
    
    ggplot(
      forecast_7,
      aes(
        x = Date,
        y = Predicted_Demand
      )
    ) +
      
      geom_line(
        color = "#168aad",
        linewidth = 1.2
      ) +
      
      geom_point(
        color = "#f4a261",
        size = 2
      ) +
      
      labs(
        title = "7-Day Demand Forecast",
        x = "Forecast Date",
        y = "Predicted Demand"
      ) +
      
      theme_minimal(base_size = 13)
    
  })
  
  # ======================================
  # 30-DAY FORECAST
  # ======================================
  
  output$forecast_30_chart <- renderPlot({
    
    ggplot(
      forecast_30,
      aes(
        x = Date,
        y = Predicted_Demand
      )
    ) +
      
      geom_line(
        color = "#2a9d8f",
        linewidth = 1.2
      ) +
      
      labs(
        title = "30-Day Demand Forecast",
        x = "Forecast Date",
        y = "Predicted Demand"
      ) +
      
      theme_minimal(base_size = 13)
    
  })
  
  # ======================================
  # 90-DAY FORECAST
  # ======================================
  
  output$forecast_90_chart <- renderPlot({
    
    ggplot(
      forecast_90,
      aes(
        x = Date,
        y = Predicted_Demand
      )
    ) +
      
      geom_line(
        color = "#8e6cba",
        linewidth = 1.2
      ) +
      
      labs(
        title = "90-Day Demand Forecast",
        x = "Forecast Date",
        y = "Predicted Demand"
      ) +
      
      theme_minimal(base_size = 13)
    
  })
  
  # ======================================
  # RECOMMENDATION TABLES
  # ======================================
  
  output$restock_table <- renderTable({
    
    head(restock_recommendations, 10)
    
  })
  
  output$fast_moving_table <- renderTable({
    
    head(fast_moving_recommendations, 10)
    
  })
  
  output$slow_moving_table <- renderTable({
    
    head(slow_moving_recommendations, 10)
    
  })
  
  output$low_inventory_table <- renderTable({
    
    head(low_inventory_recommendations, 10)
    
  })
  
  output$supplier_table <- renderTable({
    
    head(supplier_recommendations, 10)
    
  })
  
}

# ==========================================
# RUN DASHBOARD
# ==========================================

shinyApp(
  ui = ui,
  server = server
)
