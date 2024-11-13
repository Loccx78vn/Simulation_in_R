pacman::p_load(rio,
               here,
               janitor,
               tidyverse,
               dplyr,
               magrittr,
               lubridate,
               stringr,
               shiny,
               leaflet,
               plotly,
               DT
)

library(shiny)
library(leaflet)
library(plotly)
library(DT)
library(dplyr)

# Sample data
circle_data <- data.frame(
  category = c("A", "B", "C"),
  value = c(10, 20, 30)
)

line_data <- data.frame(
  date = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "month"),
  category = rep(c("A", "B", "C"), length.out = 12),
  value = cumsum(runif(12, min = -10, max = 10))
)

pivot_data <- data.frame(
  category = rep(c("A", "B", "C"), each = 4),
  subcategory = rep(c("X", "Y"), times = 6),
  value = runif(12, min = 5, max = 15)
)

# Define UI
ui <- fluidPage(
  # Include custom CSS
  tags$head(
    tags$style(
      HTML(
        "
        body {
          background: linear-gradient(to right, #e2e2e2, #ffffff);
          font-family: Arial, sans-serif;
          color: #333;
        }
        .container-fluid {
          padding: 20px;
        }
        .sidebarPanel {
          background: #ffffff;
          border-radius: 10px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
          padding: 15px;
          margin-bottom: 20px;
        }
        .panel {
          background: #ffffff;
          border-radius: 10px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
          padding: 15px;
          margin-bottom: 20px;
        }
        .panel h3 {
          color: #007bff;
          margin-bottom: 15px;
          font-size: 20px;
        }
        .selectize-input {
          border: 1px solid #ced4da;
          border-radius: 5px;
        }
        .btn-primary {
          background-color: #007bff;
          border: none;
          border-radius: 5px;
        }
        .btn-primary:hover {
          background-color: #0056b3;
        }
        .leaflet-container {
          height: 400px;
          width: 100%;
        }
        .plotly-plot {
          height: 400px;
          width: 100%;
        }
        .dataTables_wrapper .dataTables_paginate .pagination {
          margin: 0;
        }
        "
      )
    )
  ),
  
  titlePanel("Interactive Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebarPanel",
      selectInput("categoryFilter", 
                  "Select Category:",
                  choices = c("All", unique(circle_data$category)),
                  selected = "All")
    ),
    
    mainPanel(
      fluidRow(
        column(12, class = "panel", leafletOutput("map"))
      ),
      fluidRow(
        column(6, class = "panel", plotlyOutput("circleChart")),
        column(6, class = "panel", plotlyOutput("lineChart"))
      ),
      fluidRow(
        column(12, class = "panel", DTOutput("pivotTable"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to filter data based on selected category
  filteredData <- reactive({
    selectedCategory <- input$categoryFilter
    list(
      lineData = line_data %>%
        filter(category == selectedCategory | selectedCategory == "All"),
      pivotData = pivot_data %>%
        filter(category == selectedCategory | selectedCategory == "All")
    )
  })
  
  # Render the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = -93.65, lat = 42.0285, popup = "Sample Location") %>%
      setView(lng = -93.65, lat = 42.0285, zoom = 4)
  })
  
  # Render the circle chart
  output$circleChart <- renderPlotly({
    plot_ly(circle_data, labels = ~category, values = ~value, type = 'pie',
            marker = list(colors = c('#1f77b4', '#ff7f0e', '#2ca02c'))) %>%
      layout(title = list(text = "Category Distribution", font = list(size = 20)),
             showlegend = TRUE,
             legend = list(orientation = "h", x = 0.5, xanchor = "center"))
  })
  
  # Render the line chart
  output$lineChart <- renderPlotly({
    data <- filteredData()$lineData
    plot_ly(data, x = ~date, y = ~value, color = ~category, type = 'scatter', mode = 'lines+markers',
            line = list(width = 2)) %>%
      layout(title = list(text = "Monthly Value Trends", font = list(size = 20)),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Value"),
             legend = list(orientation = "h", x = 0.5, xanchor = "center"))
  })
  
  # Render the pivot table
  output$pivotTable <- renderDT({
    data <- filteredData()$pivotData
    datatable(data, options = list(pageLength = 10, autoWidth = TRUE))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



library(shiny)
library(leaflet)
library(plotly)
library(DT)
library(dplyr)

# Sample Data
manufacturers <- data.frame(
  id = 1:3,
  name = c("Manuf A", "Manuf B", "Manuf C"),
  lat = c(34.0522, 40.7128, 37.7749),
  lon = c(-118.2437, -74.0060, -122.4194)
)

distribution_centers <- data.frame(
  id = 1:5,
  name = c("DC 1", "DC 2", "DC 3", "DC 4", "DC 5"),
  lat = c(34.0522, 40.7128, 37.7749, 36.1699, 39.0997),
  lon = c(-118.2437, -74.0060, -122.4194, -115.1398, -94.5786),
  manuf_id = c(1, 2, 1, 3, 2)  # Associating distribution centers with manufacturers
)

customers <- data.frame(
  id = 1:20,
  name = paste("Customer", 1:20),
  lat = runif(20, 30, 50),
  lon = runif(20, -120, -70),
  revenue = runif(20, 1000, 5000),
  manuf_id = sample(1:3, 20, replace = TRUE)  # Associating customers with manufacturers
)

daily_customers <- data.frame(
  date = seq.Date(from = as.Date("2024-01-01"), by = "day", length.out = 30),
  num_customers = sample(10:50, 30, replace = TRUE)
)

# UI
ui <- fluidPage(
  titlePanel("Interactive Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Manufacturer filter
      selectInput("manufacturer", "Select Manufacturer:",
                  choices = c("All", unique(manufacturers$name)),
                  selected = "All"),
      
      # Revenue filter
      sliderInput("revenueRange", "Select Revenue Range:",
                  min = min(customers$revenue),
                  max = max(customers$revenue),
                  value = c(min(customers$revenue), max(customers$revenue)),
                  step = 100
      )
    ),
    
    mainPanel(
      fluidRow(
        column(12, 
               leafletOutput("map", height = 500)  # Larger height for better map visibility
        )
      ),
      
      fluidRow(
        column(6, 
               DTOutput("pivotTable")  # Pivot table occupies half of the row width
        ),
        column(6, 
               plotlyOutput("circleChart", height = 400)  # Circle chart on the right with specified height
        )
      ),
      
      fluidRow(
        column(12, 
               plotlyOutput("lineChart", height = 400)  # Line chart occupies full width with specified height
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Filtered data based on inputs
  filtered_data <- reactive({
    filtered_customers <- customers %>%
      filter(revenue >= input$revenueRange[1], revenue <= input$revenueRange[2])
    
    if (input$manufacturer != "All") {
      manuf_id <- manufacturers %>% filter(name == input$manufacturer) %>% pull(id)
      filtered_customers <- filtered_customers %>% filter(manuf_id == manuf_id)
      filtered_distribution_centers <- distribution_centers %>% filter(manuf_id == manuf_id)
    } else {
      filtered_distribution_centers <- distribution_centers
    }
    
    list(customers = filtered_customers, distribution_centers = filtered_distribution_centers)
  })
  
  # Map Chart
  output$map <- renderLeaflet({
    data <- filtered_data()
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = manufacturers, ~lon, ~lat, popup = ~name, color = 'red', radius = 8, group = 'Manufacturers') %>%
      addCircleMarkers(data = data$distribution_centers, ~lon, ~lat, popup = ~name, color = 'blue', radius = 8, group = 'Distribution Centers') %>%
      addCircleMarkers(data = data$customers, ~lon, ~lat, popup = ~name, color = 'green', radius = 5, group = 'Customers') %>%
      addLayersControl(
        overlayGroups = c('Manufacturers', 'Distribution Centers', 'Customers'),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Pivot Table
  output$pivotTable <- renderDT({
    data <- filtered_data()
    datatable(data$customers, options = list(pageLength = 10))
  })
  
  # Circle Chart
  output$circleChart <- renderPlotly({
    data <- filtered_data()
    top_customers <- data$customers %>%
      arrange(desc(revenue)) %>%
      slice(1:5)
    
    other_customers_revenue <- sum(data$customers$revenue) - sum(top_customers$revenue)
    
    plot_ly(
      labels = c(top_customers$name, "Others"),
      values = c(top_customers$revenue, other_customers_revenue),
      type = "pie"
    ) %>%
      layout(title = "Revenue Distribution by Top 5 Customers and Others")
  })
  
  # Line Chart
  output$lineChart <- renderPlotly({
    plot_ly(daily_customers, x = ~date, y = ~num_customers, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Number of Customers per Day", xaxis = list(title = 'Date'), yaxis = list(title = 'Number of Customers'))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)
library(shinyWidgets)

ui <- page_navbar(
  title = "My App",
  theme = bs_theme(),
  inverse = TRUE,
  
  nav_panel(
    title = "One",
    p("First page content."),
    textOutput("panel_one")
  ),
  
  nav_panel(
    title = "Two",
    layout_sidebar(
      sidebar = sidebar(
        bg = "lightgrey",
        "Sidebar content goes here."
      ),
      main = textOutput("panel_two")
    )
  ),
  
  nav_panel(
    title = "Three",
    p("Third page content."),
    textOutput("panel_three")
  ),
  
  nav_spacer(),
  
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a("Posit", href = "https://posit.co")),
    nav_item(tags$a("Shiny", href = "https://shiny.posit.co"))
  )
)

server <- function(input, output, session) {
  
  output$panel_one <- renderText({
    "This is dynamic content for the first page."
  })
  
  output$panel_two <- renderText({
    "This is dynamic content for the second page."
  })
  
  output$panel_three <- renderText({
    "This is dynamic content for the third page."
  })
}

shinyApp(ui = ui, server = server)


