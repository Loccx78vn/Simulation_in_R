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
               reactable,
               leaflet.extras,
               leaflet,
               thematic,
               bslib,
               plotly,
               shinydashboard,
               forecast,
               shinythemes,
               ggplot2,
               sf,
               ggrepel,
               rnaturalearth,
               highcharter
)

# Data ----------------------------------------------------------------------
set.seed(123) 

## Enterprise data:
regions <- c("ASIA", "AFRICA", "EUROPE", "NORTHAMERICA", "SOUTHAMERICA")
num_entries <- 30  # Number of entries to generate

generate_code <- function(prefix, num) {
  sprintf("%s-%03d", prefix, num)
}

enterprise_data <- data.frame(
  region = sample(regions, num_entries, replace = TRUE),
  mf_code = sapply(ceiling(sample(1:20, num_entries, replace = TRUE)), function(x) generate_code("MF", x)),
  wh_code = sapply(ceiling(sample(1:20, num_entries, replace = TRUE)), function(x) generate_code("WH", x)),
  dc_code = sapply(ceiling(sample(1:20, num_entries, replace = TRUE)), function(x) generate_code("DC", x)),
  product_code = sapply(ceiling(sample(1:20, num_entries, replace = TRUE)), function(x) generate_code("PRO", x))
)

enterprise_data$SKU <- paste0(
  substr(enterprise_data$region, 1, 3), 
  "-", gsub("-", "", enterprise_data$mf_code),
  "-", gsub("-", "", enterprise_data$dc_code),
  "-", gsub("-", "", enterprise_data$wh_code),
  "-", gsub("-", "", enterprise_data$product_code)
)

## Locations data:

region_bounds <- data.frame(
  region = c("ASIA", "AFRICA", "EUROPE", "NORTHAMERICA", "SOUTHAMERICA"),
  lat_min = c(1, -35, 35, 24, -56),
  lat_max = c(81, 37, 72, 50, 12),
  long_min = c(60, -25, -30, -170, -75),
  long_max = c(180, 60, 50, -50, -35)
)

generate_coordinates <- function(region) {
  bounds <- region_bounds %>% filter(region == !!region)
  latitude <- runif(1, bounds$lat_min, bounds$lat_max)
  longitude <- runif(1, bounds$long_min, bounds$long_max)
  return(c(latitude, longitude))
}

location_data <- enterprise_data %>%
  pivot_longer(
    cols = c(mf_code, wh_code, dc_code),
    names_to = "facility_type",
    values_to = "facilities"
  ) %>%
  select(region, facilities)

coordinates <- t(sapply(location_data$region, generate_coordinates))
colnames(coordinates) <- c("latitude", "longitude")

location_data<- as.data.frame(cbind(location_data,coordinates))

## Sale data:
num_days <- 90
start_date <- Sys.Date() - num_days
date_seq <- seq.Date(from = start_date, by = "day", length.out = num_days)

sales_data <- data.frame()

for (SKU in unique(enterprise_data$SKU)) {
  
  daily_sales <- sample(1:100, num_days, replace = TRUE)
  selling_price <- round(runif(num_days, 10, 100), 2)
  net_income <- selling_price*20/100
  
  temp_data <- data.frame(
    date = date_seq,
    SKU = SKU,
    daily_sales = daily_sales,
    selling_price = selling_price,
    net_income = net_income
  )
  
  sales_data <- rbind(sales_data, temp_data)
}

## Procurement data:
set.seed(123)

suppliers <- c("Vinamilk", "Vingroup", "FPT", "Masan Group", "TH True Milk", "Hoa Phat Group", "BIDV", "MobiFone", "Sacombank", "PetroVietnam")

generate_random_product <- function() {
  product_names <- c("Grocery Item", "Beverage", "Dairy Product", "Snack", "Frozen Food", "Household Item", "Cosmetic", "Health Supplement", "Electronics", "Stationery")
  features <- c("Organic", "Gluten-Free", "Low Sugar", "High Protein", "Non-GMO", "Vegan", "Locally Sourced", "Eco-Friendly", "Premium Quality", "Imported")
  
  product_name <- sample(product_names, 1)
  feature <- sample(features, 1)
  
  return(list(product_name = product_name, feature = feature))
}

procurement_data <- data.frame()

for (SKU in unique(enterprise_data$SKU)) {
  supplier <- sample(suppliers, 1)
  random_product <- generate_random_product()
  
  expiry_date <- Sys.Date() + sample(30:180, 1)
  
  temp_data <- data.frame(
    SKU = SKU,
    supplier = supplier,
    product_name = random_product$product_name,
    feature = random_product$feature,
    expiry_date = expiry_date
  )
  
  procurement_data <- rbind(procurement_data, temp_data)
}

## Historical data:
historical_data <- data.frame(
  region = rep(c("EUROPE", "AFRICA", "SOUTHAMERICA", "NORTHAMERICA", "ASIA"), each = 6),
  month = rep(as.Date(paste(2024, 1:6, 1, sep = "-")), times = 5),
  total_sales = sample(800:15000, 30, replace = TRUE)
) 

historical_data <- rbind(historical_data, 
                         data.frame(region = "All", 
                                    month = rep(unique(historical_data$month), each = 1), 
                                    total_sales = tapply(historical_data$total_sales, historical_data$month, sum)))
convert_region <- function(abbrev) {
  full_names <- c("AFR" = "AFRICA", "ASI" = "ASIA", "EUR" = "EUROPE", 
                  "NOR" = "NORTHAMERICA", "SOU" = "SOUTHAMERICA", "All" = "All Regions")
  return(full_names[abbrev])
}

# Create summary_sales
summary_sales <- sales_data |> 
  mutate(region = substr(SKU, 1, 3)) |> 
  group_by(region, month = month(date)) |> 
  summarise(total_sales = sum(daily_sales), .groups = 'drop') |> 
  mutate(month = as.Date(paste(2024, month, 1, sep = "-")),
         region = convert_region(region))

summary_sales <- rbind(summary_sales, 
                       data.frame(region = "All", 
                                  month = rep(unique(summary_sales$month), each = 1), 
                                  total_sales = tapply(summary_sales$total_sales, summary_sales$month, sum)))


forecasts <- lapply(unique(historical_data$region), function(region) {
  region_data <- historical_data[historical_data$region == region, ]
  ts_data <- ts(region_data$total_sales, frequency = 1)  
  forecasted_values <- forecast(auto.arima(ts_data), h = 4)
  data.frame(
    region = region,
    month = as.Date(paste(2024, 7:10, 1, sep = "-")),
    Mean = forecasted_values$mean,
    High.95 = forecasted_values$upper[,2],
    Low.95 = forecasted_values$lower[,2]
  )
})

compare <- bind_rows(forecasts) |> 
  inner_join(summary_sales, by = c("region", "month")) |> 
  rename(Obsereved = total_sales)

## Data for cards: mutate(observed = summary_sales$total_sales,
card <- sales_data |> 
  summarise(
    revenue = sum(selling_price, na.rm = TRUE),
    total_sales = ceiling(sum(daily_sales, na.rm = TRUE)),
    net_income = sum(net_income, na.rm = TRUE)
  )

## Map:
map<- sales_data |>
  left_join(enterprise_data, by = "SKU") |>
  group_by(region,
           wh_code,
           month = month(date)) |>
  summarise(total_sales = sum(daily_sales)) |> 
  ungroup() |> 
  left_join(location_data,
            by = c("region", 
                   "wh_code" = "facilities")) |>
  select(c(longitude,
           latitude,
           total_sales,
           month,
           region,
           wh_code))
  

# Custom function-------------------------------------------------------------------
valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle, info = NULL, 
                          icon = NULL, color = "aqua", width = 4, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  info_icon <- tags$small(
    tags$i(
      class = "fa fa-info-circle fa-lg",
      title = info,
      `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.75);"
    ),
    # bs3 pull-right 
    # bs4 float-right
    class = "pull-right float-right"
  )
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      if (!is.null(sparkobj)) info_icon,
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      p(subtitle)
    ),
    # bs3 icon-large
    # bs4 icon
    if (!is.null(icon)) div(class = "icon-large icon", icon, style = "z-index; 0")
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}

# UI ----------------------------------------------------------------------
ui <- page_navbar(
  title = "Dashboard",
  inverse = TRUE,
  nav_panel(
    title = "Overview",
    page_fillable(
      padding = 0,
      layout_columns(
        width = 12,
        card(
          card_header("Location map",
                      class = "bg-dark"),
          full_screen = TRUE,
          leafletOutput("map"))
      ),
      layout_columns(
        layout_sidebar(
          border = FALSE,
          fillable = FALSE,
          sidebar = sidebar(
            bg = "lightgrey",
            "Select region and month",
            selectInput("region", "Select Region", choices = c("All", sort(unique(enterprise_data$region)))),
            selectInput("month", "Month", choices = unique(month(sales_data$date)))
          ),
          layout_columns(
            value_box(
              title = "Total revenue",
              subtitle = "test",
              value = scales::unit_format(unit = "$")(card[[1]]),
              showcase = bsicons::bs_icon("cash-coin"),
              theme = value_box_theme(bg = "#05c208",
                                      fg = "#ffffff"),
              max_height = "80px",
              fill = TRUE,
              showcase_layout = c("left center")
            ),
            ## Total sales box:
            value_box(
              title = "Total sales",
              value = scales::unit_format(unit = "tons", scale = 1)(card[[2]]*20/1000),
              showcase = bsicons::bs_icon("boxes"),
              theme = value_box_theme(fg = "#6f5d1f",
                                      bg = "#cfd8bc"),
              max_height = "80px",
              fill = TRUE,
              showcase_layout = c("left center")
            ),
            ## Net income box:
            value_box(
              title = "Net income",
              value = scales::unit_format(unit = "$")(card[[3]]),
              showcase = bsicons::bs_icon("wallet"),
              theme = "primary",
              max_height = "80px",
              fill = TRUE,
              showcase_layout = c("left center")
            )
            ),
            layout_columns(
              card(
                card_header("Predicted vs observed",
                            class = "bg-dark"),
                full_screen = TRUE,
                plotlyOutput("line")),
              card(
                card_header("Cost vs income",
                            class = "bg-dark"),
                full_screen = TRUE,
                plotlyOutput("barchart"))
            )
        )
            )
          )
        ),
    nav_panel(
      title = "Production",
      page_fillable(
        padding = 0,
        layout_sidebar(
          border = FALSE,
          fillable = FALSE,
          sidebar = sidebar(
            bg = "lightgrey",
            "Select region and month",
          selectInput("wh_code", 
                      "Select warehouse", 
                      choices = sort(unique(substr(sales_data$SKU, start = 11, stop = 15)))
                      )
          ),
        layout_columns(
          valueBoxOutput("vbox1")
        )
          )
        )
      ),
  
    nav_spacer(),
    
    nav_menu(
      title = "Links",
      align = "right",
      nav_item(tags$a("Posit", href = "https://posit.co")),
      nav_item(tags$a("Shiny", href = "https://shiny.posit.co"))
    )
  )

# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  ## Overview:
  
  line_data <- reactive({
    req(input$region)  
    
    filtered_data <- compare |>
      filter(region == input$region) 
  })
  
  output$line <-  renderPlotly({
    p<-plot_ly(line_data(),
               x = ~ month) |>
      add_trace(y = ~ Mean, name = 'Predicted', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4)) |>
      add_trace(y = ~ High.95, name = 'High 95%', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) |>
      add_trace(y = ~ Low.95, name = 'Low 95%', type = 'scatter', mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dot')) |>
      add_trace(y = ~ Obsereved, name = 'Observed', type = 'scatter', mode = 'lines', line = list(color = 'rgb(22, 96, 167)', width = 4)) |>
      layout(xaxis = list(title = "Months"),
             yaxis = list(title = "Total sales per month"),
             legend = list(orientation = 'h',                                                                         x = 0.5,          # Center the legend
                           xanchor = 'center', 
                           y = -0.2))
  })
  
  # Reactive expression for summary data
  summary_data <- reactive({
    req(input$region,input$month)  # Ensure the month input is available
    
    filtered_data <- sales_data |>
      left_join(enterprise_data, by = "SKU") |>
      group_by(region, month = month(date)) |>
      summarise(revenue = sum(daily_sales * selling_price),
                net_income = sum(net_income * daily_sales))|> 
      pivot_longer(
        cols = c(revenue, net_income),
        names_to = "metric",
        values_to = "value") |> 
      filter(month == input$month) 
  })
  
  # Render bar chart
  output$barchart <- renderPlotly({
    data <- summary_data()
    if (nrow(data) == 0) return(NULL)  
    
    plot_ly(data, 
            x = ~region, 
            y = ~value, 
            type = 'bar', 
            color = ~metric, 
            colors = c("#69c6ff", "#0470ba"), 
            text = ~value, 
            hoverinfo = "text") %>%
      layout(
        title = "Comparing net income and revenue",
        xaxis = list(title = "Month", showtitle = FALSE),
        yaxis = list(title = "Cost"),
        legend = list(orientation = 'h', 
                      x = 0.5, 
                      xanchor = 'center', 
                      y = -0.2)  
      )
  })
  
  # Render the map
  output$map <- renderLeaflet({
    if (input$region == "All") {
      filtered_data <- location_data  # Show all regions
    } else {
      filtered_data <- location_data |> filter(region == input$region)  # Filter by selected region
    }
    
    # Check if filtered data is empty
    if (nrow(filtered_data) == 0) {
      showNotification("No facilities found for the selected region.", type = "warning")
      return(NULL)
    }
    
    palPwr <- leaflet::colorFactor(palette = c("#1f78b4","#33a02c","#e31a1c","#ff7f00","#6a3d9a"), 
                                   domain = c("ASIA","AFRICA","EUROPE","NORTHAMERICA","SOUTHAMERICA"),
                                   ordered = F)
    
    labels <- paste0("<strong>Region: </strong>", filtered_data$region, "<br/>",
                     "<strong>Facility: </strong>", filtered_data$facilities, "<br/>") |> 
      lapply(htmltools::HTML)
    
    font <- labelOptions(noHide = TRUE, direction = "bottom", 
                         style = list(fontFamily = "serif",
                                      fontStyle = "italic",
                                      boxShadow = "3px 3px rgba(0,0,0,0.25)",
                                      fontSize = "10px",
                                      borderColor = "rgba(0,0,0,0.5)"))
    
    leaflet(filtered_data) |>
      addProviderTiles("CartoDB.Positron") |>
      addCircleMarkers(radius = 10, fillOpacity = 0.7, stroke = FALSE, 
                       label = ~labels, 
                       lng = ~longitude, 
                       lat = ~latitude, 
                       clusterOptions = markerClusterOptions(), 
                       color = ~palPwr(filtered_data$region),
                       labelOptions = font) |> 
      leaflet::addLegend(position = "bottomright",
                         values = ~region, 
                         opacity = .7,
                         pal = palPwr, 
                         title = "Region") |>    
      addResetMapButton()
  })
  
  ## Business result:

    vbox<-reactive({
      req(input$region) 
      
      filtered_data<-sales_data |> 
      mutate(wh_code =  substr(SKU, start = 11, stop = 15)) |>
      group_by(wh_code,date) |> 
      summarise(total_sales = sum(daily_sales)) |> 
      filter(wh_code == input$wh_code)
    })
    
    hc <- hchart(vbox(), 
                 "area", 
                 hcaes(date, total_sales), 
                 name = "Line chart of sales")  %>% 
      hc_size(height = 100) %>% 
      hc_credits(enabled = FALSE) %>% 
      hc_add_theme(hc_theme_sparkline_vb())
    
    output$vbox1 <-renderValueBox(
      valueBoxSpark(
        value = "1,345",
        title = toupper("Sales"),
        sparkobj = hc,
        subtitle = tagList(HTML("&uarr;"), "15% Since last quarter"),
        info = "Source: Rstudio",
        icon = icon("store"),
        width = 4,
        color = "teal",
        href = NULL
      )
    )
}

# Run the Shiny app
shinyApp(ui, server)