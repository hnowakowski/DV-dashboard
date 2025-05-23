library(shiny)
library(shinydashboard)
library(leaflet)
library(spData)      
library(tidyr)
library(ggplot2)
library(dplyr)

pop_data <- read.csv("datasets/population.csv", sep = ",")
fer_data <- read.csv("datasets/fertility.csv", sep = ",")
gdp_data <- read.csv("datasets/gdp.csv", sep = ",")

world <- spData::world %>%
  filter(name_long != "Antarctica")


function(input, output, session) {


  selected_country <- reactiveVal(NULL)
  
  # --- POPULATION MAP ---
  output$pop_map <- renderLeaflet({
    leaflet(world) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      addPolygons(layerId = ~name_long,
                  fillColor = "lightblue", fillOpacity = 0.2,
                  color = "black", weight = 1,
                  label = ~name_long) %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90)
  })
  
  observeEvent(input$pop_map_shape_click, {
    selected_country(input$pop_map_shape_click$id)
  })
  
  output$pop_count <- renderValueBox({
    req(selected_country())
    
    p <- pop_data %>%
      filter(Country.Name == selected_country()) %>%
      slice(1) %>%
      pull(X2023)
    
    shinydashboard::valueBox(
      value = format(p, big.mark = ","),
      subtitle = paste("Population of", selected_country()),
      icon = icon("user"),
      width = 14
    )
    
  })
  
  
  output$fertility_rate <- renderUI({
    req(selected_country())

  })
  
  output$pop_growth <- renderPlot({
    req(selected_country())

    pop_hist <- pop_data %>%
      filter(Country.Name == selected_country()) %>%
      pivot_longer(
        cols = starts_with("X"),
        names_to = "Year",
        values_to = "Population"
      ) %>%
      mutate(Year = as.integer(sub("X", "", Year)))
    
    ggplot(pop_hist, aes(x = Year, y = Population)) +
      geom_line(color = "blue") +
      geom_point() +
      labs(title = paste("Population Growth for", selected_country()),
           x = "Year", y = "Population") +
      theme_minimal()
  })
  
  # --- GDP MAP ---
  output$gdp_map <- renderLeaflet({
    leaflet(world) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      addPolygons(layerId = ~name_long,
                  fillColor = "green", fillOpacity = 0.2,
                  color = "black", weight = 1,
                  label = ~name_long) %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90)
  })
  
  observeEvent(input$gdp_map_shape_click, {
    selected_country(input$gdp_map_shape_click$id)
  })
  
  output$gdp_value <- renderValueBox({
    req(selected_country())
    
    gdp <- gdp_data %>%
      filter(Country.Name == selected_country()) %>%
      slice(1) %>%
      pull(X2023)
    
    gdp <- round(gdp/1000000000,1)
    
    shinydashboard::valueBox(
      value = paste0(format(gdp, big.mark = ","), " billion"),
      subtitle = paste("GDP of", selected_country()),
      icon = icon("dollar-sign"),
      width = 14,
      color = "green"
    )
  })
  
  output$gdp_per_capita <- renderText({
    req(selected_country())

  })
  
  output$gdp_growth <- renderPlot({
    req(selected_country())
    
    gdp_hist <- gdp_data %>%
      filter(Country.Name == selected_country()) %>%
      pivot_longer(
        cols = starts_with("X"),
        names_to = "Year",
        values_to = "GDP"
      ) %>%
      mutate(
        Year = as.integer(sub("X", "", Year)),
        GDP = as.numeric(GDP) / 1e6
        
      )

    ggplot(gdp_hist, aes(x = Year, y = GDP)) +
      geom_line(color = "green") +
      geom_point() +
      labs(title = paste("GDP Growth for", selected_country()),
           x = "Year", y = "GDP") +
      theme_minimal()
  })
  
  # --- QOL MAP ---
  output$qol_map <- renderLeaflet({
    leaflet(world) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      addPolygons(layerId = ~name_long,
                  fillColor = "orange", fillOpacity = 0.2,
                  color = "black", weight = 1,
                  label = ~name_long) %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90)
  })
  
  observeEvent(input$qol_map_shape_click, {
    selected_country(input$qol_map_shape_click$id)
  })
  
  output$hdi_value <- renderText({
    req(selected_country())

  })
  
  output$happy_score <- renderText({
    req(selected_country())

  })
  
  output$hdi_growth <- renderPlot({
    req(selected_country())
    
  })
}
