library(shiny)
library(shinydashboard)
library(flexdashboard)
library(leaflet)
library(spData)      
library(tidyr)
library(ggsci)
library(ggplot2)
library(dplyr)
library(countrycode)
library(sf)

pop_data <- read.csv("datasets/population.csv", sep = ",")
fer_data <- read.csv("datasets/fertility.csv", sep = ",")
gdp_data <- read.csv("datasets/gdp.csv", sep = ",")
hap_data <- read.csv("datasets/happiness.csv", sep = ",")
hdi_data <- read.csv("datasets/hdi.csv", sep = ",")

pop_data$Country.Code <- countrycode(pop_data$Country.Code, origin = "iso3c", destination = "iso2c") 
fer_data$Country.Code <- countrycode(fer_data$Country.Code, origin = "iso3c", destination = "iso2c") 

shinyServer(function(input, output, session) {
  print("running")


  selected_country <- reactiveVal(NULL)
  
  # --- POPULATION MAP ---
  output$pop_map <- renderLeaflet({
    leaflet(spData::world) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      addPolygons(layerId = ~iso_a2,
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
      filter(Country.Code == selected_country()) %>%
      pull(X2023)
    
    
    if (!(length(p) == 0)) {
      if (!(is.na(p))){
        balue = format(p, big.mark = ",")
      } else {
        balue = "undefined"
      }
    } else {
      balue = "undefined"
    }
    
    country_name <- pop_data %>%
      filter(Country.Code == selected_country()) %>%
      pull(Country.Name)
    
    shinydashboard::valueBox(
      value = balue,
      subtitle = paste("Population of", country_name),
      icon = icon("user"),
      color = "orange",
      width = 14
    )
    
  })
  
  
  output$fertility_gauge <- renderGauge({
    req(selected_country())
    
    all_fertility <- fer_data %>%
      pull(X2023)
    
    max_fertility <- max(all_fertility, na.rm = TRUE)
    
    f <- fer_data %>%
      filter(Country.Code == selected_country()) %>%
      pull(X2023)
  
    
    gauge(f, min=0, max = max_fertility, symbol = '', gaugeSectors(
      success = c(2.1, max_fertility),
      warning = c(1.5, 2.1),
      danger = c(0, 1.5)
    ))

  })
  
  output$pop_growth <- renderPlot({
    req(selected_country())
    print("booper")

    pop_hist <- pop_data %>%
      filter(Country.Code == selected_country()) %>%
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
})

thing <- function(){
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
    
    if (!(length(gdp) == 0)) {
      if (!(is.na(gdp))){
        balue = paste0(format(gdp, big.mark = ","), " billion")
        bolor = "green"
      } else {
        balue = "undefined"
        bolor = "black"
      }
    } else {
      balue = "undefined"
      bolor = "black"
    }
    
    
    shinydashboard::valueBox(
      value = balue,
      subtitle = paste("GDP of", selected_country()),
      icon = icon("dollar-sign"),
      width = 14,
      color = bolor
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
  
  output$hdi_value <- renderValueBox({
    req(selected_country())
    
    hdi <- hdi_data %>%
      filter(country == selected_country()) %>%
      slice(1) %>%
      pull(HumanDevelopmentIndex_HDI_score_2023)
    
    hdi_c <- hdi_data %>%
      filter(country == selected_country()) %>%
      slice(1) %>%
      pull(HumanDevelopmentIndex_HDITierCurrent_txt_YearFree)
    
    if (!(length(hdi) == 0)) {
      if (hdi_c == "Very High") {
        balue <- format(hdi, big.mark = ",")
        bicon <- icon("laugh")
        bolor <- "green"
      } else if (hdi_c == "High") {
        balue <- format(hdi, big.mark = ",")
        bicon <- icon("smile")
        bolor <- "yellow"
      } else if (hdi_c == "Medium") {
        balue <- format(hdi, big.mark = ",")
        bicon <- icon("meh")
        bolor <- "orange"
      } else {
        balue <- format(hdi, big.mark = ",")
        bicon <- icon("frown")
        bolor <- "red"
      }
    } else {
      balue <- "Undefined"
      bicon <- icon("skull", style = "color:white")
      bolor <- "black"
    }
    
    shinydashboard::valueBox(
      value = balue,
      subtitle = paste("HDI of", selected_country()),
      icon = bicon,
      width = 14,
      color = bolor
      
    )

  })
  
  output$happy_score <- renderText({
    req(selected_country())

  })
  
  output$hdi_growth <- renderPlot({
    req(selected_country())
    
  })
}
