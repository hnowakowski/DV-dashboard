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
library(plotly)
library(DT)
#library(rsconnect)

pop_data <- read.csv("datasets/population.csv", sep = ",")
fer_data <- read.csv("datasets/fertility.csv", sep = ",")
gdp_data <- read.csv("datasets/gdp.csv", sep = ",")
gdp_capita_data <- read.csv("datasets/gdp_per_capita.csv", sep=",")
hap_data <- read.csv("datasets/happiness.csv", sep = ",")
hdi_data <- read.csv("datasets/hdi.csv", sep = ",")

pop_data$Country.Code <- countrycode(pop_data$Country.Code, origin = "iso3c", destination = "iso2c") 
fer_data$Country.Code <- countrycode(fer_data$Country.Code, origin = "iso3c", destination = "iso2c") 
gdp_data$Country.Code <- countrycode(gdp_data$Country.Code, origin = "iso3c", destination = "iso2c") 
gdp_capita_data$Country.Code <- countrycode(gdp_capita_data$Country.Code, origin = "iso3c", destination = "iso2c") 




error_plot <- function() {
    error_message <- "No Info!"
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = error_message, size = 12, color = "black", hjust = 0.5, vjust = 0.5) +
      theme_void()
}

error_value <- function(){
  shinydashboard::valueBox(
    value = "No information",
    subtitle = "information of this country is not available",
    icon = icon("skull", style = "color: white"),
    width = 14,
    color = "black"
  )
}

error_gauge <- function() {
  gauge(0, min = 0, max = 100, label = "No Info", sectors = gaugeSectors(success = c(0, 0)))
}



shinyServer(function(input, output, session) {
  print("running")
  #rsconnect::writeManifest()

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

    
    if ((length(p) == 0)) {
      
      error_value()
      
    } else if ((is.na(p))){
      error_value()
      
    } else {
      
      balue = format(p, big.mark = ",")
      
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
    
  }
    
    
    
  })
  
  
  output$fertility_gauge <- renderGauge({
    req(selected_country())
    
    all_fertility <- fer_data %>%
      pull(X2023)
    
    max_fertility <- max(all_fertility, na.rm = TRUE)
    
    f <- fer_data %>%
      filter(Country.Code == selected_country()) %>%
      pull(X2023)
    if (length(f) == 0) {
      error_gauge()
    } else {
    # 2.1 is the replacement rate hence it's marked in green
    # basically the entire developed world is below that so the red threshold is 1.5 tho
    # so that not all western countries are marked in the red and there's moreso a distinction between whose situation is bad and whose is terrible
    gauge(f, min=0, max = max_fertility, label = 'Births per woman', gaugeSectors(
      success = c(2.1, max_fertility),
      warning = c(1.5, 2.1),
      danger = c(0, 1.5)
    ))
    }
  })
  
  output$pop_growth <- renderPlot({
    req(selected_country())
    
    country_name <- pop_data %>%
      filter(Country.Code == selected_country()) %>%
      pull(Country.Name)
    if (length(country_name) == 0) {
      error_plot()
    } else {
      pop_hist <- pop_data %>%
        filter(Country.Code == selected_country()) %>%
        pivot_longer(
          cols = starts_with("X"),
          names_to = "Year",
          values_to = "Population"
        ) %>%
        mutate(Year = as.integer(sub("X", "", Year)))
      
      ggplot(pop_hist, aes(x = Year, y = Population/1000000)) +
        geom_line(color = "blue") +
        geom_point() +
        labs(title = paste("Population Growth for", selected_country()),
             x = "Year", y = "Population (millions)") +
        theme_minimal()
    }
  })
  
  # --- GDP MAP ---
  output$gdp_map <- renderLeaflet({
    leaflet(spData::world) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      addPolygons(layerId = ~iso_a2,
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
      filter(Country.Code == selected_country()) %>%
      pull(X2023)
    
    gdp <- round(gdp/1000000000,1)
    
    if ((length(gdp) == 0)) {
      
      error_value()
    
    } else if ((is.na(gdp))){
      error_value()
      
    } else {
      balue = paste0(format(gdp, big.mark = ","), " billion")
      bolor = "green"
      
      country_name <- gdp_data %>%
        filter(Country.Code == selected_country()) %>%
        pull(Country.Name)
      
      shinydashboard::valueBox(
        value = balue,
        subtitle = paste("GDP of", country_name),
        icon = icon("dollar-sign"),
        width = 14,
        color = bolor
      )
    }
  })
  
  output$gdp_capita_gauge <- renderGauge({
    req(selected_country())
    
    #all_gdp_capita <- gdp_capita_data %>%
    #  pull(X2023)
    
    #max_gdp_capita <- max(all_gdp_capita, na.rm = TRUE)
    
    g <- gdp_capita_data %>%
      filter(Country.Code == selected_country()) %>%
      pull(X2023)
    if (length(g) != 0) { 
        # fictional tax haven countries make the gauge based on max gdp per capita useless (Japan is in the red)
        # so the thresholds are more or less an educated assumption:
        # Green: 30k and up -> most western countries like Japan 33k, Germany 54k, France 44k, usa 82k
        # Yellow: 10k - 30k -> most *somewhat* developed countries like Poland 22k, r*ssia 13k (emphasis on somewhat developed), China 12k, Mexico 13k
        # Red: below 10k -> most developing countries like basically most of sub-saharan Africa
        # print(max_gdp_capita)
        
        # max gauge value set a lil above usa's gdp per capita so that the bar does not look comically small for most countries
        gauge(g, min=0, max = 90000, symbol = '$', gaugeSectors(
          success = c(30000, 90000),
          warning = c(10000, 30000),
          danger = c(0, 10000)
        ))
        
      } else {
        error_gauge() 
      }
      
  })
  
  output$gdp_growth <- renderPlot({
    req(selected_country())
    
    country_name <- gdp_data %>%
      filter(Country.Code == selected_country()) %>%
      pull(Country.Name)
    
    if (length(country_name) != 0){
      
        gdp_hist <- gdp_data %>%
          filter(Country.Code == selected_country()) %>%
          pivot_longer(
            cols = starts_with("X"),
            names_to = "Year",
            values_to = "GDP"
          ) %>%
          mutate(
            Year = as.integer(sub("X", "", Year)),
            GDP = as.numeric(GDP) / 1e9
            
          )
      
        ggplot(gdp_hist, aes(x = Year, y = GDP)) +
          geom_line(color = "green") +
          geom_point() +
          labs(title = paste("GDP Growth for", country_name),
               x = "Year", y = "GDP (Billions)") +
          theme_minimal()
    
      } else {
            error_plot()
      }
    
  })
  
  # --- QOL MAP ---
  output$qol_map <- renderLeaflet({
    leaflet(spData::world) %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      addPolygons(layerId = ~iso_a2,
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
      filter(flagCode == selected_country()) %>%
      pull(HumanDevelopmentIndex_HDI_score_2023)
    
    if (length(hdi) != 0) {
      
      hdi_c <- hdi_data %>%
        filter(flagCode == selected_country()) %>%
        pull(HumanDevelopmentIndex_HDITierCurrent_txt_YearFree)
      
      country_name <- hdi_data %>%
        filter(flagCode == selected_country()) %>%
        pull(country)
      
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
      }
      
      shinydashboard::valueBox(
        value = balue,
        subtitle = paste("HDI of", country_name),
        icon = bicon,
        width = 14,
        color = bolor
        
      )
      
    } else {
        error_value()
    }

  })
  
  output$happy_score <- renderGauge({
    req(selected_country())
    
    country_name <- hdi_data %>%
      filter(flagCode == selected_country()) %>%
      pull(country)
    
    if (length(country_name) != 0) {
    
      all_hap <- hap_data %>%
        pull(HappiestCountriesWorldHappinessReportScore2024)
      
      max_hap <- max(all_hap, na.rm = TRUE)
      
      h <- hap_data %>%
        filter(flagCode == selected_country()) %>%
        pull(HappiestCountriesWorldHappinessReportScore2024)
      
      
      gauge(h, min=0, max = max_hap, gaugeSectors(
        success = c(max_hap*0.7, max_hap),
        warning = c(max_hap*0.5, max_hap*0.7),
        danger = c(0, max_hap*0.5)
      ))
    } else {
      error_gauge()
    }

  })
  
  
  
  output$hdi_growth <- renderPlotly({
    req(selected_country())
    
    target_index <- which(hap_data$flagCode == selected_country())
    
    if (length(target_index) != 0) {
      
          target_pos <- hap_data[target_index, "HappiestCountriesWorldHappinessReportRankings2024"]
          print(paste("Target index:", target_index))
          
          window <- 2
          neighbors <- hap_data[max(1, target_index - window ) : min(target_index + window, nrow(hap_data)), ]
          
          neighbors$color_group <- ifelse(neighbors$HappiestCountriesWorldHappinessReportRankings2024 == target_pos, "red", "#1f77b4")
          
          lowest <- min(neighbors$HappiestCountriesWorldHappinessReportScore2024, na.rm = TRUE)
          highest <- max(neighbors$HappiestCountriesWorldHappinessReportScore2024, na.rm = TRUE)

          neighbors$country <- gsub(" ", "\n", neighbors$country)
          
          p <- ggplot(neighbors, aes(x = reorder(country, HappiestCountriesWorldHappinessReportScore2024), 
                                     y = HappiestCountriesWorldHappinessReportScore2024, 
                                     fill = color_group,
                                     text = paste0(country, "<br>Score: ", HappiestCountriesWorldHappinessReportScore2024, "<br>Rank: ", HappiestCountriesWorldHappinessReportRankings2024))) +
            geom_col() +
            scale_fill_identity() +
            coord_flip(ylim = c(lowest-0.02, highest+0.02)) +
            labs(title = "",
                 x = "Country",
                 y = "Happiness Score") +
            theme_minimal() + theme(
              legend.position = "none",
              title = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(), 
              axis.text.y = element_text(size = 8, margin = margin(r = 200)) 
            )
          
          rank <- as.character(target_pos)
          ggplotly(p, tooltip = "text")
      
    } else {
      p <- error_plot()
      ggplotly(p)
      }
  })
  
  
  
  n_pop_data <- pop_data %>% select(-c(Country.Code, Indicator.Name, Indicator.Code))
  n_fer_data <- fer_data %>% select(-c(Country.Code, Indicator.Name, Indicator.Code))
  
  
  
  
  output$pop_table <- renderDT({
    req(input$pop_choice)
    switch(input$pop_choice,
             "pop" = datatable(
               n_pop_data,
               options = list(
                 scrollX = TRUE,
                 scrollY = "400px",
                 pageLength = 10,
                 autoWidth = TRUE
               ),
               rownames = FALSE
             ),
             
             "fer" =  datatable(
               n_fer_data,
               options = list(
                 scrollX = TRUE,
                 scrollY = "400px",
                 pageLength = 10,
                 autoWidth = TRUE
               ),
               rownames = FALSE
             )
        )
  })
  
  output$gdp_table <- renderDT({ 
    req(input$gdp_choice)
    switch(input$gdp_choice,
           "gdp" = datatable(
             gdp_data,
             options = list(
               scrollX = TRUE,
               scrollY = "400px",
               pageLength = 10,
               autoWidth = TRUE
             ),
             rownames = FALSE
           ),
           
           "gdpc" =  datatable(
             gdp_capita_data,
             options = list(
               scrollX = TRUE,
               scrollY = "400px",
               pageLength = 10,
               autoWidth = TRUE
             ),
             rownames = FALSE
           )
    )
  })
  
  output$qol_table <- renderDT({ 
    req(input$qol_choice)
    switch(input$qol_choice,
           "hap" = datatable(
             hap_data,
             options = list(
               scrollX = TRUE,
               scrollY = "400px",
               pageLength = 10,
               autoWidth = TRUE
             ),
             rownames = FALSE
           ),
           
           "hdi" =  datatable(
             hdi_data,
             options = list(
               scrollX = TRUE,
               scrollY = "400px",
               pageLength = 10,
               autoWidth = TRUE
             ),
             rownames = FALSE
           )
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
})
