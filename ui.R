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
library(shinythemes)
library(plotly)
library(DT)

dashboardPage(skin = "black",
  dashboardHeader(title="People Inspector"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("info-circle")),
      menuItem("Population", tabName = "population", icon = icon("users")),
      menuItem("GDP", tabName = "gdp", icon = icon("dollar-sign")),
      menuItem("Quality of Life", tabName = "qol", icon = icon("smile")),
      
      menuItem("Tables", icon = icon("table"),
               menuSubItem("Population Table", tabName = "Population_table"),
               menuSubItem("GDP Table", tabName = "GDP_table"),
               menuSubItem("Quality of Life Table", tabName = "qol_table")
      )
    )
  )
  ,
  dashboardBody(
    tags$head(
      includeCSS("www/style.css")
    ),
    
    tags$style(HTML("
          table.dataTable {
            background-color: #1e1e1e !important;  
            color: white !important;               
          }
        
          table.dataTable thead {
            background-color: #333333 !important;  
            color: white !important;
          }
        
          table.dataTable tbody tr {
            background-color: #2a2a2a !important;
          }
        
          .dataTables_wrapper {
            background-color: #1e1e1e !important;
            color: white !important;
          }
      ")),
    
    tabItems(
      tabItem(tabName = "introduction",
              h2("Welcome to the People Inspector Dashboard"),
              p("This dashboard provides insights into population, GDP, and quality of life across countries."),
              p("Use the sidebar to navigate between different sections.")
      ),
      tabItem(tabName = "population",
              fluidRow(
                column(width=8,
                       box(width=12, style="height: 92vh;",
                           leafletOutput("pop_map", height = "90vh")
                       )
                ),
                column(width=4,
                       
                       box(
                         width = 12, style="height: 17vh;",
                          valueBoxOutput("pop_count", width = 16)
                       )
                       ,
                       box(width=12, style="height: 25vh;",
                           h4("Fertility Rate"),
                           gaugeOutput("fertility_gauge")
                       ),
                       box(width=12,
                           h4("Graph :)))"),
                           plotOutput("pop_growth")
                       )
                )
                
              )
      ),
      
      tabItem(tabName = "gdp",
              fluidRow(
                column(width=8,
                       box(width=12, style="height: 92vh;",
                           leafletOutput("gdp_map", height = "90vh")
                       )
                ),
                column(width=4,
                       box(width=12, style="height: 17vh;",
                           valueBoxOutput("gdp_value")
                       ),
                       box(width=12, style="height: 25vh;",
                           h4("GDP per Capita"),
                           gaugeOutput("gdp_capita_gauge")
                       ),
                       box(width=12,
                           plotOutput("gdp_growth")
                       )
                )
              )
      ),
      
      tabItem(tabName = "qol",
              fluidRow(
                column(width=8,
                       box(width=12, style="height: 92vh;",
                           leafletOutput("qol_map", height = "90vh")
                       )
                ),
                column(width=4,
                       box(width=12, style="height: 17vh;",
                           valueBoxOutput("hdi_value")
                       ),
                       box(width=12, style="height: 25vh;",
                           h4("Happiness Score"),
                           gaugeOutput("happy_score")
                       ),
                       box(width=12,
                           h4("HDI Ranking"),
                           plotlyOutput("hdi_growth")
                       )
                )
            )
      ),
      
      tabItem(tabName = "Population_table", fluidRow(
        column(4,
               selectInput("Pop choice", "Choose table:",
                           choices = c("Population" = "pop", "Fertility" = "fer"))
        ),
        column(12,
               DTOutput("pop_table")
        )
      )),   
      
      tabItem(tabName = "GDP_table", h2("GDP Panel"), DTOutput("gdp_table")),
      tabItem(tabName = "qol_table", h2("Quality of Life Panel"), DTOutput("qol_table"))
      
      
    )
  )

)