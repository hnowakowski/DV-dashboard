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


dashboardPage(skin = "black",
  dashboardHeader(title="People Inspector"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction"),
      menuItem("Population", tabName = "population"),
      menuItem("GDP", tabName = "gdp"),
      menuItem("Quality of Life", tabName = "qol")
    )
  ),
  dashboardBody(
    tags$head(
      includeCSS("www/style.css")
    ),
    tabItems(
      tabItem(tabName = "introduction",
              h2("Welcome to the People Inspector Dashboard"),
              p("This dashboard provides insights into population, GDP, and quality of life across countries."),
              p("Use the sidebar to navigate between different sections.")
      )
      ,
      tabItem(tabName = "population",
              fluidRow(
                column(width=8,
                       box(width=12, style="height: 92vh;",
                           leafletOutput("pop_map", height = "90vh")
                       )
                ),
                column(width=4,
                       
                       box(
                         width = 12, 
                          valueBoxOutput("pop_count", width = 16)
                       )
                       ,
                       box(width=12,
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
                       box(width=12, 
                           valueBoxOutput("gdp_value")
                       ),
                       box(width=12,
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
                       box(width=12,
                           valueBoxOutput("hdi_value")
                       ),
                       box(width=12,
                           h4("Happiness Score"),
                           gaugeOutput("happy_score")
                       ),
                       box(width=12,
                           h4("HDI Ranking"),
                           plotOutput("hdi_growth")
                       )
                )
              )
      )
    )
  )

)