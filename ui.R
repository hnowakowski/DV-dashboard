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


dashboardPage(
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
                       box(width=12, style="height: 90vh;",
                           leafletOutput("pop_map", height = "100%", width = "100%")
                       )
                ),
                column(width=4,
                       
                       box(
                         width = 12, style="height: 17vh;", 
                          valueBoxOutput("pop_count", height = "100%", width = "100%")
                       )
                       ,
                       box(width=12, style="height: 28.5vh;",
                           h4("Fertility Rate"),
                           gaugeOutput("fertility_gauge")
                       ),
                       box(width=12, style="height: 40vh;",
                           plotOutput("pop_growth", height = "100%", width = "100%")
                       )
                )
                
              )
      ),
      
      tabItem(tabName = "gdp",
              fluidRow(
                column(width=8,
                       box(width=12, style="height: 90vh;",
                           leafletOutput("gdp_map", height = "100%", width = "100%")
                       )
                ),
                column(width=4,
                       box(width=12, style="height: 17vh;", 
                           valueBoxOutput("gdp_value", height = "100%", width = "100%")
                       ),
                       box(width=12, style="height: 28.5vh;",
                           h4("GDP per Capita"),
                           gaugeOutput("gdp_capita_gauge")
                       ),
                       box(width=12, style="height: 40vh;",
                           plotOutput("gdp_growth", height = "100%", width = "100%")
                       )
                )
              )
      ),
      
      tabItem(tabName = "qol",
              fluidRow(
                column(width=8,
                       box(width=12, style="height: 90vh;",
                           leafletOutput("qol_map", height = "90vh")
                       )
                ),
                column(width=4,
                       box(width=12, style="height: 17vh;", 
                           valueBoxOutput("hdi_value", height = "90%", width = "110%")
                       ),
                       box(width=12, style="height: 25vh;",
                           h4("Happiness Score"),
                           gaugeOutput("happy_score")
                       ),
                       box(width=12, style="height: 45vh;",
                           plotOutput("hdi_growth", height = "100%", width = "100%")
                       )
                )
              )
      )
    )
  )

)