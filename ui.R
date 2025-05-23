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
      menuItem("Population", tabName = "population"),
      menuItem("GDP", tabName = "gdp"),
      menuItem("Quality of Life", tabName = "qol")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "population",
              fluidRow(
                column(width=8,
                       box(width=12, style="height: 90vh;",
                           leafletOutput("pop_map", height = "90vh")
                       )
                ),
                column(width=4,
                       
                       box(
                         width = 12, style="height: 17vh;", 
                          valueBoxOutput("pop_count", width = 16)
                       )
                       ,
                       box(width=12, style="height: 28.5vh;",
                           h4("Fertility Rate"),
                           gaugeOutput("fertility_gauge")
                       ),
                       box(width=12, style="height: 40vh;",
                           h4("booper"),
                           plotOutput("pop_growth")
                       )
                )
                
              )
      ),
      
      tabItem(tabName = "gdp",
              fluidRow(
                column(width=8,
                       box(width=12, style="height: 90vh;",
                           leafletOutput("gdp_map", height = "90vh")
                       )
                ),
                column(width=4,
                       box(width=12, style="height: 17vh;", 
                           valueBoxOutput("gdp_value")
                       ),
                       box(width=12, style="height: 28.5vh;",
                           h4("GDP per Capita"),
                           textOutput("gdp_per_capita")
                       ),
                       box(width=12, style="height: 40vh;",
                           plotOutput("gdp_growth", height = "290px")
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
                           valueBoxOutput("hdi_value")
                       ),
                       box(width=12, style="height: 28.5vh;",
                           h4("Happiness Score"),
                           textOutput("happy_score")
                       ),
                       box(width=12, style="height: 28.5vh;",
                           h4("HDI Over Time"),
                           plotOutput("hdi_growth")
                       )
                )
              )
      )
    )
  )

)