library(shiny)
library(shinydashboard)
library(leaflet)
library(spData)
library(dplyr)



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
                         width = 14, style="height: 17vh;", 
                        valueBoxOutput("pop_count", width = 16)
                       )
                       ,
                       box(width=12, style="height: 28.5vh;",
                           h4("Fertility Rate"),
                           uiOutput("fertility_gauge")
                       ),
                       box(width=12, style="height: 40vh;",
                           title = 'boopers',
                           plotOutput("pop_growth", height = "290px")
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
                           plotOutput("pop_growth", height = "290px")
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
                       box(width=12, style="height: 28.5vh;", 
                           h4("HDI"),
                           textOutput("hdi_value")
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