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
      menuItem("How To Use", tabName = "how", icon = icon("question-circle")),
      menuItem("Population", tabName = "population", icon = icon("users")),
      menuItem("GDP", tabName = "gdp", icon = icon("dollar-sign")),
      menuItem("Quality of Life", tabName = "qol", icon = icon("smile")),
      
      menuItem("Tables", icon = icon("table"),
               menuSubItem("Population Table", tabName = "population_table"),
               menuSubItem("GDP Table", tabName = "gdp_table"),
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
        
        table.dataTable thead th {
            text-align: center !important;
        }
        
        table.dataTable tbody tr {
            background-color: #2a2a2a !important;
        }
        
        table.dataTable tbody td {
            text-align: center !important;
        }
        
        .dataTables_wrapper {
            background-color: #1e1e1e !important;
            color: white !important;
        }

      ")),
    
    tabItems(
      #------------------------ intro stuff -------------------
      tabItem(tabName = "introduction",
              img(src = "pplogo.png", height = "160px"),
              div(style = "text-align: center;",
              h2("Welcome to the People Inspector Dashboard"),
              h3("What is the People Inspector Dashboard?"),
              p("The People Inspector Dashboard is an interactive tool designed to provide insights into various country attributes using up-to-date datasets. Whether you’re interested in population statistics, economic indicators, or happiness metrics, this dashboard makes it easy to explore, compare, and visualize key information about countries around the world."),
              h4(br(), br(), "Authors:"),
              
              fluidRow(
                column(width = 5,offset = 1, style = "text-align: right;", img(src = "hubert.jpg", width = "50%")),
                column(width = 5, style = "text-align: left;", img(src = "mukha.jpg", width = "50%"))
              ),
              p(br(),"Hubert Nowakowski (left)"),
              p("Mukhammad Sattorov (right)")
              
              
              )
      )
              
      ,        
              
      #------------------------ intro stuff -------------------
      
      #------------------------ how to stuff -----------------
      
      
  tabItem(tabName = "how",
              
              h3("What kindof data can I see?"),
              h4("Population:"),
              p("Have you ever wondered what the current population of Afghanistan is, what kind of growth China has experienced in the past 10 years, or simply what the fertility rate of Papua New Guinea is?"),
              p("All you have to do is drag you mouse around the map and click on a country"),
              img(src = "popmap.png", height = "600px"),
              p(br()),
              fluidRow(
                column(width = 5,offset = 1, style = "text-align: left;", p("First thing is the value box, which can be of 3 different colors", br(), "Its purpose is to visualize effectively the meaning of that value (whether it is high or too low)", br(), "Next up is the fertility gauge that shows the amount of births per woman of that country. Again, it visualizes if the number is dangerously low.")),
                column(width = 5, style = "text-align: left;", img(src = "popp1.png", height = "250px"))
              ),
              p(br()),
              fluidRow(
                column(width = 5,offset = 1, style = "text-align: left;", p("And lastly is the population growth of that country.", br(), "It helps to educate one on the previous behaviour of the population growth and provide information in predicting further change.")),
                column(width = 5, style = "text-align: left;", img(src = "popp2.png", height = "250px"))
              ),
              
              p(br()),
              h4("GDP:"),
              p("The GDP related page is similar to the population page."),
              p(br()),
              fluidRow(
                column(width = 5,offset = 1, style = "text-align: left;", p("As you can see, it is very similar and is used for more or less the same purpose.", br(), "Total GPD of the current year is found first , followed by the per Capita with colors", br(), "and visualization to distinguish between different levels. And lastly, GPD growth history helps to understand how a specifc country's economy goes about. ")),
                column(width = 5, style = "text-align: left;", img(src = "GPD .png", height = "500px"))
              ),
              
              p(br()),
              h4("Quality of Life:"),
              p("Now here, we have some variety."),
              
              p(br()),
              fluidRow(
                column(width = 5,offset = 1, style = "text-align: left;", p("First, a value box with the country's Human Development Index (with colors to distinguish)", br(), "Followed by the Happiness score of that country.", br(), "And then there is an interactive bar chart")),
                column(width = 5, style = "text-align: left;", img(src = "qol1.png", height = "300px"))
              ),
              
              p(br()),
              fluidRow(
                column(width = 5,offset = 1, style = "text-align: left;", p("This bar chart visualizes the ranking of the country based on it's happiness score. Hovering your cursor will reveal the rank of the country, furthermore, there are 4 more countries present.", br(), "These countries are 2 direct superiors and 2 direct inferiors (though when the happiness scores are equal, they are treated as interchangable", br(), "Your chosen country is highlighted in red")),
                column(width = 5, style = "text-align: left;", img(src = "qol2.png", height = "400px"))
              )
              
      ),        
      
      
      #------------------------ how to stuff -----------------
      
      
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
                           h4("Graph"),
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
      
      tabItem(tabName = "population_table", fluidRow(
        column(4, selectInput("pop_choice", "Choose table:",
                           choices = c("Population" = "pop", "Fertility" = "fer"))),
        column(12, DTOutput("pop_table")))),   
      
      
      tabItem(tabName = "gdp_table", fluidRow(
        column(4, selectInput("gdp_choice", "Choose table:",
                              choices = c("GDP" = "gdp", "GDP per capita" = "gdpc"))),
        column(12, DTOutput("gdp_table")))),
      
      tabItem(tabName = "qol_table", fluidRow(
        column(4, selectInput("qol_choice", "Choose table:",
                              choices = c("Happiness Index" = "hap", "HDI" = "hdi"))),
        column(12, DTOutput("qol_table"))))
      
    )
  )

)