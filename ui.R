library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(
    dashboardHeader(title="people inspector idk"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Population", tabName = "population"),
        menuItem("GDP", tabName = "gdp"),
        menuItem("Quality of Life", tabName = "qol")
      )
    ),
    dashboardBody(
      fluidRow( # display flex does not work :(((((
        column(width=8,
               div(id="map_select_pop",
                   box(width=12, style="height: 90vh;", "map here")
                   )),
        column(width=4,
               div(id="value_box_pop", 
                   box(width=12, style="height: 28.5vh;", "pop count here")),
               div(id="gauge_pop",
                   box(width=12, style="height: 28.5vh;", "fertility rate here")),
               div(id="graph_pop",
                   box(width=12, style="height: 28.5vh;", "pop graph here"))
               )
        )
      )
  )
)