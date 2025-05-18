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
    dashboardBody(h2("idk"))
  )
)