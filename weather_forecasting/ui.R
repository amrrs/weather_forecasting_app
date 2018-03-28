library(shiny)
library(dplyr)

library(highcharter)

require(shinydashboard)

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Weather Forecast")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
             href = "https://www.salesforce.com")
  )
)


frow1 <- fluidRow(
  
  shiny::uiOutput('state'),
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

frow2 <- fluidRow(
  
  box(
    title = "Interactive Rainfall Trend"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,highchartOutput("revenuebyPrd", height = "300px")
    ,width = 12
  )
)   

frow3 <- fluidRow(
box(
    title = "Rainfall Seasonal Trend"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("seasonalplot1", height = "300px")
    ,width = 12
  ) 
  
)


frow4 <- fluidRow(
  
  box(
    title = "Rainfall Forecast"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("revenuebyRegion", height = "300px")
    ,width = 12
  ) 
  
)



frow6 <- fluidRow(   
  box(
    title = "Rainfall in Map"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,highchartOutput("indiamap")
    ,width = 12
  ) 
  
)

# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2, frow3, frow4, frow6)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Weather Forecasting App', header, sidebar, body, skin='red')
