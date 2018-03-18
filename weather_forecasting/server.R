#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

library(highcharter)

require(shinydashboard)
library(ggplot2)
library(dplyr)

recommendation <- read.csv('recommendation.csv',stringsAsFactors = F,header=T)

weather <- read.csv('rainfall in india 1901-2015.csv', stringsAsFactors = F, header = T)


# create the server functions for the dashboard  
server <- function(input, output) { 
  
  #primary dropdown to select state
  
  output$state <- renderUI({
    selectInput('state1','State',choices = unique(weather$SUBDIVISION),selected = 1)
  })
  
  #some data manipulation to derive the values of KPI boxes
  total.revenue <- sum(recommendation$Revenue)
  sales.account <- recommendation %>% group_by(Account) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
  prof.prod <- recommendation %>% group_by(Product) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
  
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(sales.account$value, format="d", big.mark=',')
      ,paste('Top Account:',sales.account$Account)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
    
    
  })
  
  
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(total.revenue, format="d", big.mark=',')
      ,'Total Expected Revenue'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "green")
    
  })
  
  
  
  output$value3 <- renderValueBox({
    
    valueBox(
      formatC(prof.prod$value, format="d", big.mark=',')
      ,paste('Top Product:',prof.prod$Product)
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")
    
  })
  
  #creating the plotOutput content
  
  output$revenuebyPrd <- renderHighchart({
    ds <- weather %>% filter(SUBDIVISION %in% input$state1) %>% group_by(SUBDIVISION,YEAR) %>% summarise(sum_of_annual = sum(ANNUAL))
    
    hchart(ds,'line',hcaes(x = YEAR, y = sum_of_annual))
    
  })
  
  output$revenuebyRegion <- renderPlot({
    
    ds <- weather %>% filter(SUBDIVISION %in% input$state1) %>% group_by(SUBDIVISION,YEAR) %>% summarise(sum_of_annual = sum(ANNUAL))
    ggplot(data = ds, aes(x=YEAR, y=sum_of_annual)) + 
      geom_line() + ylab("Sum of Annual Rainfall") + 
      xlab("Account") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle(paste0('Rainall of ',input$state1)) 
  })
  
 
  
   
  output$indiamap <- renderHighchart({
    
    cities <- data_frame(
      name = c("London", "Birmingham", "Glasgow", "Liverpool"),
      lat = c(51.507222, 52.483056,  55.858, 53.4),
      lon = c(-0.1275, -1.893611, -4.259, -3),
      z = c(1, 2, 3, 2)
    )
    
    
    
    mapdata <- get_data_from_map(download_map_data("countries/in/in-all"))
    #glimpse(mapdata)
    
    set.seed(1234)
    
    data_fake <- mapdata %>% 
      select(code = `hc-a2`) %>% 
      mutate(value = 1e5 * abs(rt(nrow(.), df = 10)))
    
    
    hcmap("countries/in/in-all", data = data_fake, value = "value",
          joinBy = c("hc-a2", "code"), name = "Fake data",
          dataLabels = list(enabled = TRUE, format = '{point.name}'),
          borderColor = "#FAFAFA", borderWidth = 0.1,
          tooltip = list(valueDecimals = 2, valuePrefix = "$", valueSuffix = " USD")) 
  })
  
  
}



