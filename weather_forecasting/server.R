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
library(plotly)

library(highcharter)

require(shinydashboard)
library(ggplot2)
library(dplyr)
library("reshape2")
#install.packages('TSstudio')

library(TSstudio)


#setwd('/Users/amrs/weather_forecasting_app/weather_forecasting')

recommendation <- read.csv('recommendation.csv',stringsAsFactors = F,header=T)

#weather <- read.csv('rainfall in india 1901-2015.csv', stringsAsFactors = F, header = T)

weather <- read.csv('districtwise1901_to_2002.csv', stringsAsFactors = F, header = T)


weather <- na.omit(weather)

weather$all <- sum(weather[,4:15])
### remove outliers


# create the server functions for the dashboard  
server <- function(input, output) { 
  
  #primary dropdown to select state
  
  output$state <- renderUI({
    selectInput('state1','State',choices = unique(weather$State),selected = 1)
  })
  
  
  
  #next dropdown to select district
  output$district <- renderUI({
    selectInput("district1", "District", choices = as.character(weather[weather$State==input$state1,"District"]),selected = 1)
  })
  
  
  
  total <- reactive({
    
   # weather <- read.csv('rainfall in india 1901-2015.csv', stringsAsFactors = F, header = T)
    
    weather <- read.csv('districtwise1901_to_2002.csv', stringsAsFactors = F, header = T)
    
    
    tn <- weather %>% filter(State %in% input$state1) %>% 
      filter(District %in% input$district1)
    
    
    tn_months <- tn %>% select('Year',(month.abb))
    
    dummy.df <- tn_months 
    
    dummy.df <- melt(tn_months, id.vars = "Year")
    
    dummy.df$Date <- as.Date(paste(dummy.df$YEAR, dummy.df$variable, "01", sep = "-"),
                             format = ("%Y-%b-%d"))
    dummy.df <- dummy.df[order(dummy.df$Date), ]
    
    #View(dummy.df)
    
    
    dummy.df.ts <- ts(dummy.df$value, start=c(1901,1), end=c(2015,12), frequency=12)
    
    ts_seasonal(dummy.df.ts)
    
    total <- dummy.df.ts
    
    total
  })
  
  
  
  
  #some data manipulation to derive the values of KPI boxes
  total.revenue <-  recommendation %>% group_by(Account) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
  sales.account <- recommendation %>% group_by(Account) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
  prof.prod <- recommendation %>% group_by(Product) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(33.5, format="d", big.mark=',')
      ,paste('Average yearly Rainfall:', input$district1)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
    
    
  })
  
  
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(35.5, format="d", big.mark=',')
      ,paste('Total Expected Rainfall',input$district1)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")
    
  })
  
  
  
  output$value3 <- renderValueBox({
    
    valueBox(
      formatC(33.7, format="d", big.mark=',')
      ,paste('Past Rainfall:', input$district1)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "yellow")
    
  })
  
  #creating the plotOutput content
  
  output$revenuebyPrd <- renderHighchart({
    ds <- weather %>% filter(State %in% input$state1) %>% 
      filter(District %in% input$district1) %>% group_by(Year) %>% summarise(sum_of_annual = sum(Jan))
    
    hchart(ds,'line',hcaes(x = Year, y = sum_of_annual))
    
  })
  
  output$revenuebyRegion <- renderPlotly({
    
    #ds <- weather %>% filter(SUBDIVISION %in% input$state1) %>% group_by(SUBDIVISION,YEAR) %>% summarise(sum_of_annual = sum(ANNUAL))
    
    total <- total()
    
    train <- window(total, start = c(1990,01),end = c(2014,12))
    
    test <- window(total, start = c(2015,01))
    
    
    library(forecast)
    
    model <- HoltWinters(train)
    
    plot(model)
    
    forecasts <- forecast(model,12)
    
    mean(abs((forecasts$mean - test)/test))
    
    library(forecast)
    
    h = 12
    
    total2 <- window(total, start = c(1980,01))
    
    split_ts <- ts_split(total2, sample.out = h)
    
    train <- split_ts$train
    test <- split_ts$test
    
    
    #m2 <- auto.arima(train, stepwise = F)
    
    m2 <- nnetar(train)
    
    f2 <- forecast(m2, h = h)
    
    m3 <- ets(train)
    
    f3 <- forecast(m3, h = h)
    
    
    test_forecast(actual = total2, forecast.obj = f2, train = train, test = test)
    
    
    
  })
  
  output$ets <- renderPlotly({
    
    #ds <- weather %>% filter(SUBDIVISION %in% input$state1) %>% group_by(SUBDIVISION,YEAR) %>% summarise(sum_of_annual = sum(ANNUAL))
    
    total <- total()
    
    train <- window(total, start = c(1990,01),end = c(2014,12))
    
    test <- window(total, start = c(2015,01))
    
    
    library(forecast)
    
    model <- ets(train)
    
    plot(model)
    
    forecasts <- forecast(model,12)
    
    mean(abs((forecasts$mean - test)/test))
    
    library(forecast)
    
    h = 12
    
    total2 <- window(total, start = c(1980,01))
    
    split_ts <- ts_split(total2, sample.out = h)
    
    train <- split_ts$train
    test <- split_ts$test
    
   
    m3 <- auto.arima(train)
    
    f3 <- forecast(m3, h = h)
    
    
    test_forecast(actual = total2, forecast.obj = f3, train = train, test = test)
    
    
    
  })
  
  output$seasonalplot1 <- renderPlotly({
    
    #ds <- weather %>% filter(SUBDIVISION %in% input$state1) %>% group_by(SUBDIVISION,YEAR) %>% summarise(sum_of_annual = sum(ANNUAL))
    
    total <- total()
    
    train <- window(total, start = c(1990,01),end = c(2014,12))
    
    test <- window(total, start = c(2015,01))
    
    
   # library(forecast)
    
    #model <- HoltWinters(train)
    
    #plot(model)
    
    #forecasts <- forecast(model,12)
    
    #mean(abs((forecasts$mean - test)/test))
    
    #library(forecast)
    
    h = 12
    
    total2 <- window(total, start = c(1980,01))
    
    split_ts <- ts_split(total2, sample.out = h)
    
    train <- split_ts$train
    test <- split_ts$test
    
    
    #f1 <- snaive(train, h = h)
    
    #test_forecast(actual = total2, forecast.obj = f1, train = train, test = test)
    
    ts_seasonal(total,type = 'normal')
    
    #ts_heatmap(total)
    
  })
  
  
  output$heatmap1 <- renderPlotly({
    
    #ds <- weather %>% filter(SUBDIVISION %in% input$state1) %>% group_by(SUBDIVISION,YEAR) %>% summarise(sum_of_annual = sum(ANNUAL))
    
    total <- total()
    
    train <- window(total, start = c(1990,01),end = c(2014,12))
    
    test <- window(total, start = c(2015,01))
    
    ts_heatmap(total)
    
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



