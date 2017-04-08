#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(quantmod)
library(forecast)
getSymbols(c("AAPL","GOOG","FB","NFLX"))
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  reactiveValues()
  output$distPlot <- renderPlot({

  	df <- as.name(input$symbol)
  	adjPrice <- eval(df)[,6]
  	colnames(adjPrice) <- "Close"
  	plot(adjPrice, main=paste0("Close Prices for ",input$symbol))
    
  })
  
  output$ClosePrice = renderTable({
  	df <- as.name(input$symbol)
  	adjPrice <- eval(df)[,6]
  	colnames(adjPrice) <- "Close"
  	fit  <- auto.arima(adjPrice)
  	forecast(fit, h=2)
  })

 
})
