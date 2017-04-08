#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
mycss <- "
#plot-container {
position: relative;
}
#loadingText {
position: absolute;
left: 50%;
top: 50%;
z-index: -1;
margin-top: -33px;  /* half of the spinner's height */
margin-left: -33px; /* half of the spinner's width */
}
#distPlot.recalculating {
z-index: -2;
}
"
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$head(tags$style(HTML(mycss))),
  # Application title
  titlePanel("Predict the Stock Price"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
    	selectInput("symbol",
                   "Choose a Stock Symbol:",
                    choices = c("FB","AAPL","NFLX","GOOG"),
    		    selected = "FB"
    		    )
    	
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
    	h4("This cool application predicts the closing price for the next two trading days."),
    	p("Simply select the stock symbol to the left and watch the table below the graph."),
    	p("The first two values in the table are the predicted closes. Other values are the 
    	  80% and 95% confidence intervals for the close price."),
    	
    	div(id = "plot-container",
    	    tags$h2("LOADING....", id="loadingText"),
    	    plotOutput("distPlot")
    	),	
       tableOutput("ClosePrice")
    )
  )
  
))
