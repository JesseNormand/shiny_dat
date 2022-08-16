library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(qcc)
library(bslib)

#Load data

c_dat <- read.csv("control_data.csv") 

# Define UI for app ----------------------------------------------------------

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  
# Sidebar layout with input and output definitions ----------------------------

  sidebarLayout(
    
# Sidebar panel for inputs ----------------------------------------------------

    sidebarPanel(
      
      #Set shape with Slider
      
      sliderInput(inputId = "NewData",
                  label = "Units Sold: Use the slider to show control chart simulation",
                  min = 10,
                  max = 100,
                  value = 50),
    ),
    
#Main Panel Output: Show scatter plots and data table -------------------------

    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("XBar", plotOutput(outputId = "scatterplot")),
                  tabPanel("mR", plotOutput(outputId = "scatterplotR"))
      
      )
    )
  )
)


#Define server -------------------------------------------------------------

server <- function(input, output, session) {
  
  output$scatterplot <- renderPlot({
    a <- c_dat$units
    lst <- list(a)
    #inputID changes last variable in column for new data input. 
    a[30] <- input$NewData
    
    #xbar chart
    
    qcc(a[1:28], type = "xbar.one", newdata = a[29:30])
  })
  
  output$scatterplotR <- renderPlot({
    a <- c_dat$units
    lst <- list(a)
    #inputID changes last variable in column for new data input. 
    
    a[29] <- input$NewData
    
    #build matrix for qcc to compute moving ragne.
    
    mr <- matrix(cbind(a[1:length(a)-1], a[2:length(a)]), ncol=2)
    
    #R chart
    rc <- qcc(mr[1:27,], type="R", newdata= mr[28:29,])
  
  })
}

#Create a shiny appp object -------------------------------------------------

shinyApp(ui = ui, server = server)

