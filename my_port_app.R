library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(qcc)
library(bslib)

#Load data

c_dat <- read.csv("control_data.csv") 

# Define UI for app - plots features for movies
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  

# Sidebar layout with input and output definitions ----------------------------
  sidebarLayout(
    
# Sidebar panel for inputs ----------------------------------------------------
    sidebarPanel(
      
      # Input: Select variables for the y - axis
      # selectInput(inputId = "y", label = "Y-axis",
      #             choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"),
      #             selected = "audience_score"),
      
      #Select variable for x-axis
      # selectInput(inputId = "x", label = "X-axis",
                  # choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"),
                  # selected = "critics_score"),
      
      
      #Set shape with Slider
      sliderInput(inputId = "NewData",
                  label = "Units Sold: Use the slider to show control chart simulation",
                  min = 10,
                  max = 100,
                  value = 50),
     
     #Show data table
     #checkboxInput(inputId = "show_data",
                  # label  = "Show data table",
                   #value = TRUE)
    
      
     
      
      
    ),
    
#Main Panel Output: Show scatter plots and data table -----------------------
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("XBar", plotOutput(outputId = "scatterplot")),
                  tabPanel("mR", plotOutput(outputId = "scatterplotR"))
      #plotOutput(outputId = "densityplot"),
      #dataTableOutput(outputId = "moviestable")
      
      )
    )
  )
)




#Define server -------------------------------------------------------------

server <- function(input, output, session) {
  
  output$scatterplot <- renderPlot({
    a <- c_dat$units
    lst <- list(a)
    #inputID changes last variable in column for newdata. 
    a[30] <- input$NewData
    qcc(a[1:28], type = "xbar.one", newdata = a[29:30])
  })
  
  output$scatterplotR <- renderPlot({
    a <- c_dat$units
    lst <- list(a)
    #inputID changes last variable in column for newdata. 
    a[29] <- input$NewData
    mr <- matrix(cbind(a[1:length(a)-1], a[2:length(a)]), ncol=2)
    rc <- qcc(mr[1:27,], type="R", newdata= mr[28:29,])
  
  })
  # 
  # #Print Table
  # output$moviestable <- renderDataTable({
  #   if(input$show_data){
  #     DT::datatable(data = movies %>% select(1:7),
  #                   options = list(pageLength = 10),
  #                   rownames = FALSE)
  #   }
  # })
  
}

#Create a shiny appp object -------------------------------------------------

shinyApp(ui = ui, server = server)

