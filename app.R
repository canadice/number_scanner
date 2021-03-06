
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny, quietly = TRUE)
require(shinythemes, quietly = TRUE)
require(bmp, quietly = TRUE)
require(plyr, quietly = TRUE)
require(dplyr, quietly = TRUE)

source("functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("slate"), title = "Number scanner",
                
                # Application title
                titlePanel("BMP to csv converter"),
                
                fluidRow(
                  column(width = 2,
                         wellPanel(
                           fileInput(inputId = "data", label = "Upload your own .bmp \n Assumes the MNIST 28x28 frame", 
                                     multiple = FALSE, buttonLabel = "Browse", placeholder = ""),
                           br(), 
                           br()
                           # withSpinner(uiOutput("upload")) 
                         )
                  ),
                  column(width = 8,
                         uiOutput("export_button"))
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  getData <- reactive({
    inFile <- input$data$datapath
    
    data <- read.bmp(f = inFile)
    
    data <- converter(data)
    
    # data <- data_subset(data)
    
    return(data)
  })
  
  output$export_button <- renderUI({
    if(is.null(input$data)){
      return(NULL)
    } else {
      downloadButton(outputId = "export_data", 
                     label = "Download data")
    }
  })
  
  output$export_data <- downloadHandler(
    filename = function(){
      "raw_data.csv"
    },
    content = function(file){
      if(is.null(input$data)){
        return(NULL)
      }
      
      write.csv2(x = getData(), file, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

