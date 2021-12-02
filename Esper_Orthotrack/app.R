#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# select libraries
library(shiny)
library(tidyverse)
library(motionTools)
options(bitmapType='cairo') # might not be necessary 

# import data
data.src <- file.path("../Esper_Orthotrack/data")
ortho_data <- readRDS(file= file.path(data.src,"ortho_data.Rds"))

ui <- fluidPage(
  titlePanel("Orthotrack Data"),
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        textInput(inputId = "name",
                  label = "What's your name?",
                  placeholder = "Name")),
      mainPanel(textOutput("greeting"),
      textAreaInput("story", "Tell me about yourself", rows = 3)
      )
    )
  ),
  sliderInput(inputId = "x",
              label = "If x is",
              min = 1,
              max = 50,
              value = 5),
  sliderInput(inputId = "y",
              label = "and y is",
              min = 1,
              max = 50,
              value = 5),
  textOutput("product"),
  textOutput("prod_plus5"),
  
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  plotOutput("plot"),
  
  # Select demographics of data
  fluidRow(
    # select sex
    column(2,
           selectInput("sex", "Select sex",
                       choices = unique(na.omit(ortho_data$sex)))),
    # select age
    column(2,
           numericInput("age", "Enter the age", 
                        value = 60, min = 0, max = 100)),
    # select diagnosis 
    column(3, 
           selectInput("diag", "Select the diagnosis",
                       choices = unique(na.omit(ortho_data$dw_primary_diagnosis)),
                       selected = "Parkinson's disease"))
    ),
  titlePanel("Overall Ortho Stats"),
  tableOutput("ortho_stat")
)


server <- function(input, output, session) {
  output$greeting <- renderText({
    paste0("Hello, ", input$name)
  })
  
  
  # reactive expression
  prod_xy <- reactive({
    input$x * input$y
  })
  
  output$product <- renderText({
    paste0("then, x times y is: ", input$x * input$y)
  })
  
  output$prod_plus5 <- renderText(({
    paste0("then (x * y) + 5 is: ", prod_xy() + 5)
  }))
  
  # reactive
  dataset1 <- reactive({
    get(input$dataset1, "package:datasets")
  })
  
  output$summary <- renderPrint({
    summary(dataset1())
  })
  
  output$plot <- renderPlot({
    plot(dataset1())
  }, res = 96)
 
  # mean and sd of ortho data----
  # select the relevant columns
  numeric_columns <- names(select_if(ortho_data, is.numeric))
  ortho_vars <- numeric_columns[3:23]
  # add mean and sd for each relevant column
  ortho_stats <- data.frame(variable = ortho_vars, mean = colMeans(ortho_data[,ortho_vars], na.rm = T),
                            sd = apply(ortho_data[,ortho_vars],2,sd, na.rm=T), row.names = NULL )
  
  output$ortho_stat <- renderTable(ortho_stats)
  
  
  

  
}
  






# Run the application 
shinyApp(ui = ui, server = server)
