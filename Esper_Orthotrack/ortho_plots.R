data.src <- file.path("../Esper_Orthotrack/data")
ortho_data <- readRDS(file= file.path(data.src,"ortho_data.Rds"))

# create plots
# age histogram----
ggplot(ortho_data, aes(x=age_capture)) + geom_histogram(binwidth = 10)

# obtaining the relevant columns and creating a summary 
numeric_columns <- names(select_if(ortho_data, is.numeric))
ortho_vars <- numeric_columns[3:23]

filter_ortho_data <- ortho_data |> filter(sex == "Female")

# add mean and sd for each relevant column
ortho_stats <- data.frame(variable = ortho_vars, mean = colMeans(ortho_data[,ortho_vars], na.rm = T),
                          sd = apply(ortho_data[,ortho_vars],2,sd, na.rm=T), row.names = NULL)

ortho_stats_fem <- data.frame(variable = ortho_vars, mean = colMeans(filter_ortho_data[,ortho_vars], na.rm = T),
                              sd = apply(filter_ortho_data[,ortho_vars],2,sd, na.rm=T) , row.names = NULL)




# template of what I am trying to achieve in the app
ui <- fluidPage(
  titlePanel("Desired Characteristics"),
  fluidRow(column(3,
                  # select gender
                  checkboxGroupInput(inputId = "GenderSelect",
                                     label = "Select Gender:",
                                     choices = c("Male", "Female")),
                  # select age (add comma after previous function)
                  numericInput(inputId = "AgeSelect",
                               label = "Enter Age:",
                               value = 60,
                               min = 0)
                  
                  # select diagnosis 
  )),
  # main panel for displaying outputs----
  mainPanel(
    # output: Histogram----
    plotOutput(outputId = "barplot")
    
  )
)

server <- function(input, output, session) {
  VariableFinder <- reactive({
    req
  })
  
  
  output$barplot <- renderPlot({
    barplot(ortho_data$age_capture, main="Car Distribution",
            xlab="Number of Gears")
    
  })
  
}

