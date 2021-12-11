#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# select libraries----
library(shiny)
library(tidyverse)
library(plotly)
library(scales)
library(motionTools)
library(readxl)
library(DT)
library(kableExtra)
library(bslib)

# set up theme----
# choose theme, primary color, and font
my_theme <- bs_theme(version = 4, 
                     bootswatch = "simplex",
                     primary = "#007dba", 
                     base_font = font_google("Tinos"))
# set graph theme 
thematic::thematic_shiny(font = "auto")
# import data----
raw_data <- ReadRedcapReport(token = Sys.getenv("movementDisorders_redcap_token"), 
                             url = "https://redcap.emory.edu/api/", report_id = 31113)
# clean up redcap import----
ortho_data <- raw_data |> 
  # fill in missing data among patients
  group_by(record_id) |> fill(names(raw_data)[-1], .direction = "updown") |> 
  # choose only the unique observations
  distinct(record_id, .keep_all = T) |> 
  # choose which variables are needed
  select(-c(redcap_repeat_instrument, redcap_repeat_instance, patient_fullpath_neurology, patient_folder_neurology, nrm_file, report_final_impression)) 

# change sex to factor variable
ortho_data$sex <- factor(ortho_data$sex, levels=c(0,1), labels=c("Male", "Female")) 

# import descriptor files 
data.src <- file.path("../Esper_Orthotrack/data")
ortho_label <- read_excel(path=file.path(data.src, "ortho_labels.xlsx"), sheet = 1) # labels for variables
diag_label <- read_excel(path=file.path(data.src, "ortho_labels.xlsx"), sheet = 2)  # categories for diagnosis


# ui----
ui <- fluidPage(
  # change output display----
  # theme from bs_lib bootswatch 
  theme = my_theme,
  
 
  # ORTHO section----
  titlePanel( title = h1(strong("Orthotrack Data Analysis"), align = "center")),
  
  # demographic info 
  h2(strong("Demographic Information")),
  wellPanel(
    fluidRow(
      # select sex
      column(2,
             selectInput("sex", strong("Select sex"),
                         choices = unique(na.omit(ortho_data$sex)))),
      
      # select age
      column(2,
             numericInput("age", strong("Enter the age"), 
                          value = 60, min = 0, max = 100)),
      
      # select diagnosis 
      column(3, 
             selectInput("diag", strong("Select the diagnosis"),
                         choices = unique(na.omit(ortho_data$dw_icd)),
                         selected = "G20")),
      
      # display description of diagnosis 
      column(3,
             strong("Diagnosis Name"),
             textOutput("diag_name")
      )
    )
  ),
  titlePanel("Overall Ortho Stats"),
 
  # user input on orthotrack metrics
  sidebarLayout(
    sidebarPanel(
     
      # step length
      fluidRow(
        
        column(5,
               numericInput("step_leng_ave_rt", "Step Length Average - Right", value = 48)),
        column(5,
               numericInput("step_leng_ave_lft", "Step Length Average - Left", value = 48),
               )
      ),
      # stride length
      fluidRow(
        column(5,
               numericInput("strid_leng_ave_rt", "Stride Length Average - Right", value = 96)),
        column(5,
               numericInput("strid_leng_ave_lft", "Stride Length Average - Left", value = 96)),
      ),
      # forward velocity 
      fluidRow(
        column(5,
               numericInput("for_vel_rt", "Forward Velocity Average - Right", value = 86)),
        column(5,
               numericInput("for_vel_lft", "Forwards Velocity Average - Left", value = 86)),
      ),
      # cadence
      fluidRow(
        column(5,
               numericInput("cad_ave_rt", "Cadence Average - Right", value = 108)),
        column(5,
               numericInput("cad_ave_lft", "Cadence Average - Left", value = 108)),
      ),
      # Total Support time
      fluidRow(
        column(5,
               numericInput("tot_sup_time_rt", "Total Support Time - Right", value = 64)),
        column(5,
               numericInput("tot_sup_time_lft", "Total Support Time - Left", value = 64))
        ),
      # Swing phase 
      fluidRow(
        column(5,
               numericInput("swing_phas_rt", "Swing Phase  Average - Right", value = 35)),
        column(5,
               numericInput("swing_phas_lft", "Swing Phase  Average - Left", value = 35))
        ),
      # Initial Double Support time
      fluidRow(
        column(5,
               numericInput("int_2x_sup_time_rt", "Initial Double Support Time  - Right", value = 14)),
        column(5, 
               numericInput("int_2x_sup_time_lft", "Initial Double Support Time  - Left", value = 14))
        ),
      # Single Support Time
      fluidRow(
        column(5,
               numericInput("single_sup_time_rt", "Single Support Time - Right", value = 36)),
        column(5,
               numericInput("single_sup_time_lft", "Single Support Time - Left", value = 36))
        ),
      # Step Width
      numericInput("step_width", "Step Width", value = 12),
      # Number of Steps
      fluidRow(
        column(5,
               numericInput("num_steps_rt", "Number of Steps - Right", value = 14)),
        column(5, 
               numericInput("num_steps_lft", "Number of Steps - Left", value = 14))
        ),
      # Number of Strides
      fluidRow(
        column(5, 
               numericInput("num_strides_rt", "Number of Strides - Right", value = 13)),
        column(5,
               numericInput("num_strides_lft", "Number of Strides - Left", value = 13))
        ),
      width = 3
    ),
    # display results---- 
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Overall", htmlOutput("td_overall"), tableOutput("total_stat")),
                  tabPanel("By Gender", htmlOutput("td_gender"), tableOutput("gender_stat")), # sample with incorrect table
                  tabPanel("By Age", htmlOutput("td_age"), tableOutput("age_stat")), # incorrect output but works
                  tabPanel("By Diagnosis", htmlOutput("td_diag"), tableOutput("diag_stat")), 
                  tabPanel("Combined Filters", htmlOutput("td_combined"), tableOutput("combined_stat")), 
                  tabPanel("Plots", 
                           h4("Overall", align = "center"),
                           htmlOutput("pd_overall"),
                           plotlyOutput("plot_overall"), 
                           fluidRow(
                                    column(6,
                                           h5("Gender", align = "center"),
                                           h6(htmlOutput("pd_gender"), align = "center"),
                                           plotlyOutput("plot_gender")),
                                    column(6,
                                           h6("Age", align = "center"),
                                           h6(htmlOutput("pd_age"), align = "center"),
                                           plotlyOutput("plot_age")
                           )),
                           fluidRow(
                             column(6,
                                    h5("Diagnosis", align = "center"),
                                    h6(htmlOutput("pd_diag"), align = "center"),
                                    plotlyOutput("plot_diag")),
                             column(6,
                                    h5("Combined", align = "center"),
                                    h6(htmlOutput("pd_combined"), align = "center"),
                                    plotlyOutput("plot_combined")) 
                             )
                           ),
                  )
      )
    ),
  # break
  br(),
  br(),
  
  
  # IGNORE (practice)----
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        textInput(inputId = "name",
                  label = "What's your name?",
                  placeholder = "Name"),
        actionButton("submit", "Submit!")),
      mainPanel(textOutput("greeting"),
                textOutput("mess"),
                textAreaInput("story", "Tell me about yourself", rows = 3)
      )
    )
  ),
 
  
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  plotOutput("plot"),
  
  fluidRow(
    column(3, 
           numericInput("lambda1", label = "lambda1", value = 3),
           numericInput("lambda2", label = "lambda2", value = 5),
           numericInput("n", label = "n", value = 1e4, min = 0),
           actionButton("simulate", "Simulate!")
    ),
    column(9, plotOutput("hist"))
  )
  
  
)

# server----
server <- function(input, output, session) {

  
 
  # reactive values from user input----
  # characteristics 
  sex <- reactive(input$sex)
  age <- reactive(input$age)
  diag <- reactive(input$diag)
  diag_desc <- reactive(as.character(diag_label[diag_label$icd_code == diag(), "desc"]))
  
  # display diagnosis name
  output$diag_name <- renderText({
    diag_desc()
  })
  
  # ortho variables----
  step_leng_ave_rt <- reactive(input$step_leng_ave_rt)
  step_leng_ave_lft <- reactive(input$step_leng_ave_lft)
  
  strid_leng_ave_rt <- reactive(input$strid_leng_ave_rt)
  strid_leng_ave_lft <- reactive(input$strid_leng_ave_lft)
  
  for_vel_rt <- reactive(input$strid_leng_ave_rt)
  for_vel_lft <- reactive(input$strid_leng_ave_lft)
  
  cad_ave_rt <- reactive(input$cad_ave_rt)
  cad_ave_lft <- reactive(input$cad_ave_lft)
  
  tot_sup_time_rt <- reactive(input$tot_sup_time_rt)
  tot_sup_time_lft <- reactive(input$tot_sup_time_lft)
  
  swing_phas_rt <- reactive(input$swing_phas_rt)
  swing_phas_lft <- reactive(input$swing_phas_lft)
  
  int_2x_sup_time_rt <- reactive(input$int_2x_sup_time_rt)
  int_2x_sup_time_lft <- reactive(input$int_2x_sup_time_lft)
  
  single_sup_time_rt <- reactive(input$single_sup_time_rt)
  single_sup_time_lft <- reactive(input$single_sup_time_lft)
  
  step_width <- reactive(input$step_width)
  
  num_steps_rt <- reactive(input$num_steps_rt)
  num_steps_lft <- reactive(input$num_steps_lft)
  
  num_strides_rt <- reactive(input$num_strides_rt)
  num_strides_lft<- reactive(input$num_strides_lft)
  
  # Create reactive vector of user ortho values----
  # vector
  user_ortho_values <- reactive(
    # create vector of user_values
    ortho_values_input <- c(step_leng_ave_rt(), step_leng_ave_lft(),
                            strid_leng_ave_rt(), strid_leng_ave_lft(),
                            for_vel_rt(), for_vel_lft(),
                            cad_ave_rt(), cad_ave_lft(),
                            tot_sup_time_rt(), tot_sup_time_lft(),
                            swing_phas_rt(), swing_phas_lft(),
                            int_2x_sup_time_rt(), int_2x_sup_time_lft(),
                            single_sup_time_rt(), single_sup_time_lft(),
                            step_width(), 
                            num_steps_rt(), num_steps_lft(),
                            num_strides_rt(), num_strides_lft()
    )
  )
  
  # filtered data sets----
  # GOAL: create data sets that are filtered from ortho data based on user input
  
  # filter sex data
  ortho_sex <- reactive({
    gender_subset <- dplyr::filter(ortho_data, sex == sex())
    return(gender_subset)
  })
  
  # filter age data 
  ortho_age <- reactive({
    age_subset <-  dplyr::filter(ortho_data, between(age_capture, age()-5, age()+5))
    return(age_subset)
  })
  
  # filter diagnosis data
  ortho_diag <- reactive({
    diag_subset <- dplyr::filter(ortho_data, dw_icd == diag())
    return(diag_subset)
  })
  
  # combine all filters
  ortho_filtered <- reactive({
    filter_subset <- ortho_data |> filter(sex == sex() & 
                                            between(age_capture, age()-5, age()+5) &
                                            dw_icd == diag())
    return(filter_subset)
  })

 
  # create summary table of all ortho track variables----
  sumTable <- function(ortho_data_set){
    # un-group data and select ortho track variables
    ortho_stats <- ortho_data_set |> ungroup() |> select(c(step_leng_ave_rt:num_strides_lft)) |> 
      # convert data frame from wide to long; group by key = variable
      gather(factor_key = T) |>  group_by(key)  |> 
      # create summary statistics of each variable 
      summarise(mean= mean(value, na.rm=T), sd=sd(value, na.rm = T)) |> 
      # rename key to ortho_var
      rename(Metric= key)
    # change display of variables
    ortho_stats$Metric <- ifelse(ortho_stats$Metric == ortho_label$ortho_var, 
                                 ortho_label$desc_name, ortho_stats$Metric)

    return(ortho_stats)
  }
  
  # generate percentiles----
  ## Goal: create data frame that shows mean, sd, and percentile of user value 
  # data frame from user inputs
  user_value_df <- reactive({
    user_val <- data.frame(user_input = user_ortho_values())
    return(user_val)
  })
  
  # function to generate data frame with percentiles
  pctlTable <- function(ortho_data_set, user_ortho_values){
    # user_input to summary stats data frame for that data set 
    pctl_df <- bind_cols(ortho_data_set, user_ortho_values) |> 
      # adds percentile column
      mutate(Percentile = (percent(round(pnorm(q=user_input, mean = mean, sd=sd), 2)))) |> 
      # removes the column from user input
      select(-c(user_input))
    
    # return plot
    return(pctl_df)
  }
  
  # Create percentile summary data set for each subset of data----
  # total subjects
  overall_pctls <- reactive({
    over_all_pct <- pctlTable(sumTable(ortho_data), user_value_df())
    return(over_all_pct)
  })
  
  # percentile data frame based on gender input 
  gender_pctls <- reactive({
    gender_subset <- dplyr::filter(ortho_data, sex == sex())
    gender_sum <- sumTable(gender_subset)
    gender_pctls <- pctlTable(gender_sum, user_value_df())
    return(gender_pctls)
  })
  
  # percentile data frame based on age input 
  age_pctls <- reactive({
    age_subset <- dplyr::filter(ortho_data, between(age_capture, age()-5, age()+5))
    age_sum <- sumTable(age_subset)
    age_pctls <- pctlTable(age_sum, user_value_df())
    return(age_pctls)
  })
  
  # percentile data frame based on diagnosis input  
  diag_pctls <- reactive({
    diag_subset <- dplyr::filter(ortho_data, dw_icd == diag())
    diag_sum <- sumTable(diag_subset)
    diag_pctls <- pctlTable(diag_sum, user_value_df())
    return(diag_pctls)
  })
  
  # percentile data frame based on combined filters of user input
  combined_pctls <- reactive({
    filter_subset <- ortho_data |> filter(sex == sex() & 
                                            between(age_capture, age()-5, age()+5) &
                                            dw_icd == diag())
    combined_sum <- sumTable(filter_subset)
    combined_pctls <- pctlTable(combined_sum, user_value_df())
    return(combined_pctls)
  })
  
  # generate informative descriptions for data output----
  # overall 
  overall_info <- reactive({
    paste0("<b>Number of observations: </b>", nrow(ortho_data))
  })
  # gender
  gender_info <- reactive({
    paste0("<b>Gender:  </b>", sex(), "  <b>Number of Observations: </b>", nrow(ortho_sex()))
  })
  # age
  age_info <- reactive({
    paste0("<b>Age range:  </b>(", age()-5, ", ", age()+5, ")", "  <b>Number of Observations: </b>", nrow(ortho_age()))
  })
  
  # diagnosis
  diag_info <- reactive({
    paste0("<b>Diagnosis:  </b>", diag_desc(), "  <b>Number of Observations: </b>", nrow(ortho_diag()))
  })
  
  # combined
  combined_info <- reactive({
    paste0("<b>Gender:  </b>", sex(), "    <b>Age range:  </b>(", age()-5, ", ", age()+5, ")", "    <b>Diagnosis:  </b>", diag_desc(), "    <b>Number of Observations: </b>", nrow(ortho_filtered()))
  })
  
 
  
  # display summary data per dataset----
  # overall subjects 
  output$total_stat <- renderTable({
    overall_pctls()
  })
  # info on overall data
  output$td_overall <- renderText({
    overall_info()
  })  
  
  # table from filtered data set
  
  # by gender
  output$gender_stat <- renderTable({
    gender_pctls()
  })
  
  # info on gender table
  output$td_gender <- renderText({
    gender_info()
  })  
  
  # by age
  output$age_stat <- renderTable({
    age_pctls()
  })
  # distribution info for age
  output$td_age <- renderText({
    age_info()
  })  
  
  # by diagnosis
  output$diag_stat <- renderTable({
    diag_pctls()
  })
  # distribution info for diagnosis
  output$td_diag <- renderText({
    diag_info()
  })  
  
  # filtered with all values
  output$combined_stat <- renderTable({
    combined_pctls()
  })
  # informative on filtered table
  output$td_combined <- renderText({
    combined_info()
  })  
  

  # value/average plot----
  gen_mean_plot <- function(ortho_data_set, user_input){
    # generate average of each metric 
    ortho_mean <- ortho_data_set |> ungroup() |> 
      # select ortho variables
      select(c(step_leng_ave_rt:num_strides_lft)) |> 
      # convert data frame from wide to long; group by key = variable
      gather(factor_key = T) |>  group_by(key) |>  
      # calculate mean of each data set 
      summarise(mean= mean(value, na.rm=T)) |> 
      # rename key variable to metric 
      rename(Metric= key)
    
    # add left/right category; NA for step width 
    ortho_mean$side <- ifelse(str_detect(ortho_mean$Metric, "_rt"), "right",
                              ifelse(str_detect(ortho_mean$Metric, "_lft"), "left",
                                     NA))
    
    ## change Metric name
    
    # obtain variable names
    ortho_vars <- unique(ortho_mean$Metric)
    # extract relevant part of variable description
    metric_desc <- sapply(strsplit(ortho_label$desc_name, "-"), "[[",1)
    # remove the "Average" part of the name and obtain the stripped description
    prefix <- data.frame(prefix = sapply(ifelse(str_detect(metric_desc, "Average"),strsplit(metric_desc, " Average "), str_trim(metric_desc, side = "both")),"[[",1))
    
    ## Combine data set for analysis
    # combine stats, descriptive names and user input 
    ortho_mean <- bind_cols(ortho_mean, prefix, user_input)
    
    # find user_value/mean
    ortho_mean$mean_multiple <- round(ortho_mean$user_input/ortho_mean$mean, 2)
    # scale the values (< 1 = neg, > 1 = pos. to show direction of change )
    ortho_mean$mm_scaled <- ifelse(ortho_mean$mean_multiple < 1, ortho_mean$mean_multiple * -1, ortho_mean$mean_multiple)
    
    # set factor to keep order
    ortho_mean$prefix <- factor(ortho_mean$prefix, levels = rev(unique(ortho_mean$prefix)), ordered = T)
    ortho_mean$side <- factor(ortho_mean$side, levels = c("left", "right"))
    
    # generate bar plot
    plot_basic <- ggplot(ortho_mean, aes(prefix, mm_scaled, fill = side, text = paste('Side: ', str_to_title(side), '<br>Metric: ', prefix, '<br>Average: ', round(mean, 2), '<br>Patient value: ', user_input,  '<br>Value/Average: ', percent(mean_multiple)))) + 
      geom_bar(position = "dodge", stat = "identity") + coord_flip() + 
      guides(fill = guide_legend(reverse = F)) + xlab("") + ylab("Percent") +
      scale_fill_manual("legend", values = c("right" = "#1578cf", "left" = "#77c2fe"),na.value = "#484c44") +
      scale_y_continuous(labels = scales::percent_format(scale = 100))
    plot <- ggplotly(plot_basic, tooltip = c("text"))
 
    return(plot)
  }
  
  
  # generate plots----
  # create plot from overall data
  overall_plot <- reactive({
    display_plot <- gen_mean_plot(ortho_data, user_value_df())
    return(display_plot)
  })
  # display overall plot
  output$plot_overall <- renderPlotly({
    overall_plot()
  })
  # information on plot
  output$pd_overall <- renderText({
    overall_info()
  })  
  

  # create plot from gender data
  gender_plot <- reactive({
    display_plot <- gen_mean_plot(ortho_sex(), user_value_df())  |> hide_legend()
    return(display_plot)
  })
  # display plot from gender data
  output$plot_gender <- renderPlotly({
    gender_plot() 
  })
  # information on plot
  output$pd_gender <- renderText({
    gender_info()
  })  
  
  # create plot from age data
  age_plot <- reactive({
    display_plot <- gen_mean_plot(ortho_age(), user_value_df()) |> hide_legend()
    return(display_plot)
  })
  # display plot from age data
  output$plot_age <- renderPlotly({
    age_plot() 
  })
  # information on plot
  output$pd_age <- renderText({
    age_info()
  })  
  
  # create plot from diagnosis data
  diag_plot <- reactive({
    display_plot <- gen_mean_plot(ortho_diag(), user_value_df()) |> hide_legend()
    return(display_plot)
  })
  # display plot from diagnosis data
  output$plot_diag <- renderPlotly({
    diag_plot()
  })
  # information on plot
  output$pd_diag <- renderText({
    diag_info()
  })  
  
  # create plot from combined data
  combined_plot <- reactive({
    display_plot <- gen_mean_plot(ortho_filtered(), user_value_df()) |> hide_legend()
    return(display_plot)
  })
  # display plot from combined data
  output$plot_combined <- renderPlotly({
    diag_plot()
  })
  # information on plot
  output$pd_combined <- renderText({
    combined_info()
  })  
  
  # trying things----
  message <- eventReactive(input$submit, {
    input$name
  })
  
  output$mess <- renderText({
    paste0("Hello, ", message())
  })
  
  
  
  # reactive----
  dataset1 <- reactive({
    get(input$dataset, "package:datasets")
  })
  
  output$summary <- renderPrint({
    summary(dataset1())
  })
  
  output$plot <- renderPlot({
    plot(dataset1())
  }, res = 96)
 
  
  # create data and functions for practice
  freqpoly <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
    df <- data.frame(
      x = c(x1, x2),
      g = c(rep("x1", length(x1)), rep("x2", length(x2)))
    )
    
    ggplot(df, aes(x, colour = g)) +
      geom_freqpoly(binwidth = binwidth, size = 1) +
      coord_cartesian(xlim = xlim)
  }
  
  t_test <- function(x1, x2) {
    test <- t.test(x1, x2)
    
    # use sprintf() to format t.test() results compactly
    sprintf(
      "p value: %0.3f\n[%0.2f, %0.2f]",
      test$p.value, test$conf.int[1], test$conf.int[2]
    )
  }
  
  # reactive events
  x1 <- eventReactive(input$simulate, {
    rpois(input$n, input$lambda1)
  })
  
  x2 <- eventReactive(input$simulate, {
    rpois(input$n, input$lambda2)
  })
  
  
  output$hist <- renderPlot({
    freqpoly(x1(), x2(), binwidth = 1, xlim = c(0, 40))
  }, res = 96)
  
  output$ttest <- renderText({
    t_test(x1(), x2())
  })
  
  
  
  
  
}
  






# Run the application 
shinyApp(ui = ui, server = server)
