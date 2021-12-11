library(dplyr)
library(kableExtra)
library(tidyverse)
library(readxl)
library(kableExtra)
library(scales)
library(plotly)

# import necessecary files----
data.src <- file.path("./Esper_Orthotrack/data")
# data frame of esper data
ortho_data <- readRDS(file= file.path(data.src,"ortho_data.Rds"))
# labels of ortho metric variable and English expressions
ortho_label <- read_excel(path=file.path(data.src, "ortho_labels.xlsx"), sheet = 1)
# match icd codes to disease name 
diag_label <- read_excel(path=file.path(data.src, "ortho_labels.xlsx"), sheet = 2)



# create summary table----
## Goal: create data frame with mean and sd of each orthotrack metric
gen_sum <- function(data_set){
  # un-group data and select ortho track variables
  ortho_stats <- data_set |> ungroup() |> select(c(step_leng_ave_rt:num_strides_lft)) |> 
    # convert data frame from wide to long; group by key = variable
    gather(factor_key = T) |>  group_by(key)  |> 
    # create summary statistics of each variable 
    summarise(mean= mean(value, na.rm=T), sd=sd(value, na.rm = T)) |> 
    # rename key to ortho_var
    rename(Metric=key)
  # change display of variables
  ortho_stats$Metric <- ifelse(ortho_stats$Metric == ortho_label$ortho_var, 
                               ortho_label$desc_name, ortho_stats$Metric)
  
  return(ortho_stats)
}

# create percentiles----
## Goal: take user input and calculate it's percentile based on the mean and sd 
gen_pctls <- function(ortho_data_set, user_ortho_values){
  # user_input to summary stats data frame for that data set 
  pctl_df <- bind_cols(ortho_data_set, user_ortho_values) |> 
    # adds percentile column
    mutate(Percentile = (percent(round(pnorm(q=user_input, mean = mean, sd=sd), 2)))) |> 
    # removes the column from user input
    select(-c(user_input))
  
  # return plot
  return(pctl_df)
}

# generate sample values as user input
user_input <- data.frame(user_input = sample(14:100, 21, replace=TRUE)) # example data






# sample code: Kable Extra----
ortho_stats <- gen_sum(ortho_data)
fem_stats <- ortho_data |> filter(sex == "Female") |> gen_sum()
diag_stats <- ortho_data |> filter(dw_icd == "G20") |> gen_sum()


group_data <- data.frame(ortho_stats, fem_stats[,c("mean", "sd")], diag_stats[,c("mean", "sd")], check.names = F)

header_group_data <- kable(group_data) |> kable_styling("striped") |> add_header_above(c(" " = 1, "Group 1" = 2, "Group 2" = 2, "Group 3" = 2))


# separate right and left (two columns)----
# generate average of each metric 
ortho_mean <- ortho_data |> ungroup() |> 
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
# set factor to keep order
ortho_mean$prefix <- factor(ortho_mean$prefix, levels = rev(unique(ortho_mean$prefix)), ordered = T)
ortho_mean$side <- factor(ortho_mean$side, levels = c("left", "right"))


# separate left and right-----
## Goal create data frame that stores user_value/mean for each metric 

# generate average of each metric 
ortho_mean <- ortho_data |> ungroup() |> 
  # select ortho variables
  select(c(step_leng_ave_rt:num_strides_lft)) |> 
  # convert data frame from wide to long; group by key = variabl
  gather(factor_key = T) |>  group_by(key) |>  
  # calculate mean of each data set 
  summarise(mean= mean(value, na.rm=T)) |> 
  # rename key variable to metric 
  rename(Metric= key)

# add left/right category; NA for step width 
ortho_mean$side <- ifelse(str_detect(ortho_mean$Metric, "_rt"), "right",
                                     ifelse(str_detect(ortho_mean$Metric, "_lft"), "left",
                                            NA))

# change Metric name
# obtain variable names
ortho_vars <- unique(ortho_mean$Metric)
# extract relevant part of variable description
metric_desc <- sapply(strsplit(ortho_label$desc_name, "-"), "[[",1)
# remove the "Average" part of the name and obtain the stripped description
prefix <- data.frame(prefix = sapply(ifelse(str_detect(metric_desc, "Average"),strsplit(metric_desc, " Average "), str_trim(metric_desc, side = "both")),"[[",1))
# combine statistics, descriptions and user input 
ortho_mean <- bind_cols(ortho_mean, prefix, user_input)
# find user_value/mean
ortho_mean$mean_multiple <- round(ortho_mean$user_input/ortho_mean$mean, 2)
ortho_mean$mm_scaled <- ifelse(ortho_mean$mean_multiple < 1, ortho_mean$mean_multiple * -1, ortho_mean$mean_multiple)
# set factor to keep order
ortho_mean$prefix <- factor(ortho_mean$prefix, levels = rev(unique(ortho_mean$prefix)), ordered = T)
ortho_mean$side <- factor(ortho_mean$side, levels = c("left", "right"))

# plot of value/mean
ggplot(ortho_mean, aes(prefix, mm_scaled, fill = side)) + 
  geom_bar(position = "dodge", stat = "identity") + coord_flip() + 
  guides(fill = guide_legend(reverse = F)) + theme_minimal() + xlab("Metric") + ylab("Value/Average") +
  scale_fill_manual("legend", values = c("right" = "blue", "left" = "green", "NA" = "grey")) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) 
  

# generate data set to be plotted
mean_multiple_plot <- function(ortho_data_set, user_input){
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
  # set factor to keep order
  ortho_mean$prefix <- factor(ortho_mean$prefix, levels = rev(unique(ortho_mean$prefix)), ordered = T)
  ortho_mean$side <- factor(ortho_mean$side, levels = c("left", "right"))
  
  # generate bar plot
  plot <- ggplot(ortho_mean, aes(prefix, mean_multiple, fill = side)) + 
    geom_bar(position = "dodge", stat = "identity") + coord_flip() + 
    guides(fill = guide_legend(reverse = T)) + 
    xlab("Metric") + ylab("Value/Average") +
    scale_y_continuous(labels = scales::percent_format(scale = 100))
  
  return(plot)
  
}

## plotly version of plot
plot <- plot_ly(ortho_mean,
               y = ~prefix,
               x = ~mean_multiple,
               type = 'bar',
               split = ~side,
               orientation = 'h'
               )





# generate plot----
ggplot(ortho_mean, aes(prefix, mean, fill = side)) + geom_col(position = "dodge") + coord_flip()


ggplot(ortho_mean, aes(prefix, mean, fill = side)) + 
  geom_bar(position = "dodge", stat = "identity") + coord_flip() + 
  guides(fill = guide_legend(reverse = F)) + theme_minimal() + xlab("Metric") + ylab("Eventually, value/pop_avg") +
  scale_fill_manual("legend", values = c("right" = "blue", "left" = "green"), na.values = "grey")
# may not have to set theme if it follows the default theme
# if I manually set the fill_manual, then I don't need to set the `fill = guide_legend`
# if colors are different in shiny output, change this 

# Add section to graph specific value divided by the average
