Matching Patients Imari
================

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

# Left join datasets

## Load libraries

``` r
  library(tidyverse) # data cleaning
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.5     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.0.2     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
  library(motionTools)  # upload from redcap
```

    ## ------------------------------------------------------------------------------

    ## motionTools is maintained by lucas@dbmi.emory.edu

    ## ------------------------------------------------------------------------------

``` r
  library(openxlsx)  # upload excel files
```

## Upload report from Redcap

``` r
    redcap_data <- ReadRedcapReport(token = Sys.getenv("movementDisorders_redcap_token"), 
                                url = "https://redcap.emory.edu/api/", report_id = 32159)
```

    ## Rows: 953 Columns: 17

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (1): mrn
    ## dbl  (3): record_id, dw_mrn, deep_brain_stimulation_patients_complete
    ## lgl (13): redcap_repeat_instrument, redcap_repeat_instance, tec_mrn, euh_mrn...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

### Subset relevant columns

``` r
    redcap_data <- redcap_data |>  select(-c("tec_mrn":"deep_brain_stimulation_patients_complete"))
```

## Upload data from excel

``` r
    svetData <- read.xlsx("DBS_pts_10-21-21_GRS.xlsx") 
```

### Edit data frame file

``` r
    svetData <- svetData |> select(-c("record_id"))
    svetData$tec_mrn <- as.character(svetData$tec_mrn)  # refactor tec_mrn variable
```

## Complete left join

``` r
    leftJoin <- left_join(redcap_data, svetData, by=c("mrn" = "tec_mrn"))
```

## Including Code

You can include R code in the document as follows:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](matching-patients_Imari_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
