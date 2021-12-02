Filling out missing data
================

# Objectives:

-   Import Redcap report:`Orthotrack Normative Data`
-   Fill missing data across rows
-   Create age categories for patients
-   Create summaries on Orthotrack variables by age
-   Create summaries on Orthotrack variables by age

# Libraries

Here are the libraries that we used, and what they are used for.

``` r
  library(tidyverse) # data cleaning
  library(motionTools)  # upload from redcap
  library(openxlsx)  # upload excel files
  library(arsenal)  # summary tables
```

# Upload report from Redcap

``` r
    oD <- ReadRedcapReport(token = Sys.getenv("movementDisorders_redcap_token"), 
                                url = "https://redcap.emory.edu/api/", report_id = 31113)
```

## Organize data

In this step, we created a new data frame from the original data frame
we created to import the Redcap report into R. We also remove the
variables that we don’t need, and change the `sex` variable to a factor
(0 = Male; 1 = Female).

``` r
    oD1 <- oD |> select(-c(redcap_repeat_instrument, redcap_repeat_instance, patient_fullpath_neurology, patient_folder_neurology, nrm_file, report_final_impression)) 
    # re-factor the sex variable
    oD1$sex <- factor(oD$sex, levels=c(0,1), labels=c("Male", "Female")) 
```

# Fill in missing info

``` r
     # view data set
    head(oD1)
```

    ## # A tibble: 6 x 26
    ##   record_id dw_icd dw_primary_diagnosis age_capture sex   step_leng_ave_rt
    ##       <dbl> <chr>  <chr>                      <dbl> <fct>            <dbl>
    ## 1         1 <NA>   <NA>                        NA   Male              NA  
    ## 2         1 <NA>   <NA>                         0.6 <NA>              NA  
    ## 3         1 <NA>   <NA>                        NA   <NA>              51.9
    ## 4         2 <NA>   <NA>                        NA   Male              NA  
    ## 5         2 <NA>   <NA>                        66.5 <NA>              NA  
    ## 6         2 <NA>   <NA>                        NA   <NA>              51.9
    ## # ... with 20 more variables: step_leng_ave_lft <dbl>, strid_leng_ave_rt <dbl>,
    ## #   strid_leng_ave_lft <dbl>, for_vel_rt <dbl>, for_vel_lft <dbl>,
    ## #   cad_ave_rt <dbl>, cad_ave_lft <dbl>, tot_sup_time_rt <dbl>,
    ## #   tot_sup_time_lft <dbl>, swing_phas_rt <dbl>, swing_phas_lft <dbl>,
    ## #   int_2x_sup_time_rt <dbl>, int_2x_sup_time_lft <dbl>,
    ## #   single_sup_time_rt <dbl>, single_sup_time_lft <dbl>, step_width <dbl>,
    ## #   num_steps_rt <dbl>, num_steps_lft <dbl>, num_strides_rt <dbl>, ...

In the original data set, there are duplicates of the patients on each
row. Some rows contain values for some variables, and not others. The
goal is to fill in the missing data across the columns for each patient.

## Fill in gaps

``` r
    oD2 <- oD1 |> group_by(record_id) |> fill(names(oD1)[-1], .direction = "updown")
```

First the data set is grouped by `record_id`. The argument for `fill()`
is `names(oD1)[-1]`. This gets the names of the columns in the data set,
except the first (`record_id`). These are the variables that need to be
filled.  
The argument `.direction` species where R should look to fill in the
data. “updown” tells R to look up and down, because not all the
information is obtained in the top row.

## Subset the data

``` r
    oD3 <-  distinct(oD2, record_id, .keep_all = T)
```

After filling in the missing information, we have repeat observations
for each patient. We only want one for each, so we just select each
distinct observation from the data set.

# Creating age categories

We are going to create a categorical variable that takes the ages from
the `age_capture` variable and creates age categories in 1 decade bins
(e.g. “0-10, 10-20, etc.”)

``` r
    # new dataset
    oD4 <- oD3 
    
    # create age group variable
    oD4$age_cat <- cut(oD4$age_capture, seq(0, 100, by=10), labels=c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"), include.lowest=T)
    
    # change order of the variable
    oD4 <- oD4 |> relocate(age_cat, .after = age_capture)
```

## Summary by age

Now we use the categories we made in the previous step to get a summary
(mean, standard deviation, range) of each Orthotrack variable.

``` r
    # obtain name of all the numeric variables
    numeric_columns <- names(select_if(oD4, is.numeric))

    # paste the values together separated by '+' 
    # [3:22] only select Orthotrack variables
    nc_values <- paste(numeric_columns[3:22], collapse = "+")
    
    # summary by age
    summary(tableby(as.formula(paste('age_cat ~', nc_values)), data=oD4))
```

    ## 
    ## 
    ## |                            |    0-10 (N=4)    |    10-20 (N=3)    |   20-30 (N=15)   |   30-40 (N=21)   |   40-50 (N=45)   |  50-60 (N=117)   |  60-70 (N=240)   |  70-80 (N=249)   |   80-90 (N=45)   |   90-100 (N=2)    |  Total (N=741)   | p value|
    ## |:---------------------------|:----------------:|:-----------------:|:----------------:|:----------------:|:----------------:|:----------------:|:----------------:|:----------------:|:----------------:|:-----------------:|:----------------:|-------:|
    ## |**step_leng_ave_rt**        |                  |                   |                  |                  |                  |                  |                  |                  |                  |                   |                  |   0.035|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        2         |         0         |        2         |        3         |        11        |        21        |        47        |        50        |        8         |         1         |       145        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  45.584 (8.976)  |  63.202 (4.655)   |  53.389 (7.523)  | 53.858 (13.431)  | 51.878 (16.325)  | 48.288 (16.789)  | 48.243 (13.063)  | 47.466 (14.062)  | 41.871 (13.728)  |    44.825 (NA)    | 48.145 (14.309)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 39.237 - 51.931  |  58.321 - 67.592  | 38.909 - 62.463  | 22.646 - 75.193  | 10.703 - 80.709  |  0.672 - 80.497  |  6.107 - 75.474  |  4.369 - 87.433  |  7.180 - 70.802  |  44.825 - 44.825  |  0.672 - 87.433  |        |
    ## |**step_leng_ave_lft**       |                  |                   |                  |                  |                  |                  |                  |                  |                  |                   |                  |   0.006|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        2         |         0         |        2         |        3         |        11        |        22        |        47        |        50        |        8         |         1         |       146        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  39.343 (1.921)  |  65.961 (6.313)   |  51.115 (8.453)  | 54.095 (11.040)  | 52.672 (15.055)  | 48.923 (16.553)  | 48.257 (13.250)  | 47.323 (14.354)  | 40.966 (13.744)  |    43.296 (NA)    | 48.140 (14.345)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 37.984 - 40.701  |  60.571 - 72.906  | 27.563 - 60.338  | 34.864 - 75.047  | 13.281 - 77.009  | -1.559 - 76.649  |  6.970 - 73.900  |  1.029 - 87.201  | 12.301 - 71.137  |  43.296 - 43.296  | -1.559 - 87.201  |        |
    ## |**strid_leng_ave_rt**       |                  |                   |                  |                  |                  |                  |                  |                  |                  |                   |                  |   0.013|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        2         |         0         |        2         |        3         |        11        |        21        |        47        |        50        |        8         |         1         |       145        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  84.892 (7.307)  | 129.272 (11.760)  | 104.958 (14.542) | 108.028 (24.534) | 104.875 (30.794) | 97.143 (33.032)  | 96.632 (25.804)  | 94.886 (28.033)  | 83.077 (26.991)  |    88.593 (NA)    | 96.397 (28.231)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 79.725 - 90.058  | 118.856 - 142.025 | 70.946 - 123.695 | 55.917 - 150.973 | 23.680 - 157.471 | 9.688 - 156.445  | 19.994 - 148.916 | 5.021 - 175.592  | 23.529 - 142.753 |  88.593 - 88.593  | 5.021 - 175.592  |        |
    ## |**strid_leng_ave_lft**      |                  |                   |                  |                  |                  |                  |                  |                  |                  |                   |                  |   0.015|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        2         |         0         |        2         |        3         |        11        |        22        |        47        |        50        |        8         |         1         |       146        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  85.138 (7.436)  | 127.760 (11.244)  | 104.762 (15.173) | 107.924 (23.995) | 104.580 (30.896) | 97.481 (32.877)  | 96.651 (25.933)  | 94.880 (28.139)  | 82.851 (27.252)  |    87.879 (NA)    | 96.408 (28.273)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 79.880 - 90.396  | 118.722 - 140.351 | 67.669 - 121.958 | 60.288 - 150.792 | 23.409 - 157.843 | 9.914 - 156.745  | 17.447 - 149.877 | 5.303 - 174.382  | 23.053 - 142.070 |  87.879 - 87.879  | 5.303 - 174.382  |        |
    ## |**for_vel_rt**              |                  |                   |                  |                  |                  |                  |                  |                  |                  |                   |                  |   0.123|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        2         |         0         |        2         |        3         |        11        |        21        |        47        |        50        |        8         |         1         |       145        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  72.486 (2.881)  | 117.770 (22.952)  | 90.667 (23.557)  | 96.064 (33.294)  | 89.709 (35.772)  | 87.752 (34.339)  | 85.128 (27.558)  | 84.606 (29.112)  | 73.456 (28.864)  |    74.494 (NA)    | 85.468 (30.042)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 70.448 - 74.523  | 102.846 - 144.199 | 37.990 - 135.588 | 26.251 - 161.201 | 5.873 - 145.602  | 4.199 - 156.788  | 5.465 - 156.275  | 3.889 - 165.626  | 19.253 - 129.749 |  74.494 - 74.494  | 3.889 - 165.626  |        |
    ## |**for_vel_lft**             |                  |                   |                  |                  |                  |                  |                  |                  |                  |                   |                  |   0.128|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        2         |         0         |        2         |        3         |        11        |        22        |        47        |        50        |        8         |         1         |       146        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  72.385 (3.521)  | 116.125 (22.919)  | 90.629 (23.707)  | 95.981 (33.286)  | 89.642 (35.893)  | 88.174 (34.317)  | 85.293 (27.652)  | 84.633 (29.131)  | 73.356 (28.957)  |    73.801 (NA)    | 85.571 (30.077)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 69.895 - 74.874  | 102.741 - 142.589 | 37.729 - 136.223 | 26.088 - 160.809 | 5.296 - 146.004  | 4.238 - 160.221  | 5.097 - 157.953  | 3.681 - 163.539  | 17.937 - 129.343 |  73.801 - 73.801  | 3.681 - 163.539  |        |
    ## |**cad_ave_rt**              |                  |                   |                  |                  |                  |                  |                  |                  |                  |                   |                  |   0.577|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        2         |         0         |        2         |        3         |        11        |        21        |        47        |        50        |        8         |         1         |       145        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) | 103.254 (13.572) | 109.197 (12.263)  | 102.018 (17.807) | 104.594 (21.002) | 97.731 (23.145)  | 105.753 (17.495) | 104.885 (17.525) | 106.025 (15.517) | 104.498 (16.417) |   100.632 (NA)    | 104.911 (17.255) |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 93.657 - 112.850 | 101.169 - 123.312 | 61.111 - 135.883 | 55.388 - 136.264 | 30.363 - 126.642 | 26.378 - 142.125 | 39.217 - 182.091 | 64.545 - 167.564 | 71.180 - 150.929 | 100.632 - 100.632 | 26.378 - 182.091 |        |
    ## |**cad_ave_lft**             |                  |                   |                  |                  |                  |                  |                  |                  |                  |                   |                  |   0.584|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        2         |         0         |        2         |        3         |        11        |        22        |        47        |        50        |        8         |         1         |       146        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) | 103.725 (12.098) | 109.170 (12.339)  | 101.832 (17.711) | 104.261 (21.065) | 97.723 (23.180)  | 105.816 (17.622) | 104.832 (17.642) | 105.943 (15.573) | 104.357 (16.481) |   100.443 (NA)    | 104.853 (17.335) |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 95.170 - 112.279 | 101.050 - 123.369 | 62.195 - 136.383 | 53.146 - 136.185 | 29.639 - 126.885 | 25.660 - 141.996 | 33.413 - 180.457 | 63.913 - 167.808 | 70.178 - 149.121 | 100.443 - 100.443 | 25.660 - 180.457 |        |
    ## |**tot_sup_time_rt**         |                  |                   |                  |                  |                  |                  |                  |                  |                  |                   |                  |   0.591|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        2         |         0         |        2         |        3         |        11        |        21        |        47        |        50        |        8         |         1         |       145        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  68.352 (0.534)  |  63.123 (0.598)   |  63.311 (3.760)  |  65.007 (5.648)  |  65.590 (6.963)  |  64.995 (5.048)  |  64.778 (3.669)  |  64.979 (3.656)  |  65.953 (3.664)  |    62.183 (NA)    |  64.973 (4.217)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 67.975 - 68.730  |  62.607 - 63.779  | 59.538 - 72.956  | 59.192 - 80.092  | 58.958 - 93.536  | 57.207 - 87.065  | 55.134 - 85.227  | 59.399 - 86.332  | 59.088 - 75.529  |  62.183 - 62.183  | 55.134 - 93.536  |        |
    ## |**tot_sup_time_lft**        |                  |                   |                  |                  |                  |                  |                  |                  |                  |                   |                  |   0.693|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        2         |         0         |        2         |        3         |        11        |        22        |        47        |        50        |        8         |         1         |       146        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  65.627 (3.601)  |  63.078 (0.412)   |  63.933 (4.498)  |  63.488 (3.175)  |  65.391 (7.214)  |  65.234 (5.408)  |  64.664 (3.959)  |  64.868 (4.012)  |  65.795 (3.693)  |    61.743 (NA)    |  64.874 (4.440)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 63.081 - 68.174  |  62.793 - 63.551  | 59.220 - 77.334  | 58.877 - 70.099  | 58.468 - 93.417  | 56.537 - 89.930  | 52.493 - 85.345  | 52.500 - 85.936  | 59.729 - 73.306  |  61.743 - 61.743  | 52.493 - 93.417  |        |
    ## |**swing_phas_rt**           |                  |                   |                  |                  |                  |                  |                  |                  |                  |                   |                  |   0.591|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        2         |         0         |        2         |        3         |        11        |        21        |        47        |        50        |        8         |         1         |       145        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  31.648 (0.534)  |  36.877 (0.598)   |  36.689 (3.759)  |  34.993 (5.648)  |  34.410 (6.963)  |  35.005 (5.048)  |  35.222 (3.669)  |  35.021 (3.656)  |  34.047 (3.664)  |    37.817 (NA)    |  35.027 (4.217)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 31.270 - 32.025  |  36.221 - 37.393  | 27.044 - 40.462  | 19.908 - 40.808  |  6.464 - 41.042  | 12.935 - 42.793  | 14.773 - 44.866  | 13.668 - 40.601  | 24.470 - 40.912  |  37.817 - 37.817  |  6.464 - 44.866  |        |
    ## |**swing_phas_lft**          |                  |                   |                  |                  |                  |                  |                  |                  |                  |                   |                  |   0.693|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        2         |         0         |        2         |        3         |        11        |        22        |        47        |        50        |        8         |         1         |       146        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  34.373 (3.601)  |  36.922 (0.412)   |  36.067 (4.498)  |  36.512 (3.175)  |  34.609 (7.214)  |  34.766 (5.408)  |  35.336 (3.959)  |  35.132 (4.012)  |  34.205 (3.693)  |    38.257 (NA)    |  35.126 (4.440)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 31.826 - 36.919  |  36.449 - 37.207  | 22.666 - 40.780  | 29.901 - 41.123  |  6.583 - 41.532  | 10.070 - 43.463  | 14.655 - 47.507  | 14.064 - 47.500  | 26.694 - 40.271  |  38.257 - 38.257  |  6.583 - 47.507  |        |
    ## |**int_2x_sup_time_rt**      |                  |                   |                  |                  |                  |                  |                  |                  |                  |                   |                  |   0.715|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        2         |         0         |        2         |        3         |        11        |        21        |        47        |        50        |        8         |         1         |       145        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  16.910 (2.410)  |  13.431 (0.993)   |  14.015 (3.073)  |  14.093 (4.001)  |  15.836 (8.315)  |  14.904 (4.655)  |  14.804 (3.559)  |  15.032 (3.886)  |  15.825 (3.068)  |    11.435 (NA)    |  14.974 (4.222)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 15.206 - 18.614  |  12.332 - 14.264  |  9.812 - 20.755  |  8.575 - 25.012  |  9.006 - 50.074  |  8.682 - 39.591  |  6.852 - 35.880  |  3.696 - 35.839  | 10.280 - 22.575  |  11.435 - 11.435  |  3.696 - 50.074  |        |
    ## |**int_2x_sup_time_lft**     |                  |                   |                  |                  |                  |                  |                  |                  |                  |                   |                  |   0.794|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        2         |         0         |        2         |        3         |        11        |        22        |        47        |        50        |        8         |         1         |       146        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  16.345 (2.584)  |  13.077 (0.821)   |  13.570 (5.396)  |  14.402 (4.073)  |  15.493 (6.171)  |  15.389 (5.934)  |  14.993 (4.115)  |  15.002 (3.902)  |  16.054 (4.282)  |    12.394 (NA)    |  15.095 (4.540)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 14.517 - 18.172  |  12.292 - 13.930  |  7.546 - 30.075  |  9.243 - 22.320  |  9.898 - 42.349  |  8.665 - 44.591  |  9.536 - 43.361  |  8.923 - 39.103  |  8.763 - 28.934  |  12.394 - 12.394  |  7.546 - 44.591  |        |
    ## |**single_sup_time_rt**      |                  |                   |                  |                  |                  |                  |                  |                  |                  |                   |                  |   0.707|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        2         |         0         |        2         |        3         |        11        |        21        |        47        |        50        |        8         |         1         |       145        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  34.373 (3.601)  |  36.922 (0.412)   |  36.067 (4.498)  |  36.512 (3.175)  |  34.609 (7.214)  |  34.811 (5.397)  |  35.336 (3.959)  |  35.132 (4.012)  |  34.205 (3.693)  |    38.257 (NA)    |  35.133 (4.439)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 31.826 - 36.919  |  36.449 - 37.207  | 22.666 - 40.780  | 29.901 - 41.123  |  6.583 - 41.532  | 10.070 - 43.463  | 14.655 - 47.507  | 14.064 - 47.500  | 26.694 - 40.271  |  38.257 - 38.257  |  6.583 - 47.507  |        |
    ## |**single_sup_time_lft**     |                  |                   |                  |                  |                  |                  |                  |                  |                  |                   |                  |   0.587|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        2         |         0         |        2         |        3         |        11        |        22        |        47        |        50        |        8         |         1         |       146        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  31.648 (0.534)  |  36.877 (0.598)   |  36.689 (3.759)  |  34.993 (5.648)  |  34.410 (6.963)  |  35.086 (5.012)  |  35.222 (3.669)  |  35.021 (3.656)  |  34.047 (3.664)  |    37.817 (NA)    |  35.039 (4.208)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 31.270 - 32.025  |  36.221 - 37.393  | 27.044 - 40.462  | 19.908 - 40.808  |  6.464 - 41.042  | 12.935 - 42.793  | 14.773 - 44.866  | 13.668 - 40.601  | 24.470 - 40.912  |  37.817 - 37.817  |  6.464 - 44.866  |        |
    ## |**step_width**              |                  |                   |                  |                  |                  |                  |                  |                  |                  |                   |                  |   0.002|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        2         |         0         |        2         |        3         |        11        |        21        |        47        |        50        |        8         |         1         |       145        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  14.537 (0.378)  |  12.349 (3.438)   |  13.609 (5.540)  |  14.410 (4.736)  |  15.341 (6.926)  |  12.807 (4.020)  |  12.176 (3.835)  |  12.035 (3.450)  |  12.323 (3.335)  |    11.954 (NA)    |  12.527 (4.081)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 14.270 - 14.804  |  8.393 - 14.614   |  7.073 - 26.793  |  7.500 - 24.228  |  7.164 - 38.690  |  3.026 - 23.988  |  5.446 - 26.880  |  4.360 - 21.562  |  7.332 - 20.774  |  11.954 - 11.954  |  3.026 - 38.690  |        |
    ## |**num_steps_rt**            |                  |                   |                  |                  |                  |                  |                  |                  |                  |                   |                  |   0.951|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        2         |         0         |        2         |        3         |        11        |        21        |        47        |        50        |        8         |         1         |       145        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  12.000 (0.000)  |  15.000 (4.359)   |  13.154 (3.760)  |  12.278 (4.226)  |  13.618 (4.383)  |  14.094 (4.976)  |  13.658 (4.945)  |  13.774 (4.222)  |  13.649 (4.244)  |    15.000 (NA)    |  13.715 (4.572)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 12.000 - 12.000  |  10.000 - 18.000  |  7.000 - 21.000  |  8.000 - 24.000  |  7.000 - 23.000  |  5.000 - 32.000  |  6.000 - 51.000  |  5.000 - 26.000  |  6.000 - 25.000  |  15.000 - 15.000  |  5.000 - 51.000  |        |
    ## |**num_steps_lft**           |                  |                   |                  |                  |                  |                  |                  |                  |                  |                   |                  |   0.585|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        2         |         0         |        2         |        3         |        11        |        21        |        47        |        50        |        8         |         1         |       145        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  12.000 (1.414)  |  13.667 (4.933)   |  12.923 (4.349)  |  11.333 (4.366)  |  13.941 (4.880)  |  14.188 (4.758)  |  13.580 (4.812)  |  13.774 (4.137)  |  13.946 (4.766)  |    17.000 (NA)    |  13.705 (4.555)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 11.000 - 13.000  |  8.000 - 17.000   |  6.000 - 23.000  |  6.000 - 24.000  |  7.000 - 23.000  |  4.000 - 30.000  |  6.000 - 50.000  |  5.000 - 26.000  |  7.000 - 27.000  |  17.000 - 17.000  |  4.000 - 50.000  |        |
    ## |**num_strides_rt**          |                  |                   |                  |                  |                  |                  |                  |                  |                  |                   |                  |   0.786|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        2         |         0         |        2         |        3         |        11        |        21        |        47        |        50        |        8         |         1         |       145        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  11.500 (0.707)  |  13.667 (4.933)   |  11.769 (4.381)  |  11.000 (4.298)  |  12.941 (4.519)  |  13.292 (4.832)  |  12.855 (4.782)  |  13.025 (4.103)  |  13.135 (4.198)  |    15.000 (NA)    |  12.928 (4.482)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 11.000 - 12.000  |  8.000 - 17.000   |  5.000 - 21.000  |  6.000 - 24.000  |  7.000 - 23.000  |  4.000 - 30.000  |  5.000 - 50.000  |  5.000 - 26.000  |  6.000 - 25.000  |  15.000 - 15.000  |  4.000 - 50.000  |        |

## Summary by sex

Now we use part of the code in the previous step to obtain summaries of
the Orthotrack variables by sex.

``` r
    # create summary of results by sex
    summary(tableby(as.formula(paste('sex ~', nc_values)), data=oD4))
```

    ## 
    ## 
    ## |                            |   Male (N=451)   |  Female (N=298)  |  Total (N=749)   | p value|
    ## |:---------------------------|:----------------:|:----------------:|:----------------:|-------:|
    ## |**step_leng_ave_rt**        |                  |                  |                  | < 0.001|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        86        |        60        |       146        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) | 50.263 (14.395)  | 44.935 (13.186)  | 48.160 (14.161)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     |  1.368 - 87.433  |  0.672 - 70.848  |  0.672 - 87.433  |        |
    ## |**step_leng_ave_lft**       |                  |                  |                  | < 0.001|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        87        |        60        |       147        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) | 50.702 (14.051)  | 44.265 (13.563)  | 48.157 (14.203)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     |  3.468 - 87.201  | -1.559 - 72.906  | -1.559 - 87.201  |        |
    ## |**strid_leng_ave_rt**       |                  |                  |                  | < 0.001|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        86        |        60        |       146        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) | 101.020 (28.052) | 89.386 (26.315)  | 96.428 (27.943)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 18.149 - 175.592 | 5.021 - 144.297  | 5.021 - 175.592  |        |
    ## |**strid_leng_ave_lft**      |                  |                  |                  | < 0.001|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        87        |        60        |       147        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) | 101.095 (28.050) | 89.323 (26.382)  | 96.441 (27.980)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 17.308 - 174.382 | 5.303 - 143.127  | 5.303 - 174.382  |        |
    ## |**for_vel_rt**              |                  |                  |                  | < 0.001|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        86        |        60        |       146        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) | 89.866 (28.962)  | 79.154 (29.992)  | 85.638 (29.812)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 4.199 - 165.626  | 3.889 - 145.591  | 3.889 - 165.626  |        |
    ## |**for_vel_lft**             |                  |                  |                  | < 0.001|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        87        |        60        |       147        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) | 90.059 (28.991)  | 79.140 (29.965)  | 85.742 (29.838)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 4.238 - 163.539  | 3.681 - 145.641  | 3.681 - 163.539  |        |
    ## |**cad_ave_rt**              |                  |                  |                  |   0.104|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        86        |        60        |       146        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) | 106.036 (17.476) | 103.684 (17.057) | 105.108 (17.336) |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 26.378 - 182.091 | 30.363 - 145.275 | 26.378 - 182.091 |        |
    ## |**cad_ave_lft**             |                  |                  |                  |   0.094|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        87        |        60        |       147        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) | 106.014 (17.507) | 103.583 (17.191) | 105.053 (17.409) |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 25.660 - 180.457 | 29.639 - 144.505 | 25.660 - 180.457 |        |
    ## |**tot_sup_time_rt**         |                  |                  |                  |   0.002|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        86        |        60        |       146        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  64.525 (3.832)  |  65.576 (4.534)  |  64.940 (4.152)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 55.134 - 87.065  | 59.399 - 93.536  | 55.134 - 93.536  |        |
    ## |**tot_sup_time_lft**        |                  |                  |                  | < 0.001|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        87        |        60        |       147        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  64.344 (3.887)  |  65.592 (4.992)  |  64.837 (4.396)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 52.493 - 89.930  | 55.155 - 93.417  | 52.493 - 93.417  |        |
    ## |**swing_phas_rt**           |                  |                  |                  |   0.002|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        86        |        60        |       146        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  35.475 (3.832)  |  34.424 (4.534)  |  35.060 (4.152)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 12.935 - 44.866  |  6.464 - 40.601  |  6.464 - 44.866  |        |
    ## |**swing_phas_lft**          |                  |                  |                  | < 0.001|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        87        |        60        |       147        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  35.656 (3.887)  |  34.408 (4.992)  |  35.163 (4.396)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 10.070 - 47.507  |  6.583 - 44.845  |  6.583 - 47.507  |        |
    ## |**int_2x_sup_time_rt**      |                  |                  |                  |   0.002|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        86        |        60        |       146        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  14.512 (3.887)  |  15.599 (4.491)  |  14.941 (4.166)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     |  3.696 - 39.591  |  8.927 - 50.074  |  3.696 - 50.074  |        |
    ## |**int_2x_sup_time_lft**     |                  |                  |                  | < 0.001|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        87        |        60        |       147        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  14.547 (3.780)  |  15.843 (5.317)  |  15.059 (4.493)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     |  7.546 - 43.361  |  9.530 - 44.591  |  7.546 - 44.591  |        |
    ## |**single_sup_time_rt**      |                  |                  |                  | < 0.001|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        86        |        60        |       146        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  35.665 (3.886)  |  34.408 (4.992)  |  35.169 (4.395)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 10.070 - 47.507  |  6.583 - 44.845  |  6.583 - 47.507  |        |
    ## |**single_sup_time_lft**     |                  |                  |                  |   0.002|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        87        |        60        |       147        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  35.497 (3.813)  |  34.424 (4.534)  |  35.073 (4.143)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     | 12.935 - 44.866  |  6.464 - 40.601  |  6.464 - 44.866  |        |
    ## |**step_width**              |                  |                  |                  |   0.053|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        86        |        60        |       146        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  12.811 (4.030)  |  12.146 (4.255)  |  12.548 (4.129)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     |  3.026 - 31.262  |  4.360 - 38.690  |  3.026 - 38.690  |        |
    ## |**num_steps_rt**            |                  |                  |                  |   0.018|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        86        |        60        |       146        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  13.364 (4.747)  |  14.261 (4.164)  |  13.718 (4.544)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     |  5.000 - 51.000  |  5.000 - 30.000  |  5.000 - 51.000  |        |
    ## |**num_steps_lft**           |                  |                  |                  |   0.030|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        86        |        60        |       146        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  13.397 (4.656)  |  14.214 (4.284)  |  13.720 (4.527)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     |  4.000 - 50.000  |  5.000 - 35.000  |  4.000 - 50.000  |        |
    ## |**num_strides_rt**          |                  |                  |                  |   0.036|
    ## |&nbsp;&nbsp;&nbsp;N-Miss    |        86        |        60        |       146        |        |
    ## |&nbsp;&nbsp;&nbsp;Mean (SD) |  12.644 (4.664)  |  13.420 (4.086)  |  12.950 (4.457)  |        |
    ## |&nbsp;&nbsp;&nbsp;Range     |  4.000 - 50.000  |  5.000 - 30.000  |  4.000 - 50.000  |        |

# Save data

In this step, we are going to save the file as an Rdata file, so we can
use it in R shiny.

``` r
  data.save <- file.path("./Esper_Orthotrack/data")
  saveRDS(oD4, file = file.path(data.save, "ortho_data.rds"))
```

# Ignore

If for some reason we wanted to extract the values for the Orthotrack
variables individually

``` r
    sapply(strsplit(numeric_columns, "_"), "[[", 1) # first part of variable
```

    ##  [1] "record" "age"    "step"   "step"   "strid"  "strid"  "for"    "for"   
    ##  [9] "cad"    "cad"    "tot"    "tot"    "swing"  "swing"  "int"    "int"   
    ## [17] "single" "single" "step"   "num"    "num"    "num"    "num"

``` r
    sapply(strsplit(numeric_columns, "_"), "[[", 2) # second part of variable
```

    ##  [1] "id"      "capture" "leng"    "leng"    "leng"    "leng"    "vel"    
    ##  [8] "vel"     "ave"     "ave"     "sup"     "sup"     "phas"    "phas"   
    ## [15] "2x"      "2x"      "sup"     "sup"     "width"   "steps"   "steps"  
    ## [22] "strides" "strides"

``` r
    unique(paste(sapply(strsplit(numeric_columns, "_"), "[[", 1), "_", sapply(strsplit(numeric_columns, "_"), "[[", 2), sep=""))
```

    ##  [1] "record_id"   "age_capture" "step_leng"   "strid_leng"  "for_vel"    
    ##  [6] "cad_ave"     "tot_sup"     "swing_phas"  "int_2x"      "single_sup" 
    ## [11] "step_width"  "num_steps"   "num_strides"
