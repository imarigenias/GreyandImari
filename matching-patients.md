<<<<<<< HEAD
## Matching patients

### Goal: Compare the MRNs from three datasoures:
1) Svjetlana's Data - "DBS_pts_10-21-21_GRS.xlsx"
3) Christine's Data - "Motion Analysis and DBS Programming Patient Panel (2014 - present).xlsx"
4) Redcap Data - Report "Deep Brain Stimulation Patients" from "Motion Analysis in Movement Disorders" 

### Basic Procedure:
1) Import the three datasources into R as dataframes
2) Create new dataframe with distince MRNs from all tree datasources in col1
3) If MRN exists in datasource


### We used the folliowing code:
<details>
  <summary>Click to show R code</summary>
  
  ```
  library(tidyverse)
=======
Matching Patients
================

# Goal 1: combine data from Redcapt with Svetlana's data
## Basic Procedure:
## We used the following code
```
  # load libraries
  library(tidyverse) # data cleaning
  library(motionTools)  # upload from redcap
  library(openxlsx)  # upload excel files
  
  # Upload 'Deep Brain Stimulation Report' from Redcap and subset columns
  redcap_data <- ReadRedcapReport(token = Sys.getenv("movementDisorders_redcap_token"), 
                                url = "https://redcap.emory.edu/api/", 
                                report_id = 32159) |>                            select(-c("tec_mrn":"deep_brain_stimulation_patients_complete"))
  
  # Upload Svetlana's data from excel and subset columns
    svetData <- read.xlsx("DBS_pts_10-21-21_GRS.xlsx") |> select(-c("record_id"))
    svetData$tec_mrn <- as.character(svetData$tec_mrn)  # refactor tec_mrn variable
    
    # Complete left join
    RC_SVET <- left_join(redcap_data, svetData, by=c("mrn" = "tec_mrn"))
  
  

```
# Goal 2: 
```
# necessary libraries
library(tidyverse)
>>>>>>> a86d5a200f86b91c8a2cc64580464f404a88711a
library(motionTools)
library(openxlsx)
library(tibble)
library(dplyr)

<<<<<<< HEAD
# import Svet's data and remove variable "record_id"
svetData <- read.xlsx("DBS_pts_10-21-21_GRS.xlsx")|> select(-c("record_id"))
=======
>>>>>>> a86d5a200f86b91c8a2cc64580464f404a88711a

# import only the MRN's from Esper's data
esperData <- read.xlsx("Motion Analysis and DBS Programming Patient Panel (2014 - present).xlsx", "DBS Programming") |> select(c("MRN"))

<<<<<<< HEAD
# import report "Deep Brain Stimulation Patients" from redcap
redcapDBS <- ReadRedcapReport(token = Sys.getenv("movementDisorders_redcap_token"), 
                                url = "https://redcap.emory.edu/api/", report_id = 32159)

# remove variables from redcapDBS that come from svetlana's data
redcapDBS <- redcapDBS |>  select(-c("tec_mrn":"deep_brain_stimulation_patients_complete"))
=======
>>>>>>> a86d5a200f86b91c8a2cc64580464f404a88711a

# create 1 col df of all MRNs in redcapDBS, svetData, esperData
MRNs <- append(append(c(redcapDBS$mrn),c(svetData$tec_mrn)),c(esperData$MRN))
combo <- as.data.frame(MRNs)
combo <-distinct(combo)
combo["Motion"] <- combo$MRNs %in% redcapDBS$mrn
combo["Svjet"] <- combo$MRNs %in% esperData$MRN
combo["Christine"] <- combo$MRNs %in% svetData$tec_mrn
<<<<<<< HEAD
combo <- combo %>% mutate("Svjet & Christine" = if_else(Svjet == Christine & Svjet==TRUE , TRUE, FALSE))
  ```
  </details>


### We created a dataframe with the following structure:

MRNs|Motion|Svjetlana|Christine|Svjet & Christine
---|---|---|---|---|
ABC| True | True | False | False|
DEF| True | True | True | True|
etc.

## Summarize the number of patients in each group

```{r}
=======
combo <- combo %>% mutate("Svjet_Christine" = if_else(Svjet == Christine & Svjet==TRUE , TRUE, FALSE))

# summary of col
  combo_summary <- combo |> summarise(Svet_in_redcap = sum(Svjet == "TRUE"),
                          Esper_in_redcap = sum(Christine == "TRUE"),
                          Svet_in_Esper = sum(Svjet_Christine))
  rownames(combo_summary)[1] <- "Num_patients"
>>>>>>> a86d5a200f86b91c8a2cc64580464f404a88711a

```

