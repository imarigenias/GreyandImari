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
library(motionTools)
library(openxlsx)
library(tibble)
library(dplyr)

# import Svet's data and remove variable "record_id"
svetData <- read.xlsx("DBS_pts_10-21-21_GRS.xlsx")|> select(-c("record_id"))

# import only the MRN's from Esper's data
esperData <- read.xlsx("Motion Analysis and DBS Programming Patient Panel (2014 - present).xlsx", "DBS Programming") |> select(c("MRN"))

# import report "Deep Brain Stimulation Patients" from redcap
redcapDBS <- ReadRedcapReport(token = Sys.getenv("movementDisorders_redcap_token"), 
                                url = "https://redcap.emory.edu/api/", report_id = 32159)

# remove variables from redcapDBS that come from svetlana's data
redcapDBS <- redcapDBS |>  select(-c("tec_mrn":"deep_brain_stimulation_patients_complete"))

# create 1 col df of all MRNs in redcapDBS, svetData, esperData
MRNs <- append(append(c(redcapDBS$mrn),c(svetData$tec_mrn)),c(esperData$MRN))
combo <- as.data.frame(MRNs)
combo <-distinct(combo)
combo["Motion"] <- combo$MRNs %in% redcapDBS$mrn
combo["Svjet"] <- combo$MRNs %in% esperData$MRN
combo["Christine"] <- combo$MRNs %in% svetData$tec_mrn
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

```

