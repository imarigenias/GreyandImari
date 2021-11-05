# SVET DATA----
# remove record id
library(tidyverse)
library(motionTools)
library(openxlsx)
svetData <- read.xlsx("DBS_pts_10-21-21_GRS.xlsx") |> select(-c("record_id"))

# RedCap Data----
redcap_data <- ReadRedcapReport(token = Sys.getenv("movementDisorders_redcap_token"), 
                                url = "https://redcap.emory.edu/api/", report_id = 32159)

# remove variables that come from svetlana's data
redcap_data <- redcap_data |>  select(-c("tec_mrn":"deep_brain_stimulation_patients_complete"))

# re-factor svet's tec_mrn variable as character
svetData$tec_mrn <- as.character(svetData$tec_mrn)

# tidyverse left join; tell R that mrn in redcap = tec_mrn in svetData
leftJoin <- left_join(redcap_data, svetData, by=c("mrn" = "tec_mrn"))



