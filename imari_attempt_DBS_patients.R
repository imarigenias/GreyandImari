# load necessary packages ----
library(motionTools)
library(tidyverse)
library(dplyr)
library("openxlsx")

# upload redcap report ----
redcap_data <- ReadRedcapReport(token = Sys.getenv("movementDisorders_redcap_token"), 
                                url = "https://redcap.emory.edu/api/", report_id = 32159)

# import excel files ----
#load Svetlana's DBS excel
svetData <- read.xlsx("DBS_pts_10-21-21_GRS.xlsx")

#load sample Redcap report of DBS instrument; to be replaced with 
#Redcap report load in future
redcapDBSReport <- read.xlsx("sampleDBSReport.xlsx")

#next steps:
#1) do "deep plyer left join" to populate the mostly blank "redcap_data" df 
#   with the data from the "svetData" df
# want people from redcap who are also in svet
innerJoin <- inner_join(redcapDBSReport, svetData, by="tec_mrn")
leftJoin <- left_join(redcapDBSReport, svetData, by="tec_mrn")

# svetData stats ----
stats_svetData <- svetData |> 
  summarise(
    total_obs= nrow(svetData),
    num_unqiue_tec = length(unique(tec_mrn)),
    num_unique_euh = length(unique(euh_mrn)),
    numNA_tec = sum(is.na(tec_mrn)),
    numNA_euh = sum(is.na(euh_mrn))
  )
stats_svetData


# redcap Data stats----
stats_redcapData <- redcapDBSReport |> 
  summarise(
    total_obs= nrow(redcapDBSReport),
    num_unqiue_tec = length(unique(tec_mrn)),
    num_unique_euh = length(unique(euh_mrn)),
    numNA_tec = sum(is.na(tec_mrn)),
    numNA_euh = sum(is.na(euh_mrn))
  )
stats_redcapData

compare_svet_redcap <- rbind(stats_redcapData, stats_svetData)
row.names(compare_svet_redcap) <- c("redcap", "svet")
compare_svet_redcap


table(redcapDBSReport$tec_mrn %in% svetData$tec_mrn)
# 502 tec_mrn are in redcap but not svet; 445 are in redcap and svet


table(svetData$tec_mrn %in% redcapDBSReport$tec_mrn)
# 228 tec_mrn are in svetData but not redcap; 431 are in redcap and svet

