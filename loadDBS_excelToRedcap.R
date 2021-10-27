library(motionTools)
library(tidyverse)
redcap_data <- ReadRedcapReport(token = Sys.getenv("movementDisorders_redcap_token"), 
                               url = "https://redcap.emory.edu/api/", report_id = 32159)

library("openxlsx")
svetData <- read.xlsx("DBS_pts_10-21-21_GRS.xlsx")

#next steps:
#1) do "deep plyer left join" to populate the mostly blank "redcap_data" df 
#   with the data from the "svetData" df