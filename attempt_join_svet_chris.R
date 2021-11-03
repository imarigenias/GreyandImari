# SVET DATA----
# remove record id
svetData <- read.xlsx("DBS_pts_10-21-21_GRS.xlsx") |> select(-c("record_id"))

# RedCap Data----
redcap_data <- ReadRedcapReport(token = Sys.getenv("movementDisorders_redcap_token"), 
                                url = "https://redcap.emory.edu/api/", report_id = 32159)

leftJoin <- left_join(redcap_data, svetData)

names(leftJoin)
table(names(leftJoin) %in% names(redcap_data))
