usethis::edit_r_environ() # access .Renviron file

#!/usr/bin/env Rscript
token <- "E41B8D3BEE51E7124E89C1363C87466E"
url <- "https://redcap.emory.edu/api/"
formData <- list("token"=token,
                 content='report',
                 format='json',
                 report_id='23307',
                 csvDelimiter='',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
result <- httr::content(response)
print(result)

# new attempt;

library(motionTools)
library(tidyverse)

redcap_data1 = ReadRedcapReport(token = Sys.getenv("clinic_redcap_token"), url = "https://redcap.emory.edu/api/", report_id = 32116) |> 
  group_by(ledd) |> 
  select(record_id)


 
   group_by(record_id) |> 
  fill(first_name) |> 
  fill(last_name) |> 
  fill(dw_icd) |> 
  filter(redcap_repeat_instrument == "capture_session") |> 
  select(record_id,first_name,last_name,dw_icd,patient_fullpath_neurology) |> 
  mutate(directory = paste0("/Volumes/",patient_fullpath_neurology))

  



# download motionTools package
install.packages("devtools")
# Load the devtools package.

library(devtools)
# In most cases, you just use install_github("author/package"). For example, with my R/broman package, which exists at github.com/kbroman/broman, you’d type
install_github("jlucasmckay/motionTools") 
