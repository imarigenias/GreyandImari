# packages needed
Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre7")
# install.packages("openxlsx")
library("openxlsx")
library("tidyverse")

# create table of ids and var with missing value ----
findMissing <- function(df, id, var){
  # df | data frame
  # id | id to match data
  # var | variable to find missing values for 
  
  ids_with_missing <- df |>
    filter(is.na({{var}}))|>
    select({{id}}, {{var}})
  return(ids_with_missing)  # return data frame of id and var 
}


# replace NAs with updated values ----
populateMissing <- function(originalDF, editedDF, id, var){
  # convert id and var to strings
  id <- deparse(substitute(id))
  var <- deparse(substitute(var))
  
  # change value in one data frame based on matched ids 
  originalDF[,var][match(editedDF[,id], originalDF[,id])] <- editedDF[,var]
  updatedDF <- originalDF
  # return updated data frame
  return (updatedDF)
}
# link: https://stackoverflow.com/questions/2641653/pass-a-data-frame-column-name-to-a-function 


'
# creates excel report from data ----
write.xlsx(patients, "missing_Age_IDS.xlsx",
           sheetName="fun", row.names = FALSE,
           col.names = TRUE, append=FALSE)

# try creating file name from object name
name <- "hello"
write.xlsx(mis, paste(name, ".xlsx", sep=""),
           sheetName="fun", row.names = FALSE,
           col.names = TRUE, append=FALSE)
'

# another way to replace values based on matched ids 
#patients$Age[match(replaced_ages$ID, patients$ID)] <- replaced_ages$Age
# link: https://stackoverflow.com/questions/40177132/replace-values-from-another-dataframe-by-ids 

# create data frame of observations with different variable values 
findDiff <- function(df, id, var1, var2){
    # change variable input to string 
    id_str <- deparse(substitute(id))
    var1_str <- deparse(substitute(var1))
    var2_str <- deparse(substitute(var2))
  
    # select observations with different variable values 
    diffDF <- df[!df[,var1_str] %in% df[,var2_str], ] |> 
      select({{id}}, {{var1}}, {{var2}})  # only extract the relevant variables
    
    return(diffDF)
}
'# show values in Dosage that are different than Dosage Manual 
  patients$Dosage[!patients$Dosage %in% patients$Dosage_Manual]'