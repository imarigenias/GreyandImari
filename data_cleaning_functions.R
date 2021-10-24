# packages needed
Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre7")
# install.packages("xlsx")
library("xlsx")
library("tidyverse")

'
# create data set
patients <- data.frame("ID" = c("01", "02", "03", "04", "05", "06", "07",
                                "08","09", "10"),
                       "Age" = c(50, 18, NA, 45, NA, 20, 45, 60, 89, 90),
                       "Dosage" = c(100, 120, 130, NA, 3000, 450, 800, 300,
                                    NA, 500),
                       "Dosage_Manual" = c(100, 250, 130, 150, 300, 450, 800, 
                                           300,20,50)
)
'
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

"
# check function
mis <- findMissing(patients, ID, Age)
print(mis)


# sample new data set with added info 
replaced_ages <- mis
replaced_ages$Age <- c(20, 30)
replaced_ages
"

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

"
# check function
patients <- populateMissing(patients, replaced_ages, ID, Age)
patients
"

'
# creates excel report from data ----
write.xlsx(mis, "missing_Age_IDS.xlsx",
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