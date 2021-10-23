# packages needed
Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre7")
install.packages("xlsx")
library("xlsx")

# example questions
fooABC <- function(x) {
  k <- x+1
  return(k)
}

fooXYZ <- function(x) {
  k <- fooABC(x)+1
  return(k)
}

# find ids of patients with missing info 
findMissing <- function(df, id, var){
  ids_with_missing <- df |>
    filter(is.na({{var}}))|>
    select({{id}})
  return(ids_with_missing)
}

mis <- findMissing(patients, ID, Age)
print(mis)

name <- "hello"
write.xlsx(mis, "missing_Age_IDS.xlsx",
           sheetName="fun", row.names = FALSE,
           col.names = TRUE, append=FALSE)

write.xlsx(mis, paste(name, ".xlsx", sep=""),
           sheetName="fun", row.names = FALSE,
           col.names = TRUE, append=FALSE)
