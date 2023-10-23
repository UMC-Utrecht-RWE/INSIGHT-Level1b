#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/05/2022

##Aim
##Date of data extraction and DAP name is retrieved from CDM. This is used for the naming of the output files.

##in/output
#Input 1: CDM_SOURCE.csv
#Output 1: the variable DAP, if no DAP info found than continue with UNKDAP 
#Output 2: the variable DATE, if no DATE info found than continue with  the date if today.

#Check if the CDM_SOURCE.csv file is in the CDM tables folder. This to prevent the program from stopping if the file is not available yet. 
if(file.exists(paste0(path, "CDM_SOURCE.csv"))){
  
  #Load only the needed columns from the CDM_SOURCE.csv and set date_creation to the desired date format so sorting is possible
  INFO <- IMPORT_PATTERN(dir = path, pat = "CDM_SOURCE.csv", colls = c("data_access_provider_name", "date_creation"), date.colls = "date_creation")
  
  #Check if the CDM_SOURCE.csv is filled. This to prevent the program from stopping if the file is not filled.
  if(nrow(INFO) > 0){
    #Order on date_creation with a minus meaning from high to low. This is done because we want the most recent row.
    setorder(INFO, -date_creation )
    #Get the date and dap. This is done from the first row which is the most recent information because of the sorting
    DAP <- INFO[1,][["data_access_provider_name"]]
    DATE <- gsub("-", "", as.character(INFO[1,][["date_creation"]]))
  }else{
    #If no information available we still want to have an output naming for the output files.
    DAP <- "UNKDAP"
    DATE <- gsub("-", "", as.character(Sys.Date()))
    print("CDM_SOURCE.csv has 0 rows so for date the day of today is used in output files and DAP is filld with UNKDAP")
  }
  
  if(is.na(DAP)){
    #If no information available we still want to have an output naming for the output files.
    DAP <- "UNKDAP"
    print("DAP not filled in CDM_SOURCE.csv. UNKDAP is used for the naming of the output files")
  }
  
  if(is.na(DATE)){ 
    #If no information available we still want to have an output naming for the output files.
    DATE <- gsub("-", "", as.character(Sys.Date()))
    print("Date not filled in CDM_SOURCE.csv. Date of today is used for the naming of the output files ")
  }
  
  rm(INFO)
  
}else{
  #If no information available we still want to have an output naming for the output files.
  DAP <- "UNKDAP"
  DATE <- gsub("-", "", as.character(Sys.Date()))
  print("CDM_SOURCE.csv is missing so for date the day of today is used in output files and DAP is filld with UNKDAP")
}
