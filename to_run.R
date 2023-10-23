
#Author:Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 26/07/2021

#Aim: get files that give a overview regarding the semantics of the data. Or what information is exactly is in the CDM.
#input: CDM csv files
#output: WHERECLAUSE csv files. This file contains an overview of all the distinct rows (id and date columns excluded) and a count per distinct row.

#Empty memory to be sure a previous run does not interact with a previous run.
rm(list=ls())
gc()

###
#Fill variables (StudyName or path_to_fill) that are meant to point to the location of the CDM csv files, which are the tables to analyze. 
#There are two ways of doing this, via a path of choice or via a folder name (StudyName) within the standardized folder CDMInstances. 
#If StudyName is set to NULL a path needs to be specified. If a StudyName is defined the path_to_fill is set to NULL 
#See wiki for more profound explanation.

StudyName <- "RTI_SIM_CSV_100k"
#StudyName <- NULL
#path_to_fill <- "/Users/acidroyo/Documents/GitHub/Astrazeneca-PASS-study/CDMInstances/RTI_SIM_CSV"
path_to_fill <- NULL
###

###
#Choose which tables you want to analyse or fill NULL to get all tables analysed. Eurocat cannot be analysed by this script
#t.interest <- c("SURVEY_OBSERVATIONS", "SURVEY_ID", "MEDICAL_OBSERVATIONS", "VACCINES")
t.interest <- NULL
###

#Load the package rstudioapi that is needed to get location of program
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

#Retrieve the path to the location of the program. This variable serves as the starting path to define the paths of other directories used in this program.
projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)

#Set the location of the CDM tables
if(!is.null(StudyName)){
  #Set location of CDM tables only if the StudyName is choosen to point to the CDM tables 
  system.time(source(paste0(projectFolder,"/99_path.R")))
}else{path <- path_to_fill}


#Empty g_output folder to be sure that the end results are from this run and not a previous
if(length(list.files(paste0(projectFolder,"/g_output"))) > 0)file.remove(paste0(projectFolder,"/g_output/",list.files(paste0(projectFolder,"/g_output"))))

#Load needed functions. This should be put into a module so changes in the functions are updated automatically.
system.time(source(paste0(projectFolder,"/p_steps/functions/IMPORT_PATTERN.R")))
system.time(source(paste0(projectFolder,"/p_steps/functions/GetColumnNamesCDM.R")))

#Get needed packages
system.time(source(paste0(projectFolder,"/packages.R")))

#Get info from CDM_SOURCE file needed for naming output files (DAP and Date of data extraction)
system.time(source(paste0(projectFolder,"/p_steps/GetInfoOutputFiles.R")))

#Get CDM tables and columns. This is the starting point for the program to know what tables and columns are valid and need to be included in the analyses.
TABLES <- GetColumnNamesCDM(paste0(projectFolder,"/p_meta/ConcePTION_CDM tables v2.2.xlsx"))

#Correct for problems with excel import and mistakes in the original CDM file. csv files are imported via IMPORT_PATTERN and checked. This is here done for excel.
#If leading or trailing spaces are included than filtering is not accurate. 
invisible(lapply(c("Variable", "TABLE"), function(x) TABLES <- TABLES[, eval(x) := str_trim(get(x), "both")]))
TABLES <- TABLES[, .(Variable, TABLE)]
TABLES[TABLES == ""] <- NA

#Note that the original files stored in the google drive contain or contained mistakes in the spelling. I made hard coded corrections for this here.
###
TABLES <- TABLES[Variable ==  "inidication_code" , Variable := "indication_code"]
###
#TODO, fix the original files instead of correcting in this script.


#Define tables to analyse if the user did not fill the t.interest themselves.
if(is.null(t.interest)){
      #Get the tables that are known in the CDM
      t.interest <- unique(TABLES[["TABLE"]])
      
      #some tables in the CDM do not contain information of interest. So these tables are excluded from the variable here.
      t.interest <- t.interest[!t.interest %in% c("METADATA","CDM_SOURCE","INSTANCE")]
      
      #Define only the tables that are found in the CDM folder. If a DAP does not have a particular table it is not analysed.
      #TODO, this line of code can be written vectorized and mayby a start with is more valid.
      t.interest <- t.interest[unname((unlist(sapply(t.interest, function(x) any(grepl(x ,list.files(path)), na.rm = T)))))] 
      
      #Remove eurocat because invalid and wrong specified column names. Moreover, it is a very wide table which makes this analyses less useful.
      t.interest <- t.interest[!grepl("EUROCAT", t.interest)]
      
} 
  

#Run program that is counting all combinations after deleting columns with id and date in it.
system.time(source(paste0(projectFolder,"/p_steps/GetCounts.R")))

#Create output files that can be uploaded to DRE
system.time(source(paste0(projectFolder,"/p_steps/Step_002_CreateOutput.R")))








