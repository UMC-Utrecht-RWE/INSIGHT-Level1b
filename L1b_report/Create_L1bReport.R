#Author: Albert Cid ROyo
#email: a.cidroyo@umcutrechjt.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 01/02/2023

#Empty memory
rm(list=ls())
gc()

#Get location of program
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)

system.time(source(paste0(projectFolder,"/CreateDAPSpecificCodelist.R")))

#Get needed packages
system.time(source(paste0(projectFolder,"/packages.R")))

#Level1b report



processingCodelist_EVENTS <- function(codelist_EVENTS){
  codelist.cols <- c('coding_system', 'code', 'event_abbreviation', 'type', 'tags', 'system', 'code_name')
  codelist_EVENTS <- codelist_EVENTS[, ..codelist.cols][, Outcome := paste0(system, "_", event_abbreviation, "_", type)][, ":="(event_abbreviation = NULL, type = NULL, system = NULL)]
  
  codelist_EVENTS <- codelist_EVENTS[,Outcome := toupper(Outcome)]
  codelist_EVENTS <- codelist_EVENTS[,coding_system := toupper(coding_system)]
  
  aggregate.cols <- c('code_name')
  codelist.cols.by <- codelist.cols <- c('coding_system', 'code', 'tags', 'Outcome')
  codelist_EVENTS <- codelist_EVENTS[, .SD[1], by = codelist.cols.by, .SDcols = aggregate.cols]
  codelist_EVENTS <- codelist_EVENTS[!is.na(code) & !is.na(Outcome)]
  start_with_colls <- c("ICD10CM", "ICD10", "ICD10DA", "ICD9CM", "MTHICD9", "ICPC", "ICPC2P","ICPC2EENG", "ATC", "vx_atc")
  
  return(codelist_EVENTS)
}

processingCodelist_VACCINES <- function(codelist_VACCINES){
  codelist_VACCINES <- codelist_VACCINES[!is.na(code) & !is.na(Outcome)]
  return(codelist_VACCINES)
}

processingCodelist_MEDICINES <- function(codelist_MEDICINES){
  codelist_MEDICINES <- codelist_MEDICINES[,c('drug_proxy','atc_codes','type')]
  setnames(codelist_MEDICINES,c('Outcome','code','coding_system'))
  codelist_MEDICINES <- codelist_MEDICINES[!is.na(code) & !is.na(Outcome)]
  return(codelist_MEDICINES)
}

processingUniqueCodelist_EVENTS <- function(UNIQUE_CODELIST){
  setnames(UNIQUE_CODELIST,c('event_record_vocabulary','value'),c('coding_system','code'))
  UNIQUE_CODELIST[, coding_system := toupper(coding_system)]
  UNIQUE_CODELIST[, code_no_dot := gsub("\\.", "", code)]#Deleting the dot from the code
  setnames(UNIQUE_CODELIST, 'code', 'code.DAP_UNIQUE_CODELIST')
  return(UNIQUE_CODELIST)
}

processingUniqueCodelist_MEDICINES <- function(UNIQUE_CODELIST){
  if(DAP %in% 'CPRD'){
    UNIQUE_CODELIST[, event_record_vocabulary := 'PRODCODEID']
  }else{
    UNIQUE_CODELIST[, event_record_vocabulary := 'ATC']
  }
  setnames(UNIQUE_CODELIST,c('event_record_vocabulary','value'),c('coding_system','code'))
  UNIQUE_CODELIST[, coding_system := toupper(coding_system)]
  UNIQUE_CODELIST[, code_no_dot := gsub("\\.", "", code)]#Deleting the dot from the code
  setnames(UNIQUE_CODELIST, 'code', 'code.DAP_UNIQUE_CODELIST')
  return(UNIQUE_CODELIST)
}

processingUniqueCodelist_MEDICINES <- function(UNIQUE_CODELIST){
  UNIQUE_CODELIST[, event_record_vocabulary := 'ATC']
  setnames(UNIQUE_CODELIST,c('event_record_vocabulary','value'),c('coding_system','code'))
  UNIQUE_CODELIST[, coding_system := toupper(coding_system)]
  UNIQUE_CODELIST[, code_no_dot := gsub("\\.", "", code)]#Deleting the dot from the code
  setnames(UNIQUE_CODELIST, 'code', 'code.DAP_UNIQUE_CODELIST')
  return(UNIQUE_CODELIST)
}

processingUniqueCodelist_VACCINES <- function(UNIQUE_CODELIST){
  return(processingUniqueCodelist_MEDICINES(UNIQUE_CODELIST))
}

inputFolder <- paste0(projectFolder,"/INPUT_LEVEL1B/")
if(!dir.exists(inputFolder)){
  stop("Create the folder INPUT_LEVEL1B and include the CODELIST filess")
}
codelistFolder <- paste0(projectFolder,"/INPUT_LEVEL1B/Codelists/")
if(!dir.exists(inputFolder)){
  stop("Create the folder INPUT_LEVEL1B and include the codelists files")
}

outputFolder <- paste0(projectFolder,"/OUTPUT/")
if(!dir.exists(outputFolder)){
  dir.create(outputFolder)
}

intermediateFolder <- paste(paste0(projectFolder,"/INTERMEDIATE/"),'Files',Sys.Date(), sep = "_",'/')
if(!dir.exists(intermediateFolder)){
  dir.create(intermediateFolder)
}

CDM_tables <- c('VACCINES','MEDICINES','EVENTS') #List of tables to be analyzed
additionalColumns <- list('EVENTS' = c("tags","code_name","meaning_of_event"), 'MEDICINES' = NA)
start_with_colls <- c("ICD10CM", "ICD10", "ICD10DA", "ICD9CM", "MTHICD9", "ICPC", "ICPC2P","ICPC2EENG", "ATC", "vx_atc")

for (cdm_table in CDM_tables){
  #Loading codelist table based on variable cdm_table
  codelist_name <- list.files(codelistFolder, pattern = paste0('_codelist_',cdm_table,'.csv'))
  file.copy(from = paste0(codelistFolder,codelist_name), to = paste0(intermediateFolder,codelist_name))
  codelist<- fread(paste0(codelistFolder,codelist_name), stringsAsFactors = F, na.strings = c("", NA), colClasses = 'character')
  codelist <- get(paste0('processingCodelist_',cdm_table))(codelist)
  
  #Saving results into excel
  outputFile_wb <- createWorkbook()
  addWorksheet(outputFile_wb,sheetName = 'Summary Table')
  addWorksheet(outputFile_wb,sheetName = 'Aggregated Summary Table')
  addWorksheet(outputFile_wb,sheetName = 'Agg. Sum. By Concept')
  indexSheet <- 3
  
  #Loading Level1b inputs
  unique_codelist_name <- list.files(inputFolder, pattern = paste0('_CODELIST_',cdm_table,'.rds'))
  
  for (files in unique_codelist_name){
    indexSheet <- indexSheet + 1
    DAP <- unlist(str_split(files,'_'))[2]
    UNIQUE_CODELIST <- readRDS(paste0(projectFolder,"/INPUT_LEVEL1B/",files))
    
    UNIQUE_CODELIST <- get(paste0('processingUniqueCodelist_',cdm_table))(UNIQUE_CODELIST)
    
    DAP_SPECIFIC_CODELIST <- CREATE_DAP_SPECIFIC_CODELIST(copy(UNIQUE_CODELIST),copy(codelist), additionalColumns = additionalColumns[[cdm_table]])
    
    DAP_SPECIFIC_CODELIST <- DAP_SPECIFIC_CODELIST[, eval(paste0(DAP,'_N_masked')) := N_masked][,N_masked := NULL]
    
    cols_order <- c("DAP", "Comment","Outcome",eval(paste0(DAP,'_N_masked')),"CDM_table","variable","coding_system","code.DAP_UNIQUE_CODELIST","code.CDM_CODELIST","length_str")
    if (!all(is.na(additionalColumns[[cdm_table]]))){
      cols_order <- c("DAP", "Comment","Outcome",eval(paste0(DAP,'_N_masked')),"CDM_table","variable","coding_system",additionalColumns[[cdm_table]],"code.DAP_UNIQUE_CODELIST","code.CDM_CODELIST","length_str")
    }
    # setorderv(DAP_SPECIFIC_CODELIST,cols = cols_order)
    DAP_SPECIFIC_CODELIST <- DAP_SPECIFIC_CODELIST[, ..cols_order]
    # reordering columns
    
    addWorksheet(outputFile_wb,sheetName = DAP)
    freezePane(outputFile_wb, sheet = indexSheet, firstRow = TRUE)  ## freeze first row and column
    writeDataTable(outputFile_wb, sheet = indexSheet, x = DAP_SPECIFIC_CODELIST, colNames = TRUE)
    
    write.csv(DAP_SPECIFIC_CODELIST, file = paste0(paste0(intermediateFolder,DAP,"_LEVEL1B_REPORT_ALL_",cdm_table,".csv")))
    rm(UNIQUE_CODELIST,DAP_SPECIFIC_CODELIST)
  }
  
  nameOutput <- paste0('LEVEL1B_REPORT_',cdm_table,'_',Sys.Date(),'.xlsx')
  
  if(dir.exists(intermediateFolder)){
    DAP_results <- list.files(intermediateFolder, pattern = paste0("_LEVEL1B_REPORT_ALL_",cdm_table,".csv"))
    
    if(length(DAP_results) >= 1){
      allOutput <- as.data.table(read_csv(paste0(intermediateFolder,DAP_results[1])))
      allOutput <- allOutput[Comment %in% c('CDM','BOTH')][,Comment := NULL]
      
    }
    if(length(DAP_results) > 1){
      for(idx in seq(2,length(DAP_results))){
        c_result <- as.data.table(read_csv(paste0(intermediateFolder,DAP_results[idx])))
        DAP_specificTab <- c_result[!Comment %in% c('CDM','BOTH')][,Comment := NULL]
        c_result <- c_result[Comment %in% c('CDM','BOTH')][,Comment := NULL]
        
        notIncludedCols <- c('...1',"DAP","length_str")
        
        mergeCols <- c('Outcome','CDM_table','variable','coding_system','code.DAP_UNIQUE_CODELIST')
        if (!all(is.na(additionalColumns[[cdm_table]]))){
          mergeCols <- c('Outcome','CDM_table','variable','coding_system',additionalColumns[[cdm_table]],'code.DAP_UNIQUE_CODELIST')        
          }
        allOutput <- merge(allOutput[,!..notIncludedCols],c_result[,!..notIncludedCols], by = mergeCols, all = TRUE)
        rm(c_result,DAP_specificTab)
      }
    }
    
    summaryCols <- mergeCols[!mergeCols %in% c("meaning_of_event")]
    dapCols <- names(allOutput)[str_detect(names(allOutput),"N_masked")]
    orderCols <- c(mergeCols,dapCols)
    allOutput <- allOutput[,..orderCols]
    
    #Creating summary table
    allOutput2_summary <- allOutput[, lapply(.SD, sum, na.rm=TRUE), by=summaryCols, .SDcols=dapCols]
    allOutput2_summary[, Total := rowSums(.SD), .SDcols = dapCols]
    
    allOutput3_summary <- allOutput[, lapply(.SD, sum, na.rm=TRUE), by=Outcome, .SDcols=dapCols]
    allOutput3_summary <- merge(allOutput3_summary,unique(codelist[,list(Outcome)]), by = 'Outcome', all = TRUE)
    
    freezePane(outputFile_wb, sheet = 1, firstRow = TRUE)  ## freeze first row and column
    writeDataTable(outputFile_wb, sheet = 1, x = allOutput, colNames = TRUE)
    
    freezePane(outputFile_wb, sheet = 2, firstRow = TRUE)  ## freeze first row and column
    writeDataTable(outputFile_wb, sheet = 2, x = allOutput2_summary, colNames = TRUE)
    
    freezePane(outputFile_wb, sheet = 3, firstRow = TRUE)  ## freeze first row and column
    writeDataTable(outputFile_wb, sheet = 3, x = allOutput3_summary, colNames = TRUE)
    
    saveWorkbook(outputFile_wb, paste0(outputFolder,nameOutput), overwrite = TRUE)
  }
  
}

