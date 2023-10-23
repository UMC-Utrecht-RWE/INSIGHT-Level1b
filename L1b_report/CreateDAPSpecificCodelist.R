# Author: Albert Cid Royo
# email: a.cidroyo@umcutrecht.nl
# Organisation: UMC Utrecht, Utrecht, The Netherlands
# Date: 01/12/2022

#Decription:
# This function merges the unique code list output of the Level Check 1b with the a study code list defined.
# 

#This version includes all the concepts even the missing and not missing ones.
CREATE_DAP_SPECIFIC_CODELIST <- function(UNIQUE_CODELIST, STUDY_CODELIST, additionalColumns = NA){
  
  STUDY_CODELIST[, code_no_dot := gsub("\\.", "", code)]
  STUDY_CODELIST[, length_str := str_length(code_no_dot)] #defining the length of the study code list codes
  setnames(STUDY_CODELIST, 'code', 'code.CDM_CODELIST')
  minLength_study_CODELIST <- min(STUDY_CODELIST$length_str, na.rm = TRUE)
  
  #Dividing codelist in exact and start with search codes
  start_UNIQUE_CODELIST <- UNIQUE_CODELIST[coding_system %in% start_with_colls]
  start_study_CODELIST <- STUDY_CODELIST[coding_system %in% start_with_colls]
  exact_UNIQUE_CODELIST <- UNIQUE_CODELIST[!coding_system %in% start_with_colls]
  exact_study_CODELIST <- STUDY_CODELIST[!coding_system %in% start_with_colls]
  
  
  #Finding Exact codes from the DAP (UNIQUE_CODELIST) that exist in our code list
  exactMatch <- merge(exact_UNIQUE_CODELIST,exact_study_CODELIST, by = c('coding_system','code_no_dot'))
  
  #Finding Start with codes from the DAP (UNIQUE_CODELIST) that exist in our code list
  start_exactMatch <- merge(start_UNIQUE_CODELIST,start_study_CODELIST, by = c('coding_system','code_no_dot')) #Some cases that belong to a start with might be search as exact match with the code list
  start_UNIQUE_CODELIST <- as.data.table(anti_join(start_UNIQUE_CODELIST,start_exactMatch, by = c('coding_system','code_no_dot')))
  #start_study_CODELIST <- as.data.table(anti_join(start_study_CODELIST,start_exactMatch, by = c('coding_system','code_no_dot')))
  
  #Definitio of original length of string before splitting it
  start_UNIQUE_CODELIST[, ori_length_str := str_length(code_no_dot)]
  max_code_length <- max(start_UNIQUE_CODELIST$ori_length_str)
  print(paste0("[SetCodesheets] Max length of code from the DAP is : ",max_code_length))
  
  #Range of string split to work on based on the max_code_length found in the DAP and the minimum code length found in the study code list
  listColsNames <- seq(minLength_study_CODELIST,max_code_length)
  invisible(lapply(listColsNames, function(x){
    start_UNIQUE_CODELIST[ori_length_str >= x, as.character(x) := substr(code_no_dot,1,as.numeric(x))]
  })) #Generating new columns with the code split from 1 to x characters
  
  #Melting the dataset (going form a wide format to a long format)
  start_UNIQUE_CODELIST <- melt(start_UNIQUE_CODELIST, variable.name = 'length_str', 
                                measure.vars = as.character(listColsNames), na.rm = TRUE, 
                                variable.factor = FALSE, 
                                value.name = 'code_no_dot2')
  
  start_UNIQUE_CODELIST <- start_UNIQUE_CODELIST[, length_str := as.numeric(length_str)]
  #setnames(start_UNIQUE_CODELIST, 'code', 'code.UNIQUECODELIST')
  
  
  #Inner joining the table with all the code split in different length and the codelist
  results_startwith <- merge(x = start_UNIQUE_CODELIST,
                             y = start_study_CODELIST, 
                             by.x =  c('coding_system','code_no_dot2','length_str'), 
                             by.y = c('coding_system','code_no_dot','length_str'))
  
  #After the previous inner-join there must be the possibility to have more than ones the code matched to different
  colsBy <- c('code.DAP_UNIQUE_CODELIST', 'Outcome', 'coding_system')
  if (!all(is.na(additionalColumns))){
    colsBy <- c(colsBy,additionalColumns)
  }
  
  results_startwith2 <- results_startwith[order(length_str,decreasing = TRUE), .SD[1] , by = colsBy]
  
  #Compile all different results into one output
  DAP_specific_codelist <- rbindlist(list(exactMatch,start_exactMatch), use.names = T) #results_startwith
  cols_sel <- names(DAP_specific_codelist)
  DAP_specific_codelist <- rbindlist(list(DAP_specific_codelist,results_startwith2[,..cols_sel]), use.names = T)
  
  MissingFromCDM <- as.data.table(anti_join(UNIQUE_CODELIST,DAP_specific_codelist, by = c('coding_system','code_no_dot')))
  MissingFromCODELIST <- as.data.table(anti_join(STUDY_CODELIST,DAP_specific_codelist, by = c('coding_system','code_no_dot')))
  
  DAP_specific_codelist <- rbindlist(list(DAP_specific_codelist,MissingFromCDM), fill = TRUE)
  DAP_specific_codelist <- rbindlist(list(DAP_specific_codelist,MissingFromCODELIST), fill = TRUE)
  
  DAP_specific_codelist[, Comment := 'BOTH']
  DAP_specific_codelist[is.na(code.DAP_UNIQUE_CODELIST), Comment := 'CODELIST']
  DAP_specific_codelist[is.na(code.CDM_CODELIST), Comment := 'CDM']
  
  return(DAP_specific_codelist)
  
}