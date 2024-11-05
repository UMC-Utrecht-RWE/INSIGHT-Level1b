#Authors: Albert Cid Royo
#         Roel Elbers MSc.
#email: a.cidroyo@umcutrecht.com 
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/05/2022

##Aim: Make output files that can be stored on DRE that are fit for purpose.
#Input: WHERECLAUSE & ANSWERS .rds files stored in g_intermediate per CDM table.
#Output1: WHERECLAUSE csv files per CDM table stored with DAP name and date of last extraction in the filename
#Output2: ANSWERS csv files per CDM table stored with DAP name and date of last extraction in the filename
#Output3: CODELIST rds files per CDM table stored with DAP name and date of last extraction in the filename


#Output line 1 is the original output. It is an copy of the rds files in g_intermediate only the counts are masked if lower then 5 and it is in csv format
#So the pi's that are less familiar with R can look into the files and take actions.
#TODO: connect  ANSWERS in the function createOutput1 to the main variable GetCountsColumns
createOutput1 <- function(tables_CDM,WHERECLAUSE = T, ANSWERS = F){
  
  for (table in tables_CDM){
    if (ANSWERS == T){
      answerTable <- as.data.table(readRDS(paste0(projectFolder, "/g_intermediate/",DATE,"_",DAP,"_ANSWERS_",table,".rds")))
      if (nrow(answerTable) > 0){
      answerTable <- answerTable[,  N_masked :=  fifelse(N < 5, 2.5, N) ][, N := NULL]
      }
      fwrite(answerTable,paste0(projectFolder, "/g_output/",DATE,"_",DAP,"_ANSWERS_",table,".csv"), sep = ";") 
      rm(answerTable)
    }
    
    if (WHERECLAUSE == T){
      whereClauseTable <- as.data.table(readRDS(paste0(projectFolder, "/g_intermediate/",DATE,"_",DAP,"_WHERECLAUSE_",table,".rds")))
      if (nrow(whereClauseTable) > 0){
        whereClauseTable <- as.data.table(whereClauseTable)[,  N_masked :=  fifelse(N < 5, 2.5, N) ][, N := NULL]  
      }
      fwrite(whereClauseTable[, Study_variable := as.character()],paste0(projectFolder, "/g_output/",DATE,"_",DAP,"_WHERECLAUSE_",table,".csv"), sep = ";")
      rm(whereClauseTable)
    }
  }
}

#Creating distinct list of codes with their coding systems
#Output line 2: This is added later on and the aim is to provide .rds files that can be compared with the code lists for events, medicines and vaccines on DRE.  
createOutput2 <- function(scheme,tables_CDM, keep.N = T ){
  
  for (table in tables_CDM){
    # if (ANSWERS == T){
    #   answerTable <- readRDS(paste0(projectFolder, "/g_intermediate/",DATE,"_",DAP,"_ANSWERS_",table,".rds"))
    #   answerTable <- answerTable[,  N_masked :=  fifelse(N < 5, "< 5", as.character(N)) ][, N := NULL]    
    #   fwrite(answerTable,paste0(projectFolder, "/g_output/",DATE,"_",DAP,"_ANSWERS_",table,".csv"), sep = ";") 
    #   rm(answerTable)
    # }
    
    #if (WHERECLAUSE == T){
      whereClauseTable <- as.data.table(readRDS(paste0(projectFolder, "/g_intermediate/",DATE,"_",DAP,"_WHERECLAUSE_",table,".rds")))
      # whereClauseTable <- as.data.table(whereClauseTable)[,  N_masked :=  fifelse(N < 5, "< 5", as.character(N)) ][, N := NULL]
      # fwrite(whereClauseTable[, Study_variable := as.character()],paste0(projectFolder, "/g_output/",DATE,"_",DAP,"_WHERECLAUSE_",table,".csv"), sep = ";")
      
      #Creating output of DISTINCT codelist
      sel.col.codes <- scheme[[table]][['code']]
      sel.col.codes <- sel.col.codes[sel.col.codes %in% names(whereClauseTable)]
      
      sel.col <- c(sel.col.codes)
      if ('coding_system' %in% names(scheme[[table]])){
        sel.col.coding_sys <- scheme[[table]][['coding_system']]
        sel.col.coding_sys <- sel.col.coding_sys[sel.col.coding_sys %in% names(whereClauseTable)]
        sel.col <- c(sel.col.codes,sel.col.coding_sys)
      }
      
      if (keep.N == T){
        sel.col <- c(sel.col,'N')
      }
      select_values <- whereClauseTable[!whereClauseTable[, Reduce(`&`, lapply(.SD, is.na)),.SDcols = sel.col.codes], ..sel.col]
      
      if (keep.N == T){
        sel_col2 <- sel.col[! sel.col %in% 'N']
        select_values <- select_values[, .(N = sum(N)), by = sel_col2]
      }

      select_values <- as.data.table(unique(melt(unique(select_values), measure.vars = sel.col.codes)))
      select_values <- select_values[!is.na(value)]
      select_values <- select_values[,'DAP' := DAP]
      select_values <- select_values[,'CDM_table' := table]
      
      saveRDS(select_values, paste0(projectFolder, "/g_intermediate/",DATE,"_",DAP,"_CODELIST_",table,".rds"))
      
      select_values <- unique(select_values[,  N_masked :=  fifelse(N < 5, 2.5, N) ][, N := NULL])
      saveRDS(select_values, paste0(projectFolder, "/g_output/",DATE,"_",DAP,"_CODELIST_",table,".rds"))
      
      rm(whereClauseTable)
    }
  }
#}

tables_CDM_1 <- t.interest# t.interest[!t.interest %in% c('EVENTS','MEDICINES')]

createOutput1(tables_CDM_1)

tables_CDM_2 <- c('EVENTS','MEDICINES','VACCINES', 'PROCEDURES')
scheme <- list()
scheme <- scheme[table = tables_CDM_2]

scheme <- expand.grid(c('vx_type','vx_atc'),c('EVENTS','MEDICINES','VACCINES'))
scheme <- list(EVENTS = list(code = c('event_code','event_free_text'), coding_system = c('event_record_vocabulary','meaning_of_event')),
               MEDICINES = list(code = c('medicinal_product_atc_code','medicinal_product_id')),
               VACCINES = list(code = c('vx_atc','vx_type')),
               PROCEDURES = list(code = c('procedure_code'), coding_system = c('procedure_code_vocabulary')))

#Create counts 
createOutput2(scheme,tables_CDM_2,keep.N = T)           
