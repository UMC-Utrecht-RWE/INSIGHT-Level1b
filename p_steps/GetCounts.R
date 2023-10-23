#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/05/2022

##TODO: This script has 2 procedures which can be splitted in 2 scripts with it's own aim. 
##Aim 1: Load CDM data into a sqlite database and prepare the database.
  #Input: CDM csv files
  #Output: A SQLite database file
##Aim 2: Create rds files that are more lean because id and date information is deleted and distinct rows or columns are stored.
  #Input: SQLite database
  #Output 1: WHERECLAUSE .rds files
  #Output 2: ANSWERS .rds files
  #TODO, extra outputs can be distinct date and id columns that can be used to do simple checks that are now done in level 1

#Get info for all tables that are in scope of interest
TABLES2 <- as.data.table(TABLES)[TABLE %in% t.interest, ]

#Originally the distinct rows and count are needed and column wise counts are less meaningful. Some would like have the column wise counts to so therefore
#A variable is made. If set to F only the row wise count is executed.
GetCountsColumns <- T

#Set up database in SQLite. First deleted the previous one to not get duplicate information or combinations of instances.
#TODO, make a variable for the database file at the start of to_run so less duplicating information is in the code.
if(file.exists(paste0(projectFolder,"/g_intermediate/database.db"))) file.remove(paste0(projectFolder,"/g_intermediate/database.db"))
mydb <- dbConnect(RSQLite::SQLite(), paste0(projectFolder,"/g_intermediate/database.db"))

#Loop over all the relevant CDM tables, append them per type of CDM table and count the unique rows. The results are stored in rds file in the folder g_intermediate.
for(j in t.interest){
  
  #Get needed columns and files for CDM table of interest. This is used to verify if column names are valid and prepare table for appending
  needed <- unique(TABLES2[TABLE == j,][["Variable"]])            
  
  #Create a variable with all the files for a specific CDM table. DAPS do store the information per CDM table in multiple csv's. Therefore, a loop is needed to go over all
  #csv files per CDM table.
  files <- list.files(path = path, pattern = j)
  
  #AIM1
  for(i in files){
    
    #Import csv via import_pattern so spaces are removed from start and tail. Also all is imported as character. This is needed to prevent import packages from making it's
    #own formatting choices. This causes unpredictable behavior that causes errors later on.
    TEMP <- IMPORT_PATTERN(
      append = F,
      dir = path, 
      pat = i,
    )
    
    ###
    #Note that: for appending the several csv's per CDM table to 1 table, column names, order and format need to be identical. Because all csv's are imported as character,
    #the formats are equal. However, for the column names and it's order checks and actions need to be added to the procedure.
    
    #Note that: Ideally, the CDM contains only valid column names. However, in practice this is not the case so checks are needed.
    
        if(any(!colnames(TEMP) %in% needed)){
          #Distinguish columns that are in the CDM csv that are not valid. 
          colsWrong <- colnames(TEMP)[!colnames(TEMP) %in% needed]
          
          #Remove invalid columns so it will not hamper appending process
          lapply(colsWrong, function(x) TEMP <- TEMP[, eval(x) := NULL])
          
          #Feed back to the user what did happen so the user can react to that by fixing his tables is needed
          print(paste0("Check colnames file ",i, " Some columns are excluded from the analyses: ", paste0(colsWrong, collapse = ",")))
          rm(colsWrong)
          }
    
          #Put all the column in the table that can exist in the CDM table
          #First Distinguish columns that are missing and load in a variable
          to_add <- needed[!needed %in% colnames(TEMP)]
          if(length(to_add) > 0){
              #Feedback to the user what happens so the user can react to that by fixing his tables is needed.
              print(paste0("In table ",i," colls are missing and are added as empty columns: ", paste0(to_add, collapse = ",")))
              
              #To be sure that all columns are there, add the missing columns as an empty column in character format.
              #TODO: research if this step is needed. Later we use DBI::dbWriteTable() instead of a self written query. Maybe this is already taken into account by this function. 
              lapply(to_add, function(x) TEMP <- TEMP[, eval(x) := as.character()])
              
          }
          rm(to_add)  
      
      ###
    
    #Append the individual table to the database. You can also write an update query adding the table. Then the order of the column need to be identical. According to documentation
    #dbWriteTable takes this into account. https://dbi.r-dbi.org/reference/dbwritetable      
    dbWriteTable(mydb, j, TEMP, append = T, overwrite = F)
    rm(TEMP)
    gc()
    
  }
  
  #remove information infor this loop
  rm(needed, files)            
  
  
  #TODO: during commenting I realized that the indexes where not set. Indexes are needed for better performance when queries want to extract information form the database.
  #Indexes need to be added when all data is appended to the database because updating tables means recalculating the indexes. So this would be the place to add the indexes.
  #See: https://www.brentozar.com/training/think-like-sql-server-engine/
  
  #AIM2
  #Go over every table in the database
  if(j %in% dbListTables(mydb)){
    
    #Get the column names that are in the table and store in a variable
    #TODO: replace this by DBI::dbListFields() function
    colls <- colnames(dbGetQuery(mydb,
                                 
                                 paste0(
                                   "
                SELECT  * FROM 
                ",j, " LIMIT 1"  
                                   
                                   
                                   
                                 )
                                 
    ))
    
    #Remove id's and dates using a contains/grepl action. This is because (1) this is not relevant information for the purpose of seeing what concepts are in the data, (2) the reduction of information via an
    #distinct or group by action is achieved better if dates and id's are removed because there are many different values of that per column. 
    colls <- colls[!grepl("date", colls)]
    colls <- colls[!(grepl("_id", colls) & !grepl("medicinal_product_id", colls))]
    #In the persons table the dats is stored on integers so they cannot be removed by contains(date)
    colls <- colls[!colls %in% c("day_of_birth", "month_of_birth","year_of_birth","day_of_death","month_of_death", "year_of_death" )]#"event_code", "medicinal_product_atc_code")]
    
    #Retrieve the distinct rows and add a count. group by with a count is giving the same effect as a distinct only with distinct there is no count.
    VALUES <- dbGetQuery(mydb,
                         
                         paste0(
                           "
                  SELECT count(*) AS N,",paste0(colls, collapse = ",")," FROM ",j," 
                  
                  GROUP BY ",paste0(colls, collapse = ",")
                           
                           
                         )
                         
                         
    )
    
    #If wanted, also do a count for the columns (date and id column are also excluded)
    #TODO: if it is desired to test by example dates validity, then the colls variable need to be regenerated (or stored in another variable), then the distinct result can 
    #be analysed faster. However, maybe it is needed then to store this count in the database and not in a rds file because of the size of the file.
    if(GetCountsColumns){
    for(i in colls){
      
      #Do the count by column
      VALUES2 <- dbGetQuery(mydb,
                            
                            paste0(
                              "
                            SELECT count(",i,") AS N,",i," FROM ",j," 
                            
                            GROUP BY ",i
                            )
      )
      
      #Store in a long format table by adding the column name in Column.
      setnames(VALUES2, i, "Result")
      VALUES2 <- as.data.table(VALUES2)[, Column := i]
      
      #Append to VALUES3
      #TODO: make better names for VALUES
      if(i == colls[1]){VALUES3 <- VALUES2}else{
        VALUES3 <- rbindlist(list(VALUES3, VALUES2), fill = F, use.names = T)
      }
      rm(VALUES2)
      
    }
    #Save the files to g_intermediate. These files can be used for further analyzes.
    #TODO: Remove date as the first argument. It is not valid to start with numeric values a file name.  
    saveRDS(VALUES3,paste0(projectFolder, "/g_intermediate/",DATE,"_",DAP,"_ANSWERS_",j,".rds"))
    }
    saveRDS(VALUES,paste0(projectFolder, "/g_intermediate/",DATE,"_",DAP,"_WHERECLAUSE_",j,".rds"))
    
    #Remove information from this loop so it not interacts with next loops.  
    rm(VALUES, colls)
  }
  
}

#After working with the database, you always need to disconnect. R is giving problems (warnings or you cannot delete the database file) if not doing this. 
dbDisconnect(mydb)

#Remove stored information that is only needed for this script 
rm(mydb, TABLES2)
#Clean the environment to keep working memory cost as low as possible throughout the script. 
gc()