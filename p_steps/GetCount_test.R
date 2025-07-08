#This script tests if a CDM instnace can pass the first step of the level1b
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
  
  
  colls <- colnames(dbGetQuery(mydb,
                               paste0("SELECT * FROM ", j, " LIMIT 1")))
  
  # Remove id's and dates using a contains/grepl action
  colls <- colls[!grepl("date", colls)]
  colls <- colls[!(grepl("_id", colls) & !grepl("medicinal_product_id", colls))]
  # In the persons table the dates are stored as integers so they cannot be removed by contains(date)
  colls <- colls[!colls %in% c("day_of_birth", "month_of_birth","year_of_birth",
                               "day_of_death","month_of_death", "year_of_death")]
  
  cat("Total columns to test:", length(colls), "\n")
  cat("Columns:", paste(colls, collapse = ", "), "\n\n")
  
  # Function to test a query and return result or error
  test_query <- function(cols_to_test, table_name) {
    if (length(cols_to_test) == 0) {
      return(list(success = FALSE, error = "No columns provided"))
    }
    
    query <- paste0("SELECT COUNT(*) AS N, ", 
                    paste0(cols_to_test, collapse = ", "), 
                    " FROM ", table_name, 
                    " GROUP BY ", 
                    paste0(cols_to_test, collapse = ", "))
    
    tryCatch({
      result <- dbGetQuery(mydb, query)
      return(list(success = TRUE, result = result, query = query))
    }, error = function(e) {
      return(list(success = FALSE, error = as.character(e), query = query))
    })
  }
  
  # Test 1: Try the original query to confirm the issue
  cat("=== Testing Original Query ===\n")
  original_result <- test_query(colls, j)
  if (original_result$success) {
    cat("✓ Original query succeeded!\n")
    cat("Rows returned:", nrow(original_result$result), "\n\n")
  } else {
    cat("✗ Original query failed as expected\n")
    cat("Error:", original_result$error, "\n")
    cat("Query:", original_result$query, "\n\n")
  }
  
  # Test 2: Test each column individually
  cat("=== Testing Individual Columns ===\n")
  problematic_columns <- c()
  working_columns <- c()
  
  for (col in colls) {
    result <- test_query(col, j)
    if (result$success) {
      cat("✓", col, "- OK\n")
      working_columns <- c(working_columns, col)
    } else {
      cat("✗", col, "- FAILED:", result$error, "\n")
      problematic_columns <- c(problematic_columns, col)
    }
  }
  
  cat("\nSummary of individual column tests:\n")
  cat("Working columns:", length(working_columns), "\n")
  cat("Problematic columns:", length(problematic_columns), "\n")
  if (length(problematic_columns) > 0) {
    cat("Problematic columns list:", paste(problematic_columns, collapse = ", "), "\n")
  }
  
  # Test 3: Test combinations if individual columns work
  if (length(working_columns) > 1 && length(problematic_columns) == 0) {
    cat("\n=== Testing Column Combinations ===\n")
    
    # Binary search approach to find problematic combination
    test_combinations <- function(cols, start_size = 1) {
      max_working_size <- 0
      max_working_cols <- c()
      
      for (size in start_size:length(cols)) {
        cat("Testing combinations of size", size, "...\n")
        
        # Test a few combinations of this size
        if (size <= length(cols)) {
          # Try first 'size' columns
          test_cols <- cols[1:size]
          result <- test_query(test_cols, j)
          
          if (result$success) {
            cat("✓ Combination of", size, "columns works:", paste(test_cols, collapse = ", "), "\n")
            max_working_size <- size
            max_working_cols <- test_cols
          } else {
            cat("✗ Combination of", size, "columns failed\n")
            cat("Error:", result$error, "\n")
            break
          }
        }
      }
      
      return(list(max_size = max_working_size, max_cols = max_working_cols))
    }
    
    combination_result <- test_combinations(working_columns)
    cat("Maximum working combination size:", combination_result$max_size, "\n")
    if (combination_result$max_size > 0) {
      cat("Maximum working combination:", paste(combination_result$max_cols, collapse = ", "), "\n")
    }
  }
  
  # Test 4: Check for specific data type issues
  cat("\n=== Checking Column Data Types ===\n")
  for (col in colls) {
    # Get a sample of values from each column
    sample_query <- paste0("SELECT DISTINCT ", col, " FROM ", j, " LIMIT 10")
    tryCatch({
      sample_data <- dbGetQuery(mydb, sample_query)
      cat(col, "- Sample values:", paste(head(sample_data[[1]], 5), collapse = ", "), "\n")
    }, error = function(e) {
      cat(col, "- Error getting sample:", as.character(e), "\n")
    })
  }
  
  # Test 5: Check for NULL values that might cause issues
  cat("\n=== Checking for NULL Values ===\n")
  for (col in colls) {
    null_check_query <- paste0("SELECT COUNT(*) as null_count FROM ", j, " WHERE ", col, " IS NULL")
    tryCatch({
      null_result <- dbGetQuery(mydb, null_check_query)
      if (null_result$null_count > 0) {
        cat(col, "- Has", null_result$null_count, "NULL values\n")
      }
    }, error = function(e) {
      cat(col, "- Error checking NULLs:", as.character(e), "\n")
    })
  }
  
  # Test 6: Try with LIMIT to see if it's a performance issue
  cat("\n=== Testing with LIMIT ===\n")
  limited_query <- paste0("SELECT COUNT(*) AS N, ", 
                          paste0(colls, collapse = ", "), 
                          " FROM ", j, 
                          " GROUP BY ", 
                          paste0(colls, collapse = ", "),
                          " LIMIT 100")
  
  limited_result <- tryCatch({
    result <- dbGetQuery(mydb, limited_query)
    cat("✓ Limited query succeeded! Rows returned:", nrow(result), "\n")
    TRUE
  }, error = function(e) {
    cat("✗ Limited query also failed:", as.character(e), "\n")
    FALSE
  })
  
  cat("\n=== Diagnostic Complete ===\n")
  if (length(problematic_columns) > 0) {
    cat("Recommendation: Remove these problematic columns:", paste(problematic_columns, collapse = ", "), "\n")
  } else if (!original_result$success && limited_result) {
    cat("Recommendation: The issue might be performance-related. Consider adding LIMIT or filtering the data.\n")
  } else {
    cat("Further investigation needed. Check database logs for more details.\n")
  }
  
}

#After working with the database, you always need to disconnect. R is giving problems (warnings or you cannot delete the database file) if not doing this. 
dbDisconnect(mydb)

#Remove stored information that is only needed for this script 
rm(mydb, TABLES2)
#Clean the environment to keep working memory cost as low as possible throughout the script. 
gc()
      
