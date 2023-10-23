#' Aim
#'
#'This function reads the CDM documentation (excel) stored in https://drive.google.com/file/d/1hc-TBOfEzRBthGP78ZWIa13C0RdhU7bK/view and retrieves needed information.
#'Note that only rows that are in a sheet where the first column contains the words Variable and Conventions are taken into account. This words refer to the start and 
#'end row of the needed information
#'
#' @section ??
#'
#' @author
#' Roel Elbers
#'
#' @docType package
#' @name GetColumnNamesCDM
#' @keywords ??
#' @import data.table openxlsx

NULL


#' @param cdm_path required | string | location of a copy of the excel file.


#' @return A data.table data frame with per table all the recognized column names
#' @export


GetColumnNamesCDM <- function(cdm_path){

#Make an empty date.table object. This is the starting table where the information per excel sheet is appended to  
TABLES <- data.table()

#Load the package that is needed for function that are needed if working with an excel file.
if(!require("openxlsx")){install.packages("openxlsx")}
suppressPackageStartupMessages(library(openxlsx))

#Get the sheets that are in the excel file. This sheets are the input vector for the for loop that goes over every sheet in the excel file
sheets <- getSheetNames(cdm_path)

for(i in sheets){
          
          #Read the sheet and store temporary in a date frame object. 
          TEMP <- as.data.table(read.xlsx(cdm_path, sheet = i))
          
          ###
          #The imported excel sheets are a bit unstructured is terms of column structure. Not all sheets are needed. Also not all rows are needed.
          #To extract the needed rows the first column is used.
          #Define the start row: It is assumed that the starting point is where Variable is filled in the cell 
          start <- which(TEMP[,1] == "Variable")
          
          #Define the end row: It is assumed that the end point is where Conventions is filled in the cell 
          end <- which(TEMP[,1] == "Conventions")
          ###
          
          #The information is only extracted if 1 valid start and end row are found. 
          if(length(start) == 1 & length(end) == 1){
          
          #Based on the start and end row that are recognized according to the assumptions, the needed rows are extracted to a new file TEMP2  
          TEMP2 <- as.data.frame(TEMP[(start +1) : (end - 1) ,])
          
          #The needed column names are stored on the start row. So get that row and make use them to overwrite the column names
          colnames(TEMP2) <- unlist(unname(as.list(TEMP[start,])))
          
          #Only keep the needed columns
          #TODO, now it is selecting 1:4, rather use the actual column names. This means that also to excel file needs to be perfectly standardized based on the assumptions
          TEMP2 <- TEMP2[, 1:4]
          
          #The table name is similar to the sheet name so ad this as a column. Eventual, a long file is created containing all information instead of in several tables. 
          TEMP2$TABLE <- i
          
          #Append to the main table to make 1 file in long format
          if(nrow(TABLES) == 0) TABLES <- TEMP2
          if(nrow(TABLES) > 0) TABLES <- rbindlist(list(TABLES,TEMP2), fill = T, use.names = T)
          
          #remove the files that where only needed in this loop. If an intermdeiate file is not generated then the ol d file is not used and a error will be thrown.
          rm(TEMP2, start, end)

          }
          rm(TEMP)
}

#Clean the file for empties. 
TABLES <- TABLES[!is.na(Variable) & !is.na(TABLE),]

return(TABLES)


}