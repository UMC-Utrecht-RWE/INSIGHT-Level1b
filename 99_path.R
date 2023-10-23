##Aim
#Set the location of the CDM table csv files, if the StudyName is filled 

##in/output
#Input 1: projectFolder
#Input 2: StudyName
#Output 1: path

#Set the working directory to the location of the program. This serves as the starting point
setwd(projectFolder)

#Shift the working directory up 1 step higher in the folder structure hierarchy. This is the place where the CDMInstaces folder should be located 
setwd('..') 

#Retrieve the new working directory. From this point on the path ot the CDM tables can be calculated.
dir_base<-getwd()

#Create the define variable that is the location of the CDM files and that is used throughout the rest of the program
path_dir<-paste0(dir_base,"/CDMInstances/",StudyName,"/")
path <- path_dir

