#Load demanded packages
if(!require(data.table)){install.packages("data.table")}
suppressPackageStartupMessages(library(data.table))

if(!require("rlist")){install.packages("rlist")}
suppressPackageStartupMessages(library("rlist"))

if(!require("stringr")){install.packages("stringr")}
suppressPackageStartupMessages(library(stringr))

if(!require("dplyr")){install.packages("dplyr")}
suppressPackageStartupMessages(library(dplyr))

if(!require("readr")){install.packages("readr")}
suppressPackageStartupMessages(library(readr))

if(!require("openxlsx")){install.packages("openxlsx")}
suppressPackageStartupMessages(library(openxlsx))