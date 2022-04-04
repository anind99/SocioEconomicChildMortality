#### Preamble ####
# Purpose: Clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander [CHANGE THIS TO YOUR NAME!!!!]
# Data: 3 January 2021
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!
# - Change these to yours
# Any other information needed?


#### Workspace setup ####
# Use R Projects, not setwd().
library(haven)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(kableExtra)
# Read in the raw data. 
raw_data_frame <- readr::read_csv("/Users/aauveek/Documents/stats/304/SocioEconomicChildMortality/inputs/data/raw_data.csv")["data_rows"]

## Reading as rows instead of columns
# df <- data.frame(matrix(ncol = 6, nrow = 0))
# colnames(df) <- c("Grouping", "NeoNatalMortality", "PostNatalMortality", "InfantMortality", "ChildMortality", "UnderFiveMortality")
# 
# tsplit <- strsplit(raw_data_frame[2,1][1,1]$data_rows, "\\s{1,}")[[1]]
# 
# 
# 
# counter <- 1
# for (i in 2:25){
#   tsplit <- strsplit(raw_data_frame[i,1][1,1]$data_rows, "\\s{1,}")[[1]]
#   if (length(tsplit) == 6){
#     df[counter,] <- tsplit
#     counter <- counter + 1
#   }
# }




df <- data.frame(matrix(ncol = 0, nrow = 5))
df["InfantAgeGroup"] <- c("NeoNatalMortality", "PostNatalMortality", "InfantMortality", "ChildMortality", "UnderFiveMortality")

counter <- 1
for (i in 2:25){
  tsplit <- strsplit(raw_data_frame[i,1][1,1]$data_rows, "\\s{1,}")[[1]]
  if (length(tsplit) == 6){
    df[tsplit[1]] <- tsplit[2:6]
    counter <- counter + 1
  }
}

tsplit <- strsplit(raw_data_frame[17,1][1,1]$data_rows, "\\s{1,}")[[1]]
df["No Education"] <- tsplit[3:7]

tsplit <- strsplit(raw_data_frame[21,1][1,1]$data_rows, "\\s{1,}")[[1]]
df["No antenatal or delivery care"] <- tsplit[6:10]

tsplit <- strsplit(raw_data_frame[23,1][1,1]$data_rows, "\\s{1,}")[[1]]
df["Either antenatal or delivery care"] <- tsplit[3:7]

tsplit <- strsplit(raw_data_frame[25,1][1,1]$data_rows, "\\s{1,}")[[1]]
df["Both antenatal and delivery care"] <- tsplit[3:7]


write.csv(df, "/Users/aauveek/Documents/stats/304/SocioEconomicChildMortality/inputs/data/cleaned_data.csv")



# Limitation of Data: Not stratified to Specific Socioeconomic Group

         