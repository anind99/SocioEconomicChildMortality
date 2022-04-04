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
library(pdftools)
# Read in the raw data. 
raw_data <- pdf_text("/Users/aauveek/Documents/stats/304/paper4/inputs/data/socioeconomy_mortality.pdf") 
raw_data_list <- strsplit(raw_data, split = "\n")
raw_data_array <- raw_data_list[[1]]

data_rows <- raw_data_array[22:49]

raw_data_frame <- data.frame(data_rows)

write.csv(raw_data_frame, "/Users/aauveek/Documents/stats/304/paper4/inputs/data/raw_data.csv")






         