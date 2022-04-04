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

df <- readr::read_csv("/Users/aauveek/Documents/stats/304/paper4/inputs/data/cleaned_data.csv")[-c(1)]

#means
mean_selected <- df[-c(1)]
MeanMortality <- vector(mode = "character", length = ncol(mean_selected))
SocioEconomicGroup <- vector(mode = "character", length = ncol(mean_selected))
count <- 1
for (i in colnames(mean_selected)){
  MeanMortality[count] <- mean(as.numeric(unlist(na.omit(mean_selected[i]))))
  SocioEconomicGroup[count] <- i
  count <- count + 1
}
mean_table <- data.frame(SocioEconomicGroup, MeanMortality)

knitr::kable(mean_table, caption = "Table I: Mean Mortality Rates of Selected Socio-economic Groupings") %>%
  kable_styling(latex_options="scale_down")

# Bar Graphs

selected_df <- select(df, Urban, Rural, InfantAgeGroup)
df3 <- melt(selected_df, id.vars='InfantAgeGroup')
ggplot(df3, aes(x=InfantAgeGroup, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + scale_x_discrete(guide = guide_axis(n.dodge=3)) + ggtitle("Mortality Rates of Infants by Residence Type") + guides(fill=guide_legend(title="Residence")) + ylab("Mortality Rate (Death per 1000)") 
  

selected_df <- select(df, "Primary", "Secondary+", "No Education", "InfantAgeGroup")
df3 <- melt(selected_df, id.vars='InfantAgeGroup')
ggplot(df3, aes(x=InfantAgeGroup, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + scale_x_discrete(guide = guide_axis(n.dodge=3)) + guides(fill=guide_legend(title="Education")) + ylab("Mortality Rate (Death per 1000)") + ggtitle("Mortality Rates of Infants by Education Level")


selected_df <- select(df,  "Mountain" ,"Hill", "Terai", "InfantAgeGroup")
df3 <- melt(selected_df, id.vars='InfantAgeGroup')
ggplot(df3, aes(x=InfantAgeGroup, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + scale_x_discrete(guide = guide_axis(n.dodge=3)) + guides(fill=guide_legend(title="Ecological Region")) + ylab("Mortality Rate (Death per 1000)") + ggtitle("Mortality Rates of Infants by Ecologicial Region")


selected_df <- select(df, "Eastern","Central","Western","Mid-western","Far.western","InfantAgeGroup")
df3 <- melt(selected_df, id.vars='InfantAgeGroup')
ggplot(df3, aes(x=InfantAgeGroup, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') + scale_x_discrete(guide = guide_axis(n.dodge=3)) + guides(fill=guide_legend(title="Geographical Region")) + ylab("Mortality Rate (Death per 1000)") + ggtitle("Mortality Rates of Infants by Geographical Region")



# RegionalData Couple with Group Level Deomographic Information will provide information on socioeconomic impact on mortality rates.


# T tests 

ttest1 <- t.test(as.numeric(df$Urban), as.numeric(df$Rural), alternative='less')

ttest2 <- t.test(as.numeric(df$Mountain), as.numeric(df$Hill), alternative='greater')
ttest3 <- t.test(as.numeric(df$Hill), as.numeric(df$Terai), alternative='less')

ttest4 <- t.test(as.numeric(df$Primary), as.numeric(df$`No Education`), alternative='less')
ttest5 <- t.test(as.numeric(df$`Secondary+`), as.numeric(df$`No Education`), alternative='less')

# Limitation of Data: Not promotional to Specific Socioeconomic Group

         