# Alice Burchett, Kayla Anderson, Alexis Waldschmidt
# R Project
# 12/3/2021


# This script uses functions defined in supportingFunctions.R to summarize 
# disease screening data, determine the country of infection origin, and 
# predict whether a vaccine developed for country Y will work in country X.
# 
# Usage notes: This script, supportingFunctions.R, and any country directories 
# containing screeninf files must be located in the current working directory.
# User must also have installed ggplot2, cowplot, and tidyverse.

# Data must be labelled as screen_DDD.txt or .csv, and located in folders 
# labeled with "country" followed by the country name (eg, countryX).

library(ggplot2)
library(cowplot)
library(reshape2)
library(tidyverse)
source("supportingFunctions.R")


folderNames<-c("countryX", "countryY") # Input the country directories here

# .txt files will be duplicated and converted to .csv
txt2csv("countryY")

# All screening data will be combined into one CSV, noting country day
compileCSV(folderNames)
data<-read.csv("combinedScreeningData.csv")

# Make plots for gender, age, and infection rate over time
summarize(data, folderNames)

# Question 1 Answer:
# From these graphs we can clearly see that countryX has infected patients many days
# before country Y, thus suggesting that the outbreak began in country X.


# Predict whether a vaccine for one country will work for the other
vaccine(data, folderNames)

# Question 2 Answer:
# There is 1.7% of exact similarity between country X's marker combinations on the last day and country Y's.
# Because there is such a low exact similarity between the two countries, it is unlikely
# that country Y's vaccine will work for country X's patients. This conclusion is also visually supported by the graph.
