### Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)
#install.packages("rstatix")


### Data Import and Cleaning

#Load combined data file from Part 1
full_dat <- read_rds("../rds/combined_dataset.rds")