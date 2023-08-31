### Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

### Data Import and Cleaning

#Import dataset.csv with correct delimeter
dataset <- read_delim("../data/dataset.csv", 
                      delim = "+") %>% 
  mutate(EmployeeID = 1:1470, .before = Age) %>% #add column creating sequential employee id
  mutate(across(where(is.character), as.factor)) %>% #change all character columns to factor
  mutate(across(c(Education,JobLevel,StockOptionLevel), as.factor)) #change numeric columns of categorical variables to factor

#Import satisfaction_review.csv
sat_revies <- read_delim("../data/satisfaction_reviews.csv",
                         col_names = c("PosReview","NegReview","EmployeeID"), #name columns accordingly
                         delim = ".") 

#Combine datasets joining by id
full_dat <- left_join(dataset, sat_revies, by="EmployeeID")


#save combined file to my rds folder in project directory
write_rds(full_dat, "../rds/combined_dataset.rds")