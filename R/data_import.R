#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

#Data Import

#import dataset.csv
dataset <- read_delim("../data/dataset.csv", 
                      delim = "+") %>% 
  mutate(EmployeeID = 1:1470, .before = Age) %>% #add column specifying employee id
  mutate(across(where(is.character), as.factor)) %>% #change all character columns to factor
  mutate(across(c(Education,JobLevel,StockOptionLevel), as.factor)) #change numeric columns of categorical variables to factor

#import satisfaction_review.csv
sat_revies <- read_delim("../data/satisfaction_reviews.csv",
                         col_names = c("PosReview","NegReview","EmployeeID"), #name cols accordingly
                         delim = ".") 

#combine datasets joining by id
full_dat <- left_join(dataset, sat_revies, by="EmployeeID")

#save combined file to RDS
write_rds(full_dat, "../rds/combined_dataset.rds")