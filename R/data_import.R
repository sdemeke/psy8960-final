#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

#Data Import
dataset <- read_delim("../data/dataset.csv", 
                      delim = "+") %>% 
  mutate(employee_id = 1:1470, .before = Age) %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  mutate(across(c(Education,JobLevel,StockOptionLevel), as.factor))
