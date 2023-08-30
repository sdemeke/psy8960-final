### Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)
#install.packages("rstatix")


### Data Import and Cleaning

#Load combined data file from Part 1
full_dat <- read_rds("../rds/combined_dataset.rds")

### Visualization

#make this more apa

#H1 - Relationship between monthly pay and performance rating
full_dat %>% ggplot(aes(x=MonthlyIncome, y=PerformanceRating)) +
 # geom_point(position = "jitter", alpha = 0.7)
  geom_point(position = position_jitter(width = 1,seed = 24)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
  labs(
    title = "Figure 1. Relationship Between Monthly Pay and Performance Ratings",
    x = "Monthly Income",
    y = "Performance Ratings"
  ) +
  theme_minimal() 


#H2 - Monthly pay differs by department
full_dat %>% ggplot(aes(x=Department, y=MonthlyIncome)) +
  geom_boxplot()

#add titles, anything else?


### Analysis

#H1 - Relationship between monthly pay and performance rating
full_dat %>% 
  cor_test(vars = c(MonthlyIncome, PerformanceRating), alternative = "two.sided")



#H2 - Monthly pay differs by department
h2_test <- full_dat %>% 
  anova_test(dv = MonthlyIncome, wid = EmployeeID, between = Department,
             detailed = T)




#H3 - Tenure can be predicted from relationship satisfaction. This link is moderated by gender.


