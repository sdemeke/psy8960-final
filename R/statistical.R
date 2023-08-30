### Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)
library(jtools)

### Data Import and Cleaning

#Load combined data file from Part 1
full_dat <- read_rds("../rds/combined_dataset.rds")

### Analysis

#Test of H1 - Relationship between monthly pay and performance rating
h1_test <- full_dat %>% 
  cor_test(vars = c(MonthlyIncome, PerformanceRating), alternative = "two.sided")



#Test of H2 - Monthly pay differs by department
#detailed = T returns more info like SS
h2_test <- full_dat %>% 
  anova_test(dv = MonthlyIncome, wid = EmployeeID, between = Department,
             detailed = T)
#anova is significant. post hoc tukey test to compare group differences 
h2_post_test <- full_dat %>% 
  tukey_hsd(MonthlyIncome ~ Department)


#Test of H3 - Tenure can be predicted from relationship satisfaction. This link is moderated by gender.
h3_test <- lm(YearsAtCompany ~ RelationshipSatisfaction*Gender, data = full_dat)

#use tidy() and glance() to make pretty


### Visualization


#Visualization of H1 
(ggplot(data = full_dat,
        aes(x=MonthlyIncome, y=PerformanceRating)) +
    geom_jitter() +
    geom_smooth(method = "lm",formula = 'y ~ x', se = FALSE, color = "grey33") +
    labs(
      title = "Figure 1. Relationship Between Monthly Pay and Performance Ratings",
      x = "Monthly Income",
      y = "Performance Ratings"
    ) +
    theme_apa() + #from {jtools}, removes gridlines, sets font sizes to align with APA
    theme(plot.title = element_text(size = 12))  #title was too far to the right, cut off 
  ) %>%  
  ggsave(filename="../fig/H1.png", units="px", width=1920, height=1080)


#Visualization of H2 - Monthly pay differs by department
(ggplot(data = full_dat,
        aes(x=Department, y=MonthlyIncome)) +
  geom_boxplot() +
  labs(
    title = "Figure 2. Monthly Pay by Department",
    x = "Department",
    y = "Monthly Pay"
  ) +
  theme_apa() + #from {jtools}, removes gridlines, sets font sizes to align with APA
  theme(plot.title = element_text(size = 12))  #title was too far to the right, cut off 
) %>%  
  ggsave(filename="../fig/H2.png", units="px", width=1920, height=1080)



#H3 - Tenure can be predicted from relationship satisfaction. This link is moderated by gender.

full_dat %>% ggplot(aes(x=RelationshipSatisfaction, y=YearsAtCompany)) +
  # geom_point(position = "jitter", alpha = 0.7)
  geom_point(position = position_jitter(width = .8, height= 1,seed = 24)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey")


### Publication

#custom function that takes in a numeric value and specifies the publication format
#str_replace with regex pattern of starting 0 removes leading zeros. (-?) matches a negative sign but doesn't capture it
  #replacement "\\1" references the first capturing group (leading 0)
#round function rounds decimals to 2 digits and format with nsmall argument makes sure at least 2 digits are shown (e.g., trailing zeroes)
custom_decimal <-  function(x){
  str_replace(format(round(x, digits=2L), nsmall=2L), pattern="^(-?)0", replacement = "\\1")

  }

#H1
#"The correlation between monthly pay and performance rating is r = -.50, p =.51. This test was not statistically significant."

paste0("The correlation between monthly pay and performance rating is r = ",
      custom_decimal(-0.5),
      ", p =",
      custom_decimal(h1_test$p),
      ". This test was",
      ifelse(h1_test$p < .05, " statistically significant.", " not statistically significant.")
      )


#H2

#ANOVA summary table
h2_aov_tbl <- tibble(
  "Predictor" = c(h2_test$Effect, "Error"),
  "Sum of Squares" = c(h2_test$SSn,h2_test$SSd),
  "df" = c(h2_test$DFn,h2_test$DFd),
  "Mean Square" = c(h2_test$SSn/h2_test$DFn,h2_test$SSd/h2_test$DFd),
  "F" = c(custom_decimal(h2_test$`F`), " "),
  "p" = c(custom_decimal(h2_test$p), " ")
  
) %>% 
  write_csv("../out/H2.csv")

#table of means and sds by department
h2_desc_tbl <- full_dat %>% 
  group_by(Department) %>% 
  summarise("M" = mean(MonthlyIncome, na.rm = T), 
            "SD" = sd(MonthlyIncome, na.rm = T))

#text interpretation
paste0("A one-way ANOVA was conducted to compare mean scores of monthly pay by departmental groups. The results in the summary table show that there is a significant difference in monthly income across departments, F(",
       "df1 = ",h2_test$DFn, ", df2 = ",h2_test$DFd,") = ", custom_decimal(h2_test$`F`),
       ", p = ", custom_decimal(h2_test$p),
       ". A post hoc (Tukey HSD) test revealed a statistically significant difference between the Sales",
       " (M = ", custom_decimal(h2_desc_tbl$M[3]),", SD = ", custom_decimal(h2_desc_tbl$SD[3]),
       ") and Research & Development (M = ", custom_decimal(h2_desc_tbl$M[2]),", SD = ", custom_decimal(h2_desc_tbl$SD[2]),
       ") departments. Monthly pay for Sales is higher than income in the R&D group (p = ", custom_decimal(h2_post_test$p.adj[3]), ")."
       
  
)
