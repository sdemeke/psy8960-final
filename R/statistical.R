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
h1_test

#Test of H2 - Monthly pay differs by department

#The arguement detailed = T returns more info like sum of squares
h2_test <- full_dat %>% 
  anova_test(dv = MonthlyIncome, wid = EmployeeID, between = Department,
             detailed = T)
h2_test

#Since the anova is significant, a post hoc tukey test is conducted to compare group differences 
h2_post_test <- full_dat %>% 
  tukey_hsd(MonthlyIncome ~ Department)
h2_post_test

#Test of H3 - Tenure can be predicted from relationship satisfaction. This link is moderated by gender.
h3_test <- lm(YearsAtCompany ~ RelationshipSatisfaction*Gender, data = full_dat)
h3_test


### Visualization


#Visualization of H1 

#The code creates, displays, and saves a scatterplot of MonthlyIncome and Performance Rating. Jitter is added to handle overplotting and improve visualization.
#A regression fit line is added using simply formula Performance Rating ~ MonthlyIncome.
#To create a publication-ready plot (i.e., following APA guidelines, this means no grid lines), I use theme_apa() imported from {jtools} package
h1_fig <- (ggplot(data = full_dat,
        aes(x=MonthlyIncome, y=PerformanceRating)) +
    geom_jitter() +
    geom_smooth(method = "lm",formula = 'y ~ x', se = FALSE, color = "grey33") +
    labs(
      title = "Figure 1. Relationship Between Monthly Pay and Performance Ratings",
      x = "Monthly Income",
      y = "Performance Ratings"
    ) +
    theme_apa() + 
    theme(plot.title = element_text(size = 12))  #adjusted title size, too big with default theme_apa() settings
  )
h1_fig
ggsave(h1_fig, filename="../fig/H1.png", units="px", width=1920, height=1080)


#Visualization of H2 

#Thematic elements same as for plot of H1. The code below creates a boxplot of MonthlyIncome values split by departmental level.
h2_fig <- (ggplot(data = full_dat,
        aes(x=Department, y=MonthlyIncome)) +
  geom_boxplot() +
  labs(
    title = "Figure 2. Monthly Pay by Department",
    x = "Department",
    y = "Monthly Pay"
  ) +
  theme_apa() + 
  theme(plot.title = element_text(size = 12))   
)
h2_fig
ggsave(h2_fig, filename="../fig/H2.png", units="px", width=1920, height=1080)



#Visualization of H3 

#The code below is very similar as the scatterplot created for Hypothesis 1 except for the grouping by gender (added differentiation by shape to ease visibility) the call to geom_smooth()
#Given instructions to plot predicted values and the nonlinear model for H3 (not a simple y~x), I use the mapping argument in geom_smooth() to directly specify new y-values.
#The y-values here are the predicted values (using predict() function) of the outcome from the specified model. Fitted lines are diffentiated for gender groups by color.
h3_fig <- (ggplot(data = full_dat,
        aes(x=RelationshipSatisfaction, y=YearsAtCompany,group=Gender, shape = Gender)) +
    geom_jitter() +
    geom_smooth(method = "lm", se = FALSE, aes(y = predict(h3_test, full_dat), color = Gender)) +
    labs(
      title = "Figure 3. Relationship Between Relationship Satisfaction and Tenure by Gender",
      x = "Relationship Satisfaction",
      y = "Tenure (Years at Company)"
    ) +
    theme_apa() + #from {jtools}, removes gridlines, sets font sizes to align with APA
    theme(plot.title = element_text(size = 11))  
)
h3_fig
ggsave(h3_fig, filename="../fig/H3.png", units="px", width=1920, height=1080)

#the mapping argument allows you to directly specify new y-values - such as the output of a custom linear model



### Publication

#Below is a custom function that takes in a numeric value and specifies the publication format for decimal values
#str_replace with regex pattern of starting 0 removes leading zeros. (-?) matches a negative sign but doesn't capture it
#replacement = "\\1" references the first capturing group (leading 0)
#round function rounds decimals to 2 digits and format with nsmall argument makes sure at least 2 digits are shown (e.g., trailing zeroes)
custom_decimal <-  function(x){
  str_replace(format(round(x, digits=2L), nsmall=2L), pattern="^(-?)0", replacement = "\\1")

  }

#H1

#A table is not necessary for a correlation between two variables.

#Publication-ready text interpretation for H1:
#"The correlation between monthly pay and performance rating is r = -.02, p =.51. This test was not statistically significant."
paste0("The correlation between monthly pay and performance rating is r = ",
      custom_decimal(h1_test$cor),
      ", p =",
      custom_decimal(h1_test$p),
      ". This test was",
      ifelse(h1_test$p < .05, " statistically significant.", " not statistically significant.")
      )


#H2

#Publication-ready ANOVA summary table
#The new tibble takes the desired values from h2_test. Mean Squares are computed using df/SS for between and within variables
#F and p values are only computed for the Department component and empty characters are assigned for the Error row
#It is not necessary to round these values so I just use format() to keep values to two decimal digits and use trimws() to remove random white space
h2_aov_tbl <- tibble(
  "Predictor" = c(h2_test$Effect, "Error"),
  "Sum of Squares" = trimws(format(c(h2_test$SSn,h2_test$SSd), nsmall = 2L)),
  "df" = c(h2_test$DFn,h2_test$DFd),
  "Mean Square" = trimws(format(c(h2_test$SSn/h2_test$DFn,h2_test$SSd/h2_test$DFd), nsmall = 2L)),
  "F" = c(custom_decimal(h2_test$`F`), ""),
  "p" = c(custom_decimal(h2_test$p), "")
  
) %>% 
  write_csv("../out/H2.csv")

#To accompany text interpretation of the Tukey results, I created a simple table of the means and SDs of each departmental group for monthly pay.
h2_desc_tbl <- full_dat %>% 
  group_by(Department) %>% 
  summarise("M" = mean(MonthlyIncome, na.rm = T), 
            "SD" = sd(MonthlyIncome, na.rm = T))

#Publication-ready text interpretation for H2:
#"A one-way ANOVA was conducted to compare mean scores of monthly pay by departmental groups. The results in the summary table show that there is a significant difference in monthly income across departments, F(df1 = 2, df2 = 1467) = 3.20, p = .04. A post hoc (Tukey HSD) test revealed a statistically significant difference between the Sales (M = 6959.17, SD = 4058.74) and Research & Development (M = 6281.25, SD = 4895.84) departments. Monthly pay for Sales is significantly higher than for the R&D group (p = .03)."
paste0("A one-way ANOVA was conducted to compare mean scores of monthly pay by departmental groups. The results in the summary table show that there is a significant difference in monthly income across departments, F(",
       "df1 = ",h2_test$DFn, ", df2 = ",h2_test$DFd,") = ", custom_decimal(h2_test$`F`),
       ", p = ", custom_decimal(h2_test$p),
       ". A post hoc (Tukey HSD) test revealed a statistically significant difference between the Sales",
       " (M = ", custom_decimal(h2_desc_tbl$M[3]),", SD = ", custom_decimal(h2_desc_tbl$SD[3]),
       ") and Research & Development (M = ", custom_decimal(h2_desc_tbl$M[2]),", SD = ", custom_decimal(h2_desc_tbl$SD[2]),
       ") departments. Monthly pay for Sales is higher than income in the R&D group (p = ", custom_decimal(h2_post_test$p.adj[3]), ")."
       
  
)


#H3

#converted lm object into tibble to make it easier to extract values for final table
h3_test_summary <- as_tibble(summary(h3_test)$coefficients)

#Publication-ready regression table for H3
#This code takes the values directly from the lm output. I use sapply() to apply my custom decimal value function to each vector
h3_reg_table <- tibble(
  "Effect" = c("(Intercept)","Relationship Satisfaction", "Gender(Male)", "Relationship Satisfaction x Gender(Male)"),
  "Estimate (b)" = sapply(h3_test_summary$Estimate, custom_decimal),
  "t statistic" = sapply(h3_test_summary$`t value`, custom_decimal),
  "p" = sapply(h3_test_summary$`Pr(>|t|)`, custom_decimal)
) %>% 
  write_csv("../out/H3.csv")

#Publication-ready text interpretation for H3:
#"The main effect of relationship satisfaction on tenure is not statistically significant, b = .37 (p = .11).The hypothesis of a moderating effect of gender on this relationship is not supported, b = -.43 (p = .16)."
paste0("The main effect of relationship satisfaction on tenure is",
       ifelse(h3_test_summary$`Pr(>|t|)`[2] < .05, " statistically significant", " not statistically significant"),
       ", b = ",h3_reg_table$`Estimate (b)`[2]," (p = ",h3_reg_table$p[2], ").",
       "The hypothesis of a moderating effect of gender on this relationship is",
       ifelse(h3_test_summary$`Pr(>|t|)`[4] < .05, " supported", " not supported"),
       ", b = ",h3_reg_table$`Estimate (b)`[4]," (p = ",h3_reg_table$p[4], ")."
       
  
)

