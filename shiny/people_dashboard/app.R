#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(jtools)

#read in saved skinny data
#final_shinydata <- read_rds("../shiny/people_dashboard/final_shiny_dat.RDS") #change to "./.rds" later

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("People Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        #first selection input takes in user choice for outcome Default monthly pay selected
        selectInput("outcome","Select Outcome",choices = c("Monthly Pay" = "MonthlyIncome","Turnover Status"="Attrition_num","Overall Satisfaction"="JobSatisfaction"), selected = "Monthly Pay"),
        #first selection input takes in user choice for grouping variable. Default all cases shown (no group)
        #selectInput("group","Select Group",choices = c("Gender","Department","Education Field","Job Role","All"), selected = "All"),
        
        # checkboxGroupInput("group", "Select Group(s) to show in summary table:",
        #                    c("Gender" = "Gender",
        #                      "Department" = "Department",
        #                      "Educational Field" = "EducationField",
        #                      "Job Role" = "JobRole",
        #                      "All"
        #                     ),
        #                    selected = "Department"),
        
        selectInput("gender", "Select gender groups to plot",
                    choices = c("Female","Male", "All"),
                    selected = "All"),
        selectInput("department", "Which departments do you want to see in plot?",
                    choices = c("Human Resources","Research & Development", "Sales","All"),
                    selected = "All"),
        selectInput("educ_field", "Which educational fields do you want to see in plot?",
                    choices = c("Human Resources","Life Sciences", "Marketing","Medical","Other","Technical Degree","All"),
                    selected = "All"),
        selectInput("job_role", "Which job roles do you want to see in plot?",
                    choices = c("Healthcare Representative","Human Resources", "Laboratory Technician","Manager",
                                "Manufacturing Director" ,"Research Director" ,"Research Scientist" ,"Sales Executive" ,
                                "Sales Representative", "All"),
                    selected = "All"),
        
        
        
        

        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          width = 8,
           plotOutput("univariatePlot"),
           tableOutput("means_sds_bygroup")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
    
    #output plot
  
  output$univariatePlot <- renderPlot({
    
    if(input$outcome == "MonthlyIncome") {
    final_shinydata %>% 
      #filter data based on selected gender. Default/'All' choice results in everything selected
      filter(if (input$gender!="All") Gender==input$gender else TRUE) %>%  
      filter(if (input$department!="All") Department==input$department else TRUE) %>%  
      filter(if (input$educ_field!="All") EducationField==input$educ_field else TRUE) %>%  
      filter(if (input$job_role!="All") JobRole==input$job_role else TRUE) %>%  
      ggplot(aes(x = .data[[input$outcome]])) +
      geom_histogram(bins = 30, fill = "grey30") +
      labs( title = "Distribution of Monthly",
            x = "Monthly Pay",
            
        
      ) +
      theme_minimal()
    }
    
    
  })
  
  
    
    #output table - means and sds of selected outcome by group
    
    output$means_sds_bygroup <- renderTable({
      
 
      all_possible_cols <- c("Gender","Department","EducationField","JobRole")
      selected_cols <- vector(mode="list",length = length(all_possible_cols))  
      names(selected_cols) <- all_possible_cols
      
      selected_cols[["Gender"]] <- ifelse(input$gender %in% levels(final_shinydata$Gender), TRUE,FALSE)
      selected_cols[["Department"]] <- ifelse(input$department %in% levels(final_shinydata$Department), TRUE,FALSE)
      selected_cols[["EducationField"]] <- ifelse(input$educ_field %in% levels(final_shinydata$EducationField), TRUE,FALSE)
      selected_cols[["JobRole"]] <- ifelse(input$job_role %in% levels(final_shinydata$JobRole), TRUE,FALSE)
      
      
      cols_to_group <- all_possible_cols[unlist(selected_cols)]

      if(length(cols_to_group) >= 1){
      final_shinydata %>%
        group_by(across(all_of(cols_to_group))) %>%
        summarise("Mean" = mean(.data[[input$outcome]], na.rm = T),
                  "SD" = sd(.data[[input$outcome]], na.rm = T)
                  )
      } else {
      final_shinydata %>%
          summarise("Mean" = mean(.data[[input$outcome]], na.rm = T),
                    "SD" = sd(.data[[input$outcome]], na.rm = T)
          )

      }
      


    })
    
    
    
    
}

final_shinydata %>% 
  filter(Department == "Human Resources") %>% 
  ggplot(aes(x = MonthlyIncome)) +
  geom_histogram(bins = 30, fill = "grey30") +
  theme_minimal()



# Run the application 
shinyApp(ui = ui, server = server)
