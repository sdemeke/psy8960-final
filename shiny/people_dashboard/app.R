library(shiny)
library(tidyverse)
library(jtools)

#read in saved skinny data
final_shinydata <- read_rds("./final_shiny_dat.RDS") #change to "./.rds" later

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("People Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        selectInput("outcome","Select Outcome",choices = c("Monthly Pay" = "MonthlyIncome","Turnover Status"="Attrition_num","Overall Satisfaction"="JobSatisfaction"), selected = "Monthly Pay"),
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
        
         mainPanel(
          width = 8,
           plotOutput("univariatePlot"),
           tableOutput("means_sds_bygroup")
        )
    )
)

# Define server logic required to draw a histogram or bar plot depending on outcome
server <- function(input, output) {
  
#output plot
  
  output$univariatePlot <- renderPlot({
    
    filtered_shinydata <- final_shinydata %>% 
      #filter data based on selected gender. Default/'All' choice results in everything selected
      filter(if (input$gender!="All") Gender==input$gender else TRUE) %>%  
      filter(if (input$department!="All") Department==input$department else TRUE) %>%  
      filter(if (input$educ_field!="All") EducationField==input$educ_field else TRUE) %>%  
      filter(if (input$job_role!="All") JobRole==input$job_role else TRUE) 
    
    
    
    if(input$outcome == "MonthlyIncome") {
    filtered_shinydata %>% 
      ggplot(aes(x = MonthlyIncome)) +
      geom_histogram(bins = 30, fill = "grey30") +
      labs( title = "Distribution of Monthly Income",
            x = "Monthly Pay", y = "Frequency"
        
      ) +
      theme_apa()
    } else if (input$outcome == "Attrition_num") {
    filtered_shinydata %>% 
      ggplot(aes(x = Attrition)) +
      geom_bar(width = 0.5, fill = "grey30") +
      labs( title = "Distribution of Turnover Status",
            x = "Turnover Status", y = "Frequency") +
      theme_apa()
        
    } else {
      filtered_shinydata %>% 
        ggplot(aes(x = JobSatisfaction)) +
        geom_bar(width = 0.5, fill = "grey30") +
        labs( title = "Distribution of Overall Job Satisfaction",
              x = "Overall Satisfaction", y = "Frequency") +
        theme_apa()
    }
    
    
    
    
  })
  
  
    
#output table 
    
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




# Run the application 
shinyApp(ui = ui, server = server)
