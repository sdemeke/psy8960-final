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

#read in saved skinny data
#final_shinydata <- read_rds("../shiny/people_dashboard/final_shiny_dat.RDS") #change to "./.rds" later

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("People Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
        
        #first selection input takes in user choice for outcome Default monthly pay selected
        selectInput("outcome","Select Outcome",choices = c("Monthly Pay","Turnover Status","Overall Satisfaction"), selected = "Monthly Pay"),
        #first selection input takes in user choice for grouping variable. Default all cases shown (no group)
        #selectInput("group","Select Group",choices = c("Gender","Department","Education Field","Job Role","All"), selected = "All"),
        
        checkboxGroupInput("group", "Select Group(s) to show:",
                           c("Gender" = "Gender",
                             "Department" = "Department",
                             "Educational Field" = "Education Field",
                             "Job Role" = "Job Role",
                             "All")),
       
        
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
           # ,
           # tableOutput("data")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    
    #output plot
    
    #output table - means and sds of selected outcome by group
    
    
}



## Only run examples in interactive R sessions
# if (interactive()) {
#   
#   ui <- fluidPage(
#     checkboxGroupInput("variable", "Variables to show:",
#                        c("Cylinders" = "cyl",
#                          "Transmission" = "am",
#                          "Gears" = "gear")),
#     tableOutput("data")
#   )
#   
#   server <- function(input, output, session) {
#     output$data <- renderTable({
#       mtcars[, c("mpg", input$variable), drop = FALSE]
#     }, rownames = TRUE)
#   }
#   
#   shinyApp(ui, server)
#   
#   ui <- fluidPage(
#     checkboxGroupInput("icons", "Choose icons:",
#                        choiceNames =
#                          list(icon("calendar"), icon("bed"),
#                               icon("cog"), icon("bug")),
#                        choiceValues =
#                          list("calendar", "bed", "cog", "bug")
#     ),
#     textOutput("txt")
#   )
#   
#   server <- function(input, output, session) {
#     output$txt <- renderText({
#       icons <- paste(input$icons, collapse = ", ")
#       paste("You chose", icons)
#     })
#   }
#   
#   shinyApp(ui, server)
# }

# Run the application 
shinyApp(ui = ui, server = server)
