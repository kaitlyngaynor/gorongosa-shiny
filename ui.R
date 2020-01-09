# Gorongosa Camera Trap Data Shiny App

## left off being unable to get tabItems to work, following these instructions but failing for some reason:
# https://rstudio.github.io/shinydashboard/structure.html

# User interface

source("global.R")


# Sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  
  numericInput(inputId = "independent_min",
               label = "Set quiet period for independent detections (minutes):",
               value = 15,
               min = 0,
               max = 1440)
  
)


# Body --------------------------------------------------------------------

body <- dashboardBody(
  
  fluidRow(
    
    box(
      title = "Subset records:",
      
      dateRangeInput(inputId = "date_range1",
                     label = "Date Range:",
                     start = "2016-06-01",
                     end = "2017-09-30"),
      
      selectInput(inputId = "species_select",
                  label = "Choose a species:",
                  choices = unique(records$CommName_Full))
    ),
    
    box(
      
      title = "Diel activity pattern",
      
      plotOutput(outputId = "activity_plot")
      
    )
  )
)


# Dashboard ---------------------------------------------------------------

dashboardPage(
  dashboardHeader(title = "Gorongosa cameras"),
  sidebar,
  body
)



#    tabPanel("By Camera",
#             sidebarLayout(
#               sidebarPanel("",
#                            selectInput(inputId = "camera_select",
#                                        label = "Choose a camera:",
#                                        choices = unique(records$Camera))
#               ),
#               
#               mainPanel("",
#                         p(""),
#                         tableOutput(outputId = "camera_table")
#               )
#             )
#    )
