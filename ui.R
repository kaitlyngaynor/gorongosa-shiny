# Gorongosa Camera Trap Data Shiny App

# User interface

ui <- fluidPage(
  theme = shinytheme("slate"),
  navbarPage("Gorongosa Camera Traps",
             
             tabPanel("By Species",
                      sidebarLayout(
                        sidebarPanel("",
                                     numericInput(inputId = "independent_min",
                                                  label = "Set quiet period for independent detections (minutes):",
                                                  value = 15,
                                                  min = 0,
                                                  max = 1440),
                                     
                                     dateRangeInput(inputId = "date_range1",
                                                    label = "Date Range:",
                                                    start = "2016-06-01",
                                                    end = "2017-09-30"),
                                     
                                     selectInput(inputId = "species_select",
                                                 label = "Choose a species:",
                                                 choices = unique(records$CommName_Full))
                        ),
                        
                        mainPanel("",
                                  #p(""),
                                  #tableOutput(outputId = "species_table"),
                                  p("Diel activity pattern:"),
                                  plotOutput(outputId = "activity_plot")
                        )
                      )
             ),
             tabPanel("By Camera",
                      sidebarLayout(
                        sidebarPanel("",
                                     selectInput(inputId = "camera_select",
                                                 label = "Choose a camera:",
                                                 choices = unique(records$Camera))
                        ),
                        
                        mainPanel("",
                                  p(""),
                                  tableOutput(outputId = "camera_table")
                        )
                      )
             )
  )
)
