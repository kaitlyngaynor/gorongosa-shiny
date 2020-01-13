# Gorongosa Camera Trap Data Shiny App

## left off being unable to get tabItems to work, following these instructions but failing for some reason:
# https://rstudio.github.io/shinydashboard/structure.html

# User interface

source("global.R")


# Sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Overview", tabName = "overview"),
    menuItem("By Species", tabName = "species"),
    menuItem("Species Comparison", tabName = "species_compare")
  ),
  
  tags$footer(
    p(
      "Developed by ",
      a(href = 'https://www.kaitlyngaynor.com/', "Kaitlyn Gaynor.")
    ),

    align = "left",
    style = "
            position:absolute;
            bottom:0;
            width:100%;
            height:50px; /* Height of the footer */
            color: white;
            padding: 10px;
            background-color: black;
            z-index: 1000;"
  )
)


# Body --------------------------------------------------------------------

body <- dashboardBody(
  
  tabItems(
    

# Overview ----------------------------------------------------------------

    tabItem(
      
      tabName = "overview",
      
      fluidRow(
        box(width = 12,
          h1("Gorongosa Camera Traps"),
          "A description of the camera trap project will go here")
      ),
      
      fluidRow(
        box(width = 6,
            title = "Map of cameras",
            "put a map here")
      )
      
    ),

# By species --------------------------------------------------------------
  
    tabItem(
  
      tabName = "species",
      
      fluidRow(
        
        box(
          title = "Subset records:",
          
          dateRangeInput(inputId = "date_range1",
                         label = "Date Range:",
                         start = "2016-06-01",
                         end = "2017-09-30"),
          
          selectInput(inputId = "species_select",
                      label = "Choose a species:",
                      choices = unique(records$CommName_Full)),
          
          numericInput(inputId = "independent_min",
                       label = "Set quiet period for independent detections (minutes):",
                       value = 15,
                       min = 0,
                       max = 1440)
        ),
        
        box(
          
          title = "Diel activity pattern",
          
          plotOutput(outputId = "activity_plot")
          
        )
      )
        
    ),
  
  # Species comparison ------------------------------------------------------
  
    tabItem(
      
      tabName = "species_compare",
      
      fluidRow(
        
        box(
          title = "TEST:"
        ),
        
        box(
          title = "TEST"
        )
      )
    )
  )
)


# Dashboard ---------------------------------------------------------------

dashboardPage(
  dashboardHeader(title = "GNP Cameras"),
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


# not working - not reactive yet

#  fluidRow(
#    map_ui(
#      id = "richness_map",
#      sub_title_text = "Subtitle text goes here."
#    )
#  )
