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
          "This dashboard facilitates exploration of the first two years of camera trap data (mid-2016 through mid-2018) 
          from the systematic grid in Gorongosa National Park, Mozambique. I (Kaitlyn Gaynor) collected 
          and classified these data as part of my PhD research at the University of California - Berkeley.")
      ),
      
      fluidRow(
        box(width = 12,
            title = "Study area in Gorongosa National Park",
            status = "primary",
            div(img(src="https://panoptes-uploads.zooniverse.org/production/project_attached_image/43552c76-4867-4d3f-b1d7-e4d2e263026a.png", 
                    width=600), style = "text-align: center;")
            )
      )
      
    ),

# By species --------------------------------------------------------------
  
    tabItem(
  
      tabName = "species",
      
      fluidRow(
        box(h2("INDIVIDUAL SPECIES PATTERNS"), width = 12)
      ),
      
      fluidRow(
        
        box(
          title = "Choose a species:",
          
          selectInput(inputId = "species_select",
                      label = "",
                      choices = sort(unique(records$CommName_Full)))
        ),
        
        box(
          title = "Subset records further (optional)",
          
          dateRangeInput(inputId = "date_range1",
                         label = "Date Range:",
                         start = "2016-06-23",
                         end = "2018-09-15"),
          "The first camera was set on June 23, 2016, and the last camera was checked on September 15, 2018. No individual camera was operable for this entire period.",
          br(),
          br(),
          
          numericInput(inputId = "independent_min",
                       label = "Set quiet period for independent detections (minutes):",
                       value = 15,
                       min = 0,
                       max = 1440),
          "Records of a given species will only be counted as one detection if they occur within the set quiet period. 
          This setting addresses bias caused by a single animal sitting in front of a camera for a long period of time and repeatedly triggering the camera. 
          The default setting is 15 minutes."
        )

      ),
      
      fluidRow(
        
        box(title = "Diel activity pattern",
            "Kernel density distribution of the timing of the detections across all cameras across the 24-hour period. All times are scaled to solar time based on the date of the detection.",
            plotOutput(outputId = "activity_plot")
            ),
        
        box(title = "RAI vs variable",
            "to go here")
      ),
      
      fluidRow(
        
        box(title = "RAI output",
            tableOutput(outputId = "rai_table")
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
