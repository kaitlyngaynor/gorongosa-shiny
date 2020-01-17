# Gorongosa Camera Trap Data Shiny App

# User interface

source("global.R")


# Sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Overview", tabName = "overview"),
    menuItem("By Species", tabName = "species"),
    menuItem("Pairwise Comparison", tabName = "pairwise_compare")
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
                      choices = sort(unique(records$Species)))
        ),
        
        box(
          title = "Subset records further (optional)",
          
          dateRangeInput(inputId = "date_range1",
                         label = "Date Range:",
                         start = "2016-07-01",
                         end = "2018-08-31"),
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
        
        box(title = "Map of Relative Activity Index (RAI) across camera grid",
            collapsible = TRUE,
            leafletOutput(outputId = "rai_map"),
            "Detections per trap-night at each camera. Note that greyed-out hexagons were operable during the selected period."
            ),
        
        box(title = "Environmental covariates of Relative Activity Index",
            collapsible = TRUE,
            selectInput(inputId = "metadata_select",
                        label = "Choose an environmental covariate:",
                        choices = c("tree.hansen", "termite.large.count.50m", "termite.large.count.100m", "urema.distance", 
                                    "river.distance", "road.major.distance", "boundary.distance", "fire.interval", 
                                    "pans.100m", "pans.250m", "pans.500m", "lion.dry", 
                                    "lion.wet", "termites.100m", "termites.250m", "termites.500m", "termites.1km")),
            
            plotlyOutput(outputId = "rai_metadata"),
            "All covariates have been standardized to have a mean of 0 and standard deviation of 1 in the study area,
            so x-axis units are meaningless.
            Let me know if you are interested in how these data layers were generated."
        )

      ),
            
      fluidRow(
        
        box(title = "Diel activity pattern",
            collapsible = TRUE,
            "Kernel density distribution of the timing of the detections across all cameras across the 24-hour period. All times are scaled to solar time based on the date of the detection.",
            plotOutput(outputId = "activity_plot")
            ),
        
        box(title = "RAI over time",
            collapsible = TRUE,
            plotlyOutput(outputId = "monthly_rai_hist")
        )

      ),
      
      fluidRow(
        
        box(title = "RAI output",
            collapsible = TRUE,
            tableOutput(outputId = "rai_table")
            )#,
        
       # box(title = "Monthly RAI",
       #     collapsible = TRUE,
       #     tableOutput(outputId = "monthly_rai_table")
       #     )
        
      )
        
    ),
  
  # Species comparison ------------------------------------------------------
  
    tabItem(
      
      tabName = "pairwise_compare",
      
      fluidRow(
        box(h2("COMPARISON TOOL"), width = 12,
            "This page enables the comparison of two data subsets. It can be used to compare patterns for a given species across seasons, or compare two species.")
      ),
      
      fluidRow(
        
        box(
          title = "Data Subset A:",
          
          selectInput(inputId = "species_select_A",
                      label = "Choose species for dataset A:",
                      choices = sort(unique(records$Species))),
          
          dateRangeInput(inputId = "date_range_A",
                         label = "Date Range:",
                         start = "2016-07-01",
                         end = "2018-08-31"),

          numericInput(inputId = "independent_min_A",
                       label = "Set quiet period for independent detections (minutes):",
                       value = 15,
                       min = 0,
                       max = 1440)
        ),
        
        box(
          title = "Data Subset B:",
          
          selectInput(inputId = "species_select_A",
                      label = "Choose species for dataset B:",
                      choices = sort(unique(records$Species))),
          
          dateRangeInput(inputId = "date_range_B",
                         label = "Date Range:",
                         start = "2016-07-01",
                         end = "2018-08-31"),
          
          numericInput(inputId = "independent_min_B",
                       label = "Set quiet period for independent detections (minutes):",
                       value = 15,
                       min = 0,
                       max = 1440)
        )
      ),
      
      fluidRow(
        box(width = 12, "Comparison figures.")
      ),
      
      fluidRow(
        
        box(title = "Diel overlap",
            collapsible = TRUE,
            plotOutput(outputId = "activity_plot_compare")),
        
        box(title = "Side-by-side trend over time",
            collapsible = TRUE)
        
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

