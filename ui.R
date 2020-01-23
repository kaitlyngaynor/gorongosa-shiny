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
          and classified these data as part of my PhD research at the University of California - Berkeley,
          supervised by Justin Brashares.")
      ),
      
      fluidRow(
        box(width = 12,
            title = "Camera trap study design",
            status = "primary",
            "Study area in Gorongosa National Park. The camera trap grid is located south of Lake Urema, in savanna woodland. Insets show the location of Gorongosa National Park within Mozambique, and the study area within the park.",
            div(img(src="https://panoptes-uploads.zooniverse.org/production/project_attached_image/43552c76-4867-4d3f-b1d7-e4d2e263026a.png", 
                    width=600), style = "text-align: center;"),
            "To evaluate spatiotemporal patterns of mammal activity in Gorongosa National Park, we conducted a systematic camera trap survey 
            in the woodland south of Lake Urema for three months in the late dry season of 2016 (September – November). We used a grid configuration 
            to place 60 cameras in an area of 300km2 in the woodland. We selected this study region because it contained a high density of mammals 
            and was accessible via the park’s road network. We divided the study area into 5km2 hexagonal grid cells and placed one Bushnell TrophyCam 
            camera at the center point of each grid cell, such that each camera was approximately 2.8 km from its six nearest neighbors. We mounted 
            each camera on a suitable tree within 100 meters of the center point, at a height of 1m, angled slightly downward. To maximize animal 
            detections and minimize false triggers, we faced cameras towards open areas or small game trails showing signs of animal activity. Each 
            camera was within 2 km of a road, but no cameras faced roads."
            )
      ), 
      
      fluidRow(
        box(title = "Funding and collaboration",
            width = 12,
            status = "primary",
            "This work was possible thanks to the permission of the Mozambican government and the support of Gorongosa National Park staff, 
            especially M. Stalmans, J. Denlinger, M. Mutemba, and P. Muagura. Field assistance was provided by C. Lencastro, G. Curtiz, 
            D. Semente, and many rangers. Thanks to the research assistants who classified the species in the camera trap images: T. Gu, 
            A. Wu, E. Lai, A. Ke, M. Levy, C. Jurgensen, and M. Silverberg, and thanks to the citizen scientists of WildCam Gorongosa. 
            J. Daskin and M. Stalmans provided many of the spatial data layers and D. Gonçalves collected the participatory mapping data 
            on hunting. We are grateful to the Brashares Group and Pringle Group for their feedback on this project. Funding for this work 
            came from the National Science Foundation, Schmidt Science Fellows, the Rufford Foundation, the Explorers Club Mamont Scholars 
            Program, Animal Behavior Society, IdeaWild, UC Center for African Studies Rocca Fellowship, UC Institute for International Studies, 
            Sigma Xi Berkeley Chapter, and B. Alireza.",
            div(img(src="http://www.kaitlyngaynor.com/uploads/2/8/0/3/28034347/published/gnp-logo.png?1563238709", width=100),
                img(src="http://www.kaitlyngaynor.com/uploads/2/8/0/3/28034347/hhmi_1_orig.png", width=200),
                img(src="https://panoptes-uploads.zooniverse.org/production/project_attached_image/7df912f7-f9d1-431b-bda4-8759818069c2.png", width=90),
                img(src="https://panoptes-uploads.zooniverse.org/production/project_attached_image/18427632-c14a-4ab8-a03f-4ca6099c2377.jpeg", width=150), 
                img(src="https://panoptes-uploads.zooniverse.org/production/project_attached_image/36f1f0ea-a8b2-413c-a713-0b5cde5bfb1f.png", width=150),
                img(src="https://panoptes-uploads.zooniverse.org/production/project_attached_image/ec339c2a-299a-4cc2-8839-076b1048c55a.png", width=150),
                img(src="https://panoptes-uploads.zooniverse.org/production/project_attached_image/4b33b76b-681f-42cd-a916-c6798f8712ff.png", width=100),
                img(src="https://panoptes-uploads.zooniverse.org/production/project_attached_image/16f7af1b-db79-412a-8803-9427a81b074b.png", width=50),
                img(src="http://www.kaitlyngaynor.com/uploads/2/8/0/3/28034347/published/abs-logo.png?1563238682", width=100),
                img(src="http://www.kaitlyngaynor.com/uploads/2/8/0/3/28034347/editor/sigmaxi_1.jpg?1563238692", width=100),
                img(src="http://www.kaitlyngaynor.com/uploads/2/8/0/3/28034347/published/iis-logo-0.png?1563238751", width=80)
                )
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
          
          dateRangeInput(inputId = "date_range",
                         label = "Date Range:",
                         start = "2016-07-01",
                         end = "2018-08-31"),
          "The first camera was set on June 23, 2016, and the last camera was checked on September 15, 2018. 
          If you choose dates outside of this range, it will generate an error.
          No individual camera was operable for this entire period.",
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
            "Detections per trap-night at each camera. Note that greyed-out hexagons were not operable during the selected period.",
            "Switch to log scale for easier viewing (small value of 0.001 added to all RAI to address issue with 0s)",
            radioButtons(inputId = "log_select_map", label = "",
                         choices = list("RAI" = 1, "log(RAI)" = 2), 
                         selected = 1)
            
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
            "Monthly RAI for the selected time period, calculated for the entire grid network (total detections per total trap-nights across all operating cameras). 
            An RAI of 0 indicates that there were no detections during that month.",
            plotlyOutput(outputId = "monthly_rai_hist")
        )

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
          
          selectInput(inputId = "species_select_B",
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
        
        box(title = "Diel overlap",
            collapsible = TRUE,
            textOutput(outputId = "activity_overlap"),
            plotOutput(outputId = "activity_plot_compare")
            ),
        
        box(title = "Side-by-side trend over time",
            collapsible = TRUE,
            plotlyOutput(outputId = "rai_monthly_AB"))
        
      ),
      
      fluidRow(
        
        box(title = "Plot of RAI A vs B",
            collapsible = TRUE,
            plotlyOutput(outputId = "rai_AB"),
            "Option to switch to log scale for easier viewing (small value of 0.001 added to all RAI to address issue with 0s)",
            radioButtons(inputId = "log_select", label = "",
                         choices = list("RAI" = 1, "log(RAI)" = 2), 
                         selected = 1)
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

