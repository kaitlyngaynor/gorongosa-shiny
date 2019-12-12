# Gorongosa Camera Trap Data Exploration

# Setup -------------------------------------------------------------------

library(tidyverse)
library(shiny)
library(shinythemes)
library(here)
library(overlap)

setwd(here::here("shiny-rai"))

# import record table
records <- read_csv("recordtable_allrecordscleaned_speciesmetadata.csv")
records$Date <- as.Date(records$Date)

# import camera operation spreadsheet
camera_operation <- read_csv("Camera_operation_years1and2.csv") %>%
 mutate_at(c("Start", "End", "Problem1_from", "Problem1_to",
             "Problem2_from", "Problem2_to", "Problem3_from", "Problem3_to"),
           ~as.Date(., format = "%m/%d/%y"))

# join records and camera operation
records <- left_join(records, camera_operation)

# define timeplot function
timeplot <-function (A, n.grid = 128, kmax = 3, linecol = "#00BFC4",  ...) 
{
  
  bwA <- getBandWidth(A, kmax = kmax)
  
  xsc <- 24/(2 * pi)
  xxRad <- seq(0, 2 * pi, length = n.grid)
  xx <- xxRad * xsc
  densA <- densityFit(A, xxRad, bwA)/xsc
  
  ylim <- c(0, max(densA))
  plot(0, 0, type = "n", ylim = ylim, xlim = range(xx), xlab = "Time", 
       ylab = "Activity", xaxt = "n", ...)
  axis(1, at = c(0, 6, 12, 18, 24), labels = c("Midnight", 
                                               "Sunrise", "Noon", "Sunset", "Midnight"))
  lines(xx, densA, lty = 1, col = linecol, lwd = 2)
  return(invisible(list(x = xx, densityA = densA)))
}


# UI ----------------------------------------------------------------------

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




# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  records_subset <- reactive({
    records %>%
      filter(delta.time.secs == 0 | delta.time.secs >= (60 * input$independent_min),
      Date >= input$date_range1[1], Date <= input$date_range1[2])
  })
  
  # create reactive object species_subset that changes based on species_select widget selection 
  species_subset <- reactive({
    records_subset() %>%
      filter(CommName_Full == input$species_select)
  })

  # summarize species counts across cameras
  species_summary <- reactive({
    species_subset() %>%
      group_by(Camera) %>%
      summarise(count = n())
  })
  
  # render a reactive table that shows a summary
  output$species_table <- renderTable({
    species_summary()
  })
  
  # create reactive object camera_subset that changes based on camera_select widget selection 
  camera_subset <- reactive({
    records_subset() %>%
      filter(Camera == input$camera_select)
  })
  
  # summarize species counts across species
  camera_summary <- reactive({
    camera_subset() %>%
      group_by(CommName_Full) %>%
      summarise(count = n())
  })
  
  # render a reactive table that shows a summary
  output$camera_table <- renderTable({
    camera_summary()
  })
  
  # render a reactive graph with the activity patterns of the selected species
  output$activity_plot <- renderPlot({
    timeplot(species_subset()$Time.Sun)
  })
  
}

shinyApp(ui = ui, server = server)