library(tidyverse)
library(shiny)
library(shinythemes)
library(here)
library(overlap)
library(maptools)
library(lubridate)

# import record table
records <- read_csv(here::here('data', 'raw-data', 'recordtable_year1and2_15min.csv'))

# bring in species traits
#species <- read_csv(here::here('data', '2018spp_kingdon.csv'))

##################################################################################################
#  Format and scale times
##################################################################################################

# set spatial coordinates
coords <- matrix(c(34.50, -18.82), nrow=1)
Coords <- sp::SpatialPoints(coords,
                            proj4string=sp::CRS("+proj=longlat +datum=WGS84"))

# specify date format
records$Date <- as.POSIXct(records$Date, tz = "Africa/Maputo")

# convert time to radians (could be done more efficiently with pipe)
records$Time.Corrected <- hms(records$Time)
records$Time.Decimal <- records$Time.Corrected$hour + records$Time.Corrected$minute/60 + records$Time.Corrected$second/3600
records$Time.Scaled <- records$Time.Decimal / 24
records$Time.Radians <- records$Time.Scaled * 2 * pi
# calculate suntime using function from overlap package, and coordinates and dates as formatted above
records$Time.Sun <- sunTime(records$Time.Radians, records$Date, Coords)

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

#####################

ui <- fluidPage(
  theme = shinytheme("slate"),
  titlePanel("I am adding a title!"),
  sidebarLayout(
    sidebarPanel("put my widgets here",
                 selectInput(inputId = "species_select",
                             label = "Choose a species:",
                             choices = unique(records$Species)
                 ),
                 selectInput(inputId = "camera_select",
                              label = "Choose camera:",
                              choices = unique(records$Camera))
    ),
    mainPanel("put my outputs here",
              p("Activity pattern:"),
              plotOutput(outputId = "activity_plot")
    )
  )
)



### SERVER

server <- function(input, output) {
 
  # create reactive object species_suntime that changes based on species_select widget selection 
  species_suntime <- reactive({
    records %>%
      filter(Species == input$species_select) %>%
      select(Time.Sun)
  })
  
  # render a reactive graph with the activity patterns of the selected species
  output$activity_plot <- renderPlot({
    timeplot(as.numeric(species_suntime))
  })
  
}

shinyApp(ui = ui, server = server)