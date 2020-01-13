# Gorongosa Camera Trap Data Shiny App

library(tidyverse)
library(shiny)
library(shinythemes)
library(overlap)
library(plotly)
library(here)
library(shinydashboard)
library(leaflet)

setwd(here::here("shiny-rai"))

source("modules/map_card.R")

# source custom functions
files.sources = list.files("functions", full.names = T)
sapply(files.sources, source)

# no scientific notation and round to 2 decimals
options(scipen = 999,
        digits = 2)


# Data import -------------------------------------------------------------

# import shapefile
hexes <- sf::st_read("shapefile", "CameraGridHexes", quiet = T) %>%
  sf::st_transform(crs = '+proj=longlat +datum=WGS84') %>%
  rename(Camera = StudySite) # records and camera_operation use "Camera" not "StudySite" so this allows them to join

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

# define RAI calculation function
rai.calculate <- function(record.table, camop) {
  
  # calculate how long the camera was functioning in that time period
  
  # selects columns within specified dates
  camop.subset <- dplyr::select(camop, Camera, start.date:end.date) 
  
  # sum rows within specified dates (there are 1s when camera was operating, NA when not)
  camop.subset$Operation <- rowSums(dplyr::select(camop.subset, start.date:end.date), na.rm=TRUE) 
  
  # get rid of the individual day columns, just select Camera, Operation
  camop.subset <- dplyr::select(camop.subset, Camera, Operation) 
  
  # calculate number of observations of each classification type at each camera
  records <- record.table.subset %>%
    dplyr::group_by(Classification, Camera) %>%
    dplyr::summarise(Detections = n()) %>%     # counts number of observations of each species
    spread(key = Classification, value = Detections)  # gets from long to wide format  
  
  # add columns for classes not present
  records <- add_column(records, !!!allclassifications[!names(allclassifications) %in% names(records)])
  
  # gather data so each class-camera is its own row again
  records <- records %>% gather(2:ncol(records), key = "Class", value = "Count")
  
  # replace NA with 0 
  records[is.na(records)] <- 0
  
  # join camera operation dates and observations
  RAI.table <- plyr::join(records, camop.subset)
  
  # calculate RAI
  RAI.table$RAI <- RAI.table$Count / RAI.table$Operation
  
  return(RAI.table)
  
}