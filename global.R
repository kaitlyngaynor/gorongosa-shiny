# Gorongosa Camera Trap Data Shiny App

library(tidyverse)
library(shiny)
library(shinythemes)
library(overlap)
library(plotly)
library(here)
library(shinydashboard)
library(leaflet)
library(camtrapR)

setwd(here::here("shiny-rai"))

source("modules/map_card.R")

# source custom functions
files.sources = list.files("functions", full.names = T)
sapply(files.sources, source)

# no scientific notation and round to 2 decimals
options(scipen = 999) #, digits = 2)


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


# Data manipulation -------------------------------------------------------

# join records and camera operation
records <- left_join(records, camera_operation)

# generate a camera operation matrix
camera_operation_matrix <- cameraOperation(CTtable = camera_operation,        ## data frame with metadata
                                           stationCol = "Camera",
                                           setupCol = "Start",       ## name of column in metadata with setup date
                                           retrievalCol = "End",     ## name of column in metadata with end date
                                           hasProblems = TRUE,
                                           writecsv = FALSE) %>% 
                            as_tibble(rownames = "Camera")
          
# Define timeplot function ------------------------------------------------

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


# Define RAI function -----------------------------------------------------

# Define unique classification categories. We will need this later to ensure these columns are present in all tables
# There is probably a better way to do this that is not so hard-coded and uses unique(records$Species) but I can't figure it out
allclassifications <- c(Aardvark = NA_real_, Baboon = NA_real_, Buffalo = NA_real_, Bushbaby = NA_real_, 
                        Bushbuck = NA_real_, Bushpig = NA_real_, Civet = NA_real_, Duiker_common = NA_real_, 
                        Duiker_red = NA_real_, Eland = NA_real_, Elephant = NA_real_, Genet = NA_real_, 
                        Hare = NA_real_, Hartebeest = NA_real_, Hippo = NA_real_, Honey_badger = NA_real_, 
                        Impala = NA_real_, Kudu = NA_real_, Lion = NA_real_, Mongoose_banded = NA_real_, 
                        Mongoose_bushy_tailed = NA_real_, Mongoose_dwarf = NA_real_, Mongoose_large_grey = NA_real_,
                        Mongoose_marsh = NA_real_, Mongoose_slender = NA_real_, Mongoose_white_tailed = NA_real_,
                        Nyala = NA_real_, Oribi = NA_real_, Pangolin = NA_real_, Porcupine = NA_real_, 
                        Reedbuck = NA_real_, Sable_antelope = NA_real_, Samango = NA_real_, Serval = NA_real_, 
                        Vervet = NA_real_, Warthog = NA_real_, Waterbuck = NA_real_, Wildebeest = NA_real_)  

# define RAI calculation function - using record table that has ALREADY BEEN SUBSET

rai.calculate <- function(record.table.subset, camop, start.date, end.date) {
  
  # calculate how long the camera was functioning in that time period
  
    # change start and end date to character
    start.date <- as.character(start.date)
    end.date <- as.character(end.date)
  
    # selects columns within specified dates
    camop.subset <- dplyr::select(camop, Camera, start.date:end.date) 
    
    # sum rows within specified dates (there are 1s when camera was operating, NA when not)
    camop.subset$Operation <- rowSums(dplyr::select(camop.subset, start.date:end.date), na.rm=TRUE) 
    
    # get rid of the individual day columns, just select Camera, Operation
    camop.subset <- dplyr::select(camop.subset, Camera, Operation) 
    
  # calculate number of observations of each classification type at each camera
  record_count <- record.table.subset %>%
    dplyr::group_by(Species, Camera) %>%
    dplyr::summarise(Detections = n()) %>%     # counts number of observations of each species
    spread(key = Species, value = Detections)  # gets from long to wide format  
  
  # add columns for classes not present
  record_count <- add_column(record_count, !!!allclassifications[!names(allclassifications) %in% names(record_count)])
  
  # gather data so each class-camera is its own row again
  record_count <- record_count %>% gather(2:ncol(record_count), key = "Species", value = "Count")
  
  # replace NA with 0 
  record_count[is.na(record_count)] <- 0
  
  # join camera operation dates and observations
  RAI.table <- plyr::join(record_count, camop.subset)
  
  # calculate RAI
  RAI.table$RAI <- RAI.table$Count / RAI.table$Operation
  
  return(RAI.table)
  
}